module Language.CADT.Compile where

import Language.CADT.Parse hiding (braces)
import Data.List
import Text.PrettyPrint.Leijen
import Text.ParserCombinators.Parsec


instance Pretty Field where
  pretty (Field n t) = text t <+> text n <> semi
  
instance Pretty Constructor where
  pretty (Constructor n fs) = 
    text "struct" <+> bracesBlock fs <+> text ("" ++ n) <> semi

instance Pretty DataDecl where
  pretty d@(DataDecl n cs) = def <$> vsep (constructorImpls d) <$> ctrs
    where def = text "struct" <+> text n <+> bracesBlock [e,u] <> semi
          e = text "enum" <+> bracesList tags <+> text "tag" <> semi
          tags = (text . ctorName) `fmap` cs
          u = text "union" <+> bracesBlock cs <> semi
          
          ctrs = text "const struct" 
                 <+> bracesBlock (constructorSigs d)
                 <+> text n <+> equals
                 <+> commaBraces (constructorBindings d)
                 <> semi
                 
ctrFunctionName :: DataDecl -> Constructor -> Doc
ctrFunctionName (DataDecl dn _) (Constructor cn _) = text $ dn ++ "__" ++ cn

constructorSigs :: DataDecl -> [Doc]
constructorSigs (DataDecl dn cs) = getSig `fmap` cs
  where getSig c@(Constructor cn fs) = 
          text "struct" <+> text dn <+> (parens $ text "*" <> text cn)
          <> constructorParams c
          <> semi

constructorBindings :: DataDecl -> [Doc]
constructorBindings d@(DataDecl dn cs) = getBinding `fmap` cs
  where getBinding c@(Constructor cn fs) = 
          dot <> text cn <+> equals
          <+> text "&" <> ctrFunctionName d c

constructorImpls :: DataDecl -> [Doc]
constructorImpls d@(DataDecl dn cs) = getImpl `fmap` cs
  where getImpl c@(Constructor cn fs) = 
          text "inline struct" <+> text dn <+> ctrFunctionName d c
          <> constructorParams c
          <+> bracesBlock [body]
          where varName = text "x"
                body = text "struct" <+> text dn <+> varName
                       <+> equals <+> braces (text cn) <> semi
                       <$> vsep (fieldAssign `fmap` fs)
                       <$> text "return" <+> varName <> semi
                fieldAssign (Field n t) = 
                  varName <> dot <> text ("" ++ cn) <> dot <> text n
                  <+> equals <+> text n <> semi


constructorParams :: Constructor -> Doc
constructorParams (Constructor _ fs) = tupled $ pretty' `fmap` fs
  where pretty' (Field n t) = text t <+> text n

bracesList = encloseSep lbrace rbrace comma

bracesBlock :: Pretty a => [a] -> Doc
bracesBlock = delimBracesBlock empty
commaBraces = delimBracesBlock comma

delimBracesBlock :: Pretty a => Doc -> [a] -> Doc
delimBracesBlock d xs = lbrace <$> body <$> rbrace
  where body = indent 2 $ vsep $ items
        items = punctuate d $ pretty `fmap` xs