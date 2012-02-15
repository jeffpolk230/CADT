module Language.CADT.Compile where

import Language.CADT.Parse hiding (braces)
import Data.List
import Text.PrettyPrint.Leijen
import Text.ParserCombinators.Parsec

test = putStrLn . extract $ parse parseDataDecl "" code
  where code = "data Expr = Plus { l :: int, r :: int} | Times { l :: int, r :: int } | Inc { o :: int} | Dec {o :: int} "
        extract (Right v) = show $ pretty v
        extract (Left e) = show e

instance Pretty Field where
  pretty (Field n t) = text t <+> text n <> semi
  
instance Pretty Constructor where
  pretty (Constructor n fs) = 
    text "struct" <+> bracesBlock fs <+> text n <> semi

instance Pretty DataDecl where
  pretty d@(DataDecl n cs) = def <$> ctrs
    where def = text "struct" <+> text n <+> bracesBlock [e,u] <> semi
          e = text "enum" <+> bracesList tags <+> text "tag" <> semi
          tags = (text . ctorName) `fmap` cs
          u = text "union" <+> bracesBlock cs <> semi
          
          ctrs = text "static const struct" 
                 <+> bracesBlock (constructorSigs d)
                 <+> text n <+> equals
                 <+> commaBraces (constructorImpls d)
                 <> semi

constructorSigs :: DataDecl -> [Doc]
constructorSigs (DataDecl dn cs) = getSig `fmap` cs
  where getSig c@(Constructor cn fs) = 
          text "struct" <+> text dn <+> (parens $ text "^" <> text cn)
          <> constructorParams c
          <> semi

constructorImpls :: DataDecl -> [Doc]
constructorImpls (DataDecl dn cs) = getImpl `fmap` cs
  where getImpl c@(Constructor cn fs) = 
          dot <> text cn <+> equals <+> text "^" <> constructorParams c
          <> bracesBlock [body]
          where varName = text "x"
                body = text "struct" <+> text dn <+> varName
                       <+> equals <+> braces (text cn) <> semi
                       <$> vsep (fieldAssign `fmap` fs)
                       <$> text "return" <+> varName <> semi
                fieldAssign (Field n t) = 
                  varName <> dot <> text cn <> dot <> text n
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