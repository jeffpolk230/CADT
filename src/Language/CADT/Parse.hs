module Language.CADT.Parse where

import Text.ParserCombinators.Parsec as X hiding (many, (<|>), optional)
import Text.ParserCombinators.Parsec.Expr as X
import Text.ParserCombinators.Parsec.Language as X
import qualified Text.ParserCombinators.Parsec.Token as T

import Control.Applicative as X

languageDef = haskellDef

lexer = T.makeTokenParser languageDef
identifier = T.identifier lexer
reservedOp = T.reservedOp lexer
reserved = T.reserved lexer
symbol = T.symbol lexer
braces = T.braces lexer
commaSep = T.commaSep lexer

data DataDecl = DataDecl { dataName :: String 
                         , dataCtors :: [Constructor] 
                         } deriving Show
data Constructor = Constructor { ctorName :: String 
                               , ctorFields :: [Field] 
                               } deriving Show
data Field = Field { fieldName :: String 
                   , fieldType :: String 
                   } deriving Show

parseField :: Parser Field
parseField = Field <$> identifier <*> typename
  where typename = reservedOp "::" *> identifier

parseConstructor :: Parser Constructor
parseConstructor = Constructor <$> identifier <*> fields
  where fields = braces $ commaSep parseField
        
parseDataDecl :: Parser DataDecl
parseDataDecl = DataDecl <$> name <*> ctors
  where name = reserved "data" *> identifier <* reservedOp "="
        ctors = parseConstructor `sepBy1` (symbol "|")
        