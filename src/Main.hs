module Main where

import System (getArgs)
import System.IO
import Data.Monoid
import Data.Foldable
import Text.PrettyPrint.Leijen
import Text.ParserCombinators.Parsec

import Language.CADT.Compile
import Language.CADT.Parse (parseDataDecl)

instance Monoid Doc where
  mempty = empty
  mappend = (<>)

main = do
  (path:_) <- getArgs
  source <- readFile path
  let decls = parse (many parseDataDecl) "" source
      doc = either (pretty . show) (foldMap pretty) decls
  displayIO stdout $ renderPretty 1.0 100 doc