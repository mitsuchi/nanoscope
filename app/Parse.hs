module Parse where

import AstData
import Control.Monad.Combinators.Expr
import Data.Text.Internal.Lazy
import Data.Functor.Identity
import Data.Void
import qualified Data.Text as DT
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol s = L.symbol sc s

ops :: [[Operator Parser Expr]]
ops =
  [
    [ InfixL (ExprMul <$ (symbol "*"))
    , InfixL (ExprDiv <$ (symbol "/")) ],
    [ InfixL (ExprAdd <$ (symbol "+"))
    , InfixL (ExprSub <$ (symbol "-")) ]
  ]

expr :: Parser Expr
expr = makeExprParser term ops

term :: Parser Expr
term = ExprInt <$> lexeme L.decimal
    <|> parens expr

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

parseExpr :: String -> Expr
parseExpr str = case parse (sc *> expr) "<stdin>" str of
  Right ast -> ast
  Left bundle -> error $ errorBundlePretty bundle