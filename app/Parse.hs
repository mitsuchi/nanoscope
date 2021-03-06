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

identifier :: Parser String
identifier = lexeme $ do
  firstLetter <- letterChar
  middleLetters <- many ( oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']) )
  lastLetters <- many (oneOf "!?_'")
  pure $ firstLetter : (middleLetters ++ lastLetters)

ops :: [[Operator Parser Expr]]
ops =
  [
    [ InfixL (ExprMul <$ (symbol "*"))
    , InfixL (ExprDiv <$ (symbol "/")) ],
    [ InfixL (ExprAdd <$ (symbol "+"))
    , InfixL (ExprSub <$ (symbol "-")) ],
    [ InfixL (ExprLT <$ (symbol "<")) ],    
    [ InfixL (Assign <$ (symbol "=")) ]
  ]

expr :: Parser Expr
expr = makeExprParser term ops

term :: Parser Expr
term = ExprInt <$> lexeme L.decimal
  <|> parens expr
  <|> exprIf
  <|> funDef
  <|> try funCall
  <|> Var <$> lexeme identifier

exprIf :: Parser Expr
exprIf = do
    symbol "if"
    condExpr <- expr
    symbol "then"
    thenExpr <- expr
    symbol "else"
    elseExpr <- expr
    pure $ ExprIf condExpr thenExpr elseExpr

funDef :: Parser Expr
funDef = do
    symbol "def"
    funName <- identifier
    params <- parens $ many identifier
    body <- expr
    pure $ Fun funName params body

funCall :: Parser Expr
funCall = do
    name <- identifier
    args <- parens $ expr `sepBy` (symbol ",")
    pure $ FunCall name args

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

exprs :: Parser [Expr]
exprs = expr `sepEndBy` (symbol ";")

parseExpr :: String -> [Expr]
parseExpr str = case parse (sc *> exprs) "<stdin>" str of
  Right ast -> ast
  Left bundle -> error $ errorBundlePretty bundle