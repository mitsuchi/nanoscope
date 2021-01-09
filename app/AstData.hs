module AstData where

-- EBNF:
-- program = stmt*
-- stmt = expr | assign expr expr
-- expr = expr '+' expr | expr '-' expr
--      | expr '*' expr | expr '/' expr
--      | int | var

data Stmt =           -- 文は、次のいずれか
  JustExpr Expr       -- 式そのもの
  | Assign Expr Expr  -- 変数への代入文

data Expr =           -- 式は、次のいずれか
  ExprAdd Expr Expr   -- 式 + 式
  | ExprSub Expr Expr -- 式 - 式
  | ExprMul Expr Expr -- 式 * 式
  | ExprDiv Expr Expr -- 式 / 式
  | ExprInt Integer   -- 式の中身が整数一個
  | Var String        -- 変数の値の参照
  deriving Show
