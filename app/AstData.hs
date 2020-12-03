module AstData where

data Expr =            -- 式
  ExprAdd Expr Expr   -- 式 + 式
  | ExprSub Expr Expr -- 式 - 式
  | ExprMul Expr Expr -- 式 * 式
  | ExprDiv Expr Expr -- 式 / 式
  | ExprInt Integer   -- 式の中身が整数一個
  | Assign Expr Expr  -- 変数への代入
  | Var String        -- 変数の値の参照
  | ExprIf Expr Expr Expr -- if 条件式 then式 else式
  | ExprLT Expr Expr  -- 式 < 式  
  | Fun String [String] Expr -- 関数定義: 名前 [仮引数] 本体
  | FunCall String [Expr]    -- 関数呼び出し: 名前 [引数]  
  deriving Show
