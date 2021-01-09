{-# LANGUAGE OverloadedStrings #-}
module Compile where

import AstData
import Control.Monad.Trans.State
import qualified Data.Map as M
import LLVM.Pretty
import LLVM.AST hiding (function, value)
import LLVM.AST.Type as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant

type Env = M.Map String Operand

compileExpr :: (MonadIRBuilder m) => Expr -> StateT Env m Operand
compileExpr (ExprInt n) = pure $ int32 n
compileExpr (ExprAdd e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    add e1' e2'
compileExpr (ExprSub e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    sub e1' e2'
compileExpr (ExprMul e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    mul e1' e2'
compileExpr (ExprDiv e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    sdiv e1' e2'
compileExpr (Var v) = do
    env <- get
    let opr = case M.lookup v env of
              Just x  -> x
              Nothing -> error $ "variable " ++ v ++ " not found"
    pure opr

compileStmt :: (MonadIRBuilder m) => Stmt -> StateT Env m (Maybe Operand)
compileStmt (JustExpr e) = do
  compileExpr e
  pure Nothing
compileStmt (Assign (Var v) e) = do
  env <- get
  e' <- compileExpr e
  let env' = M.insert v e' env
  put env'
  pure Nothing
compileStmt (Retn e) = do
  e' <- compileExpr e
  pure $ Just e'

compileStmts :: (MonadIRBuilder m) => [Stmt] -> StateT Env m (Maybe Operand)
compileStmts [s] = compileStmt s
compileStmts (s:ss) = do
  compileStmt s
  compileStmts ss
      
compileToLLVM ast =
    ppllvm $ buildModule "main" $ do
    function "main" [] i32 $ \[] -> do
        opr <- evalStateT (compileStmts ast) $ M.empty
        case opr of
          Just opr' -> ret opr'
          Nothing   -> error "must end with return"