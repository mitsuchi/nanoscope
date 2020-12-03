{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compile where

import AstData
import Control.Monad.Fix
import Control.Monad.Trans.State
import qualified Data.Map as M
import LLVM.Pretty
import LLVM.AST hiding (function, value)
import LLVM.AST.IntegerPredicate as P
import LLVM.AST.Type as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant

type Env = M.Map String Operand

compileExpr :: (MonadFix m, MonadIRBuilder m) => Expr -> StateT Env m Operand
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
compileExpr (Assign (Var v) e) = do
    env <- get
    e' <- compileExpr e
    let env' = M.insert v e' env
    put env'
    pure e'
compileExpr (Var v) = do
    env <- get
    let opr = case M.lookup v env of
              Just x  -> x
              Nothing -> error $ "variable " ++ v ++ " not found"
    pure opr
compileExpr (ExprLT e1 e2) = do
  e1' <- compileExpr e1
  e2' <- compileExpr e2
  icmp P.SLT e1' e2'
compileExpr (ExprIf condExpr thenExpr elseExpr) = mdo
  cond <- compileExpr condExpr
  condBr cond ifThen ifElse
  -- then:
  ifThen <- block `named` "then"
  oprThen <- compileExpr thenExpr
  br ifEnd
  enfOfThen <- currentBlock
  -- else:
  ifElse <- block `named` "else"
  oprElse <- compileExpr elseExpr
  -- end:
  br ifEnd
  endOfElse <- currentBlock
  ifEnd <- block `named` "end"
  -- phi
  phi [(oprThen, enfOfThen), (oprElse, endOfElse)]

compileExprs :: (MonadFix m, MonadIRBuilder m) => [Expr] -> StateT Env m Operand
compileExprs [e] = compileExpr e
compileExprs (e:es) = do
  compileExpr e
  compileExprs es
      
compileToLLVM ast =
    ppllvm $ buildModule "main" $ do
    function "main" [] i32 $ \[] -> do
        opr <- evalStateT (compileExprs ast) $ M.empty
        ret opr