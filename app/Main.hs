{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Internal.Lazy
import Data.Functor.Identity
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import LLVM.Pretty
import LLVM.AST hiding (function, value)
import LLVM.AST.Type as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant

main = do
  -- "12+34" のような入力から整数2つを取り出して n, m とする
  [n, m] <- map read . (split '+') <$> getContents
  LT.putStrLn $
    ppllvm $ buildModule "main" $ do
      function "main" [] i32 $ \[] -> do
        a <- add (int32 n) (int32 m)
        ret a

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s1 -> w : split c s2
    where (w, s2) = break (== c) s1        