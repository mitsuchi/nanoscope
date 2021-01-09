module Main where

import Parse
import Compile
import qualified Data.Text.Lazy.IO as LT

main :: IO ()
main = do
    str <- getLine
    let ast = parseStmts str
        llvm = compileToLLVM ast
    LT.putStrLn llvm