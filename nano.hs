module Main where
main = do
   n <- getContents
   putStrLn $ "define i32 @main() {\n   ret i32 " ++ n ++ "\n}"