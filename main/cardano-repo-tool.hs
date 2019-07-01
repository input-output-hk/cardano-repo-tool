module Main where

import qualified RepoTool (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  RepoTool.someFunc
