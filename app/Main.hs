module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  str <- getLine
  return ()
  --print $ ((foldl1 App) (eval (readExpr str))  ))

