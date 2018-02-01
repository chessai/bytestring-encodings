module Main where

import qualified AsciiSucc as Succ
import qualified AsciiFail as Fail

main :: IO Bool
main = do
  Succ.testIsAsciiSucc
  Fail.testIsAsciiFail
