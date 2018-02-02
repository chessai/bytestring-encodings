module Main (main) where

import Criterion.Main
import Data.ByteString.Ascii (isAscii)
import Data.ByteString (ByteString)
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  let bs = BC.pack (L.take 50000 (L.cycle ['a'..'z']))
  defaultMain
    [ bgroup "utf8"
      [ bench "Data.ByteString.all" $ whnf standardIsAscii bs
      , bench "isAscii" $ whnf isAscii bs
      ]
    ]

standardIsAscii :: ByteString -> Bool
standardIsAscii bs = B.all (\x -> x < 127) bs


