module AsciiTest where

import Data.ByteString.IsUtf8 (isAscii)
import Test.QuickCheck

import qualified Data.ByteString as B

sizedByteString :: Int -> Gen B.ByteString
sizedByteString n = do m <- choose (0, n)
                       fmap B.pack $ vectorOf m arbitrary

instance Arbitrary B.ByteString where
  arbitrary = do
    bs <- sized sizedByteString
    n  <- choose (0,7)
    m  <- choose (0,7) 
    pure (B.take m $ B.drop n bs) -- to give us some with non-0 offset

testIsAscii :: IO ()
testIsAscii = quickCheck (withMaxSuccess 10000 isAscii)
