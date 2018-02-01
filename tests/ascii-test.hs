module AsciiTest where

import Data.ByteString.IsUtf8 (isAscii)
import Test.QuickCheck

import qualified Data.ByteString as B

sizedByteString n = do m <- choose (0, n)
                       fmap B.pack $ vectorOf m arbitrary

instance Arbitrary B.ByteString where
  arbitrary = do
  bs <- sized sizedByteString
  n  <- choose (0,2)
  pure (B.drop n bs) -- to give us some with non-0 offset
