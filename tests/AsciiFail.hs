{-# LANGUAGE TemplateHaskell #-}

module AsciiFail where

import Data.ByteString.IsUtf8 (isAscii)
import Data.Word (Word8, Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString as B

randWord8 :: Gen Word8
randWord8 = Gen.word8 Range.constantBounded

sizedByteString :: Range.Size -> Gen B.ByteString
sizedByteString (Range.Size n) = do
  m <- Gen.enum 0 n
  fmap B.pack $ Gen.list (Range.linear 0 m) (randWord8)

randDrop :: Gen Int
randDrop = Gen.enum 0 7

randTake :: Gen Int
randTake = Gen.enum 0 7

randSuccBS :: Gen B.ByteString
randSuccBS = do
  bs <- Gen.sized sizedByteString
  n  <- randDrop
  m  <- randDrop
  pure (B.take m $ B.drop n bs) -- to give us some with non-0 offset

prop_isAsciiSucc :: Property
prop_isAsciiSucc =
  property $ do
    xs <- forAll randSuccBS
    isAscii xs === True

testIsAsciiSucc :: IO Bool
testIsAsciiSucc = checkParallel $$(discover) 
