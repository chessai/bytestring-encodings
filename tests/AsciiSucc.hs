{-# LANGUAGE TemplateHaskell #-}

module AsciiSucc
  ( testIsAsciiSucc
  ) where

import Data.Bits ((.&.))
import Data.ByteString.IsUtf8 (isAscii)
import Data.Word (Word8, Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI

sizedByteString :: Range.Size -> Gen B.ByteString
sizedByteString (Range.Size n) = do
  m <- Gen.enum 0 n
  fmap B.pack $ Gen.list (Range.linear 0 m) (randWord8)
  where
    randWord8 :: Gen Word8
    randWord8 = fmap (\w -> w .&. 0x7F) (Gen.word8 Range.constantBounded)

randSuccBS :: Gen B.ByteString
randSuccBS = do
  bs <- Gen.sized sizedByteString
  n  <- Gen.enum 0 7
  m  <- Gen.enum 0 7
  pure (B.take m $ B.drop n bs) -- to give us some with non-0 offset

showRawByteString :: B.ByteString -> String
showRawByteString bs@(BI.PS fptr off len) =
  "Payload: " ++ show (B.unpack bs) ++ ", ptr: " ++ show fptr ++ ", offset: " ++ show off ++ " len: " ++ show len

prop_isAsciiSucc :: Property
prop_isAsciiSucc =
  property $ do
    xs <- forAllWith showRawByteString randSuccBS
    isAscii xs === True

testIsAsciiSucc :: IO Bool
testIsAsciiSucc = checkParallel $$(discover) 
