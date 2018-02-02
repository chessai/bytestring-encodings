{-# LANGUAGE TemplateHaskell #-}

module TestUtf8
  ( testUtf8
  ) where

import Control.Applicative (liftA2)
import Data.Bits ((.&.), xor)
import Data.ByteString.Utf8 (isUtf8)
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
  fmap B.pack $ Gen.list (Range.constant 0 m) randWord8
  where
    randWord8 :: Gen Word8
    randWord8 = Gen.word8 Range.constantBounded

randBS :: Gen B.ByteString
randBS = do
  bs <- Gen.sized sizedByteString
  n  <- Gen.enum 0 7
  pure (B.drop n bs) -- to give us some with non-0 offset

showRawByteString :: B.ByteString -> String
showRawByteString bs@(BI.PS fptr off len) =
  "Payload: " ++ show (B.unpack bs) ++ ", ptr: " ++ show fptr ++ ", offset: " ++ show off ++ " len: " ++ show len

prop_isUtf8 :: Property
prop_isUtf8 =
  property $ do
    xs <- forAllWith showRawByteString randBS
    isUtf8 xs === True

testUtf8 :: IO Bool
testUtf8 = checkParallel $$(discover) 
