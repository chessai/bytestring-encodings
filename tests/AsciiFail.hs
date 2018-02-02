{-# LANGUAGE TemplateHaskell #-}

module AsciiFail 
  ( testIsAsciiFail
  ) where

import Control.Applicative (liftA2)
import Data.Bits (xor)
import Data.ByteString.IsUtf8 (isAscii)
import Data.Word (Word8, Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI

-- This should generate a 'ByteString' for which isAscii should short-circuit;
-- i.e. the check of any 'Word8' in the 'ByteString' should fail.
sizedByteString_ShortCircuit :: Range.Size -> Gen B.ByteString
sizedByteString_ShortCircuit (Range.Size n) = do
  m <- Gen.enum 0 n
  fmap B.pack $ Gen.list (Range.constant 0 m) (randWord8)
  where
    randWord8 :: Gen Word8
    randWord8 = fmap (\w -> if w > 127 then w else w `xor` 0xFF) (Gen.word8 Range.constantBounded)

-- a ByteString b for which isAscii b should never fail
noFailBS :: [Word8]
noFailBS = [0x7F,0x7F,0x7F,0x7F,0x7F,0x7F,0x7F,0x7F]

-- This makes sure we test past the first 8 bytes, and before the last 8 bytes, by generating a failing 'ByteString' of
-- length >= 1, then prepending and appending 'noFailBS' to it. 
sizedByteString :: Range.Size -> Gen B.ByteString
sizedByteString (Range.Size n) = do
  m <- Gen.enum 1 (n+1)
  fmap B.pack $
    liftA2 (++) (pure noFailBS) $ 
      liftA2 (++) (Gen.list (Range.constant 8 (m+8)) randWord8) (pure noFailBS)
  where
    randWord8 :: Gen Word8
    randWord8 = fmap (\w -> if w > 127 then w else w `xor` 0xFF) (Gen.word8 Range.constantBounded)
 
randFailBS_ShortCircuit :: Gen B.ByteString
randFailBS_ShortCircuit = do
  bs <- Gen.sized sizedByteString_ShortCircuit 
  n  <- Gen.enum 0 7
  pure (B.drop n bs) -- to give us some with non-0 offset

randFailBS :: Gen B.ByteString
randFailBS = do
  bs <- Gen.sized sizedByteString
  n  <- Gen.enum 0 7
  pure (B.drop n bs) -- to give us some with non-0 offset 

showRawByteString :: B.ByteString -> String
showRawByteString bs@(BI.PS fptr off len) =
  "Payload: " ++ show (B.unpack bs) ++ ", ptr: " ++ show fptr ++ ", offset: " ++ show off ++ " len: " ++ show len

prop_noFail :: Property
prop_noFail =
  property $ do
    isAscii (B.pack noFailBS) === True

prop_isAsciiFail_ShortCircuit :: Property
prop_isAsciiFail_ShortCircuit =
  property $ do
    xs <- forAllWith showRawByteString randFailBS_ShortCircuit
    if B.length xs > 0
      then isAscii xs === False
      else isAscii xs === True

prop_isAsciiFail :: Property
prop_isAsciiFail =
  property $ do
    xs <- forAllWith showRawByteString randFailBS
    if B.length xs > 0
    then isAscii xs === False
    else isAscii xs === True

testIsAsciiFail :: IO Bool
testIsAsciiFail = checkParallel $$(discover) 
