{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Slow
  ( slowTestUtf8
  ) where

{-# OPTIONS_GHC -Wall #-}

import Control.Applicative (liftA2)
import Control.Monad (join, sequence)
import Data.Bits ((.&.), xor)
import Data.ByteString.Utf8 (isUtf8)
import Data.Char (chr)
import Data.Foldable (foldl')
import Data.Word (Word8)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI

testFail :: [B.ByteString] -> Bool
testFail !xs = Prelude.all (== False) $ Prelude.map isUtf8 xs

testSucc :: [B.ByteString] -> Bool
testSucc !xs = Prelude.all (== True ) $ Prelude.map isUtf8 xs

testPass :: [Bool] -> Bool
testPass !xs = Prelude.all (== True) xs

testAll :: Bool
testAll =
  testPass
    [ testB1Succ
    , testB1Fail
    , testB2Succ
    , testB2Fail_1
    , testB2Fail_2
    , testB3Succ
    , testB3Fail_1
    , testB3Fail_2
    ]

bAll, bSucc, bFail :: [[B.ByteString]]
bAll  = [b1Succ, b1Fail, b2Succ, b2Fail_1, b2Fail_2, b3Succ, b3Fail_1, b3Fail_2]
bSucc = [b1Succ, b2Succ, b3Succ]
bFail = [b1Fail, b2Fail_1, b2Fail_2, b3Fail_1, b3Fail_2] 

b1Succ, b1Fail, b2Succ, b2Fail_1, b2Fail_2, b3Succ, b3Fail_1, b3Fail_2 :: [B.ByteString]
b1Succ = [B.pack [x] | x <- [  0..127]]
b1Fail = [B.pack [x] | x <- [128..255]]

b2Succ   = [B.pack [x,y] | x <- [192..223], y <- [128..191]]
b2Fail_1 = [B.pack [x,y] | x <- [128..191], y <- [  0..127]]
b2Fail_2 = [B.pack [x,y] | x <- [224..255], y <- [192, 255]]

b3Succ   = [B.pack [x,y,z] | x <- [224..239], y <- [128..191], z <- [128..191]]
b3Fail_1 = [B.pack [x,y,z] | x <- [128..223], y <- [  0..127], z <- [  0..127]]
b3Fail_2 = [B.pack [x,y,z] | x <- [240..255], y <- [  0..127], z <- [  0..127]]

-- Brute force tests
-- Note that the 4-byte range is too large to brute force in a "reasonable" amount of time"

testB1Succ, testB1Fail, testB2Succ, testB2Fail_1, testB2Fail_2, testB3Succ, testB3Fail_1, testB3Fail_2 :: Bool
testB1Succ = testSucc b1Succ 
testB1Fail = testFail b1Fail

testB2Succ = testSucc b2Succ 
testB2Fail_1 = testFail b2Fail_1
testB2Fail_2 = testFail b2Fail_2

testB3Succ = testSucc b3Succ
testB3Fail_1 = testFail b3Fail_1
testB3Fail_2 = testFail b3Fail_2

randBX :: [[B.ByteString]] -> Gen B.ByteString
randBX bx = join $ Gen.element <$> Gen.element bx

randBSAppend :: Int -> [[B.ByteString]] -> Gen B.ByteString
randBSAppend  0 bx = randBX bx 
randBSAppend !n bx = liftA2 B.append (randBSAppend (n - 1) bx) (randBX bx)

-- hedgehog tests

-- THESE ARE SLOW.
randBSAppendSucc :: Gen B.ByteString
randBSAppendSucc = do
  m <- Gen.enum 1 6
  randBSAppend m bSucc 

randBSAppendFail :: Gen B.ByteString
randBSAppendFail = do
  m <- Gen.enum 1 6
  randBSAppend m bFail
