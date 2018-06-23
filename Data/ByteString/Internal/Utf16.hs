--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -O2 -Wall #-}

--------------------------------------------------------------------------------

module Data.ByteString.Internal.Utf16
  ( isUtf16LE
  , isUtf16BE
  ) where

--------------------------------------------------------------------------------

import Data.Bits (shiftL)
import Data.Bool (Bool(..), (&&), (||), otherwise)
import Data.ByteString.Internal (ByteString(..))
import Data.Function ( (.) )
import Data.Ord (Ord(..))
import GHC.Exts
import GHC.Num (Num(..))
import GHC.Real (fromIntegral)
import GHC.Word (Word16(..))
import qualified Data.ByteString.Unsafe as B

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

validate1 :: Word16 -> Bool
validate1 x1 = (x1 >= 0 && x1 < 0xD800) || x1 > 0xDFFF
{-# INLINE validate1 #-}

validate2 :: Word16 -> Word16 -> Bool
validate2 x1 x2 = x1 >= 0xD800 && x1 <= 0xDBFF &&
                  x2 >= 0xDC00 && x2 <= 0xDFFF
{-# INLINE validate2 #-}

--------------------------------------------------------------------------------

isUtf16LE :: ByteString -> Bool
isUtf16LE (PS _ _ 0) = True
isUtf16LE bs@(PS _ _ len) = go 0
  where
    go !i
      | i >= len                       = True
      | i + 1 < len && validate1 x1    = go (i + 2)
      | i + 3 < len && validate2 x1 x2 = go (i + 4)
      | otherwise = False
      where
        x1  = idx i       + (idx (i + 1) `shiftL` 8)
        x2  = idx (i + 2) + (idx (i + 3) `shiftL` 8)
        idx = fromIntegral . B.unsafeIndex bs :: Int -> Word16
{-# INLINE [0] isUtf16LE #-}

isUtf16BE :: ByteString -> Bool
isUtf16BE (PS _ _ 0) = True
isUtf16BE bs@(PS _ _ len) = go 0
  where
    go !i
      | i >= len                       = True
      | i + 1 < len && validate1 x1    = go (i + 2)
      | i + 3 < len && validate2 x1 x2 = go (i + 4)
      | otherwise = False
      where
        x1  = (idx i `shiftL` 8)       + idx (i + 1)
        x2  = (idx (i + 2) `shiftL` 8) + idx (i + 3)
        idx = fromIntegral . B.unsafeIndex bs :: Int -> Word16
{-# INLINE [0] isUtf16BE #-}

--------------------------------------------------------------------------------
