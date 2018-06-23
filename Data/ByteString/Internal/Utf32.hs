--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -O2 -Wall #-}

--------------------------------------------------------------------------------

module Data.ByteString.Internal.Utf32
  ( isUtf32LE
  , isUtf32BE
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
import GHC.Word (Word32(..))
import qualified Data.ByteString.Unsafe as B

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

validate :: Word32 -> Bool
validate x1 = x1 < 0xD800 || (x1 > 0xDFFF && x1 <= 0x10FFFF)
{-# INLINE validate #-}

--------------------------------------------------------------------------------

-- | /O(n)/ determine if a 'ByteString' is big endian
--   UTF-32 encoded.
isUtf32BE :: ByteString -> Bool
isUtf32BE (PS _ _ 0) = True
isUtf32BE bs@(PS _ _ len) = go 0
  where
    {-# INLINE go #-}
    go !i
      | i >= len = True
      | i + 3 < len && validate x = go (i + 4)
      | otherwise = False
      where
        x   = shiftL x1 24 + shiftL x2 16 + shiftL x3 8 + x4
        x1  = idx i
        x2  = idx (i + 1)
        x3  = idx (i + 2)
        x4  = idx (i + 3)
        idx = fromIntegral . B.unsafeIndex bs :: Int -> Word32
{-# INLINE isUtf32BE #-}

-- | /O(n)/ determine if a 'ByteString' is little endian
--   UTF-32 encoded.
isUtf32LE :: ByteString -> Bool
isUtf32LE (PS _ _ 0) = True
isUtf32LE bs@(PS _ _ len) = go 0
  where
    {-# INLINE go #-}
    go !i
      | i >= len = True
      | i + 3 < len && validate x = go (i + 4)
      | otherwise = False
      where
        x   = shiftL x4 24 + shiftL x3 16 + shiftL x2 8 + x1
        x1  = idx i
        x2  = idx (i + 1)
        x3  = idx (i + 2)
        x4  = idx (i + 3)
        idx = fromIntegral . B.unsafeIndex bs :: Int -> Word32
{-# INLINE isUtf32LE #-}

--------------------------------------------------------------------------------
