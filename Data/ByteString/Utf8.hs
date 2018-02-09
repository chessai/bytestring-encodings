{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -O2 #-}

module Data.ByteString.Utf8
  ( isUtf8 
  ) where

import Data.Bits ( (.&.) )
import Data.ByteString.Ascii (isAscii)
import Data.ByteString.Internal (ByteString(..), accursedUnutterablePerformIO)
import Data.Word (Word8)
import GHC.Base
import GHC.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peek)

data Utf8 = U8 | U16 | U24 | U32 | UNot
  deriving Eq

which :: Word8 -> Utf8
which c
  | isUtf8b8  c = U8
  | isUtf8b16 c = U16
  | isUtf8b24 c = U24
  | isUtf8b32 c = U32
  | otherwise   = UNot

isUtf8Ptr :: Ptr Word8 -> Ptr Word8 -> IO Bool
isUtf8Ptr !p !q
  | p == q    = pure True
  | otherwise = do
      c <- peek p

      case which c of
        U8  -> isUtf8Ptr (p `plusPtr` 1) q 
        U16 -> if q `minusPtr` p >= 2
                 then 
                   do d <- peek (p `plusPtr` 1)  
                      if isUtf8OtherBytes d then isUtf8Ptr (p `plusPtr` 2) q else pure False
                 else pure False
        U24 -> if q `minusPtr` p >= 3
                 then
                   do d <- peek (p `plusPtr` 1); e <- peek (p `plusPtr` 2)
                      if (isUtf8OtherBytes d && isUtf8OtherBytes e) then isUtf8Ptr (p `plusPtr` 3) q else pure False
                 else pure False
        U32 -> if q `minusPtr` p >= 4
                 then
                   do d <- peek (p `plusPtr` 1); e <- peek (p `plusPtr` 2); f <- peek (p `plusPtr` 3);
                      if (isUtf8OtherBytes d && isUtf8OtherBytes e && isUtf8OtherBytes f) then isUtf8Ptr (p `plusPtr` 4) q else pure False
                 else pure False
        UNot -> pure False

-- Hex:     0x80
-- Binary:  10000000
-- Decimal: 128
isUtf8b8 :: Word8 -> Bool
isUtf8b8 !w = w .&. 0x80 == 0

-- Hex:     0xE0 
-- Binary:  11100000
-- Decimal: 224
isUtf8b16 :: Word8 -> Bool
isUtf8b16 !w = w .&. 0xE0 == 0xC0

-- Hex:     0xF0
-- Binary:  11110000
-- Decimal: 240 
isUtf8b24 :: Word8 -> Bool
isUtf8b24 !w = w .&. 0xF0 == 0xE0


-- Hex:     0xF8
-- Binary:  11111000
-- Decimal: 248
isUtf8b32 :: Word8 -> Bool
isUtf8b32 !w = w .&. 0xF8 == 0xF0

-- Hex:     0xC0
-- Binary:  11000000
-- Decimal: 192
isUtf8OtherBytes :: Word8 -> Bool
isUtf8OtherBytes !w = w .&. 0xC0 == 0x80

isUtf8 :: ByteString -> Bool
isUtf8   (PS _ _ 0) = True
isUtf8 b@(PS fp (I# o#) (I# l#)) = if isAscii b then True else
  accursedUnutterablePerformIO
    $ withForeignPtr fp
      $ \(Ptr addr) ->
        do
          let
            start, end :: Ptr Word8
            start = Ptr (plusAddr# addr o#)
            end   = Ptr (plusAddr# addr (o# +# l#))
            
          isUtf8Ptr start end
