{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -O2 -Wall -Werror #-}

module Data.ByteString.IsUtf8
  ( isAscii
  ) where

import Control.Monad (guard)
import Data.Bits ( (.&.) )
import Data.ByteString.Internal (ByteString(..), accursedUnutterablePerformIO)
import Data.Word (Word8, Word64)
import GHC.Base
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peek)

-- | 1000000010000000100000001000000010000000100000001000000010000000
m64 :: Word64
m64 = 9259542123273814144

-- | 10000000
m8 :: Word8
m8 = 128

isAsciiPtrW64 :: Ptr Word64 -> Ptr Word64 -> IO Bool
isAsciiPtrW64 !p !q
  | p == q    = pure True
  | otherwise = do
      c <- peek p
      if isAsciiW64 c
        then isAsciiPtrW64 (p `plusPtr` 8) q
        else pure False

isAsciiPtrW8 :: Ptr Word8 -> Ptr Word8 -> IO Bool
isAsciiPtrW8 !p !q
  | p == q    = pure True
  | otherwise = do
      c <- peek p
      if isAsciiW8 c
        then isAsciiPtrW8 (p `plusPtr` 1) q
        else pure False

isAsciiW64 :: Word64 -> Bool
isAsciiW64 !w = w .&. m64 == 0

isAsciiW8 :: Word8 -> Bool
isAsciiW8 !w = w .&. m8 == 0

alignPtrPos :: Ptr a -> Ptr a
alignPtrPos addr@(Ptr a) 
  = case remAddr# a 8# of
      0# -> addr
      n  -> Ptr (plusAddr# a (8# -# n))

alignPtrNeg :: Ptr a -> Ptr a
alignPtrNeg addr@(Ptr a)
  = case remAddr# a 8# of
      0# -> addr
      n  -> Ptr (plusAddr# a (n -# 8#))

isAscii :: ByteString -> Bool
isAscii (PS _ _ 0)  = True
isAscii (PS fp@(ForeignPtr addr _) (I# o#) (I# l#)) =
  accursedUnutterablePerformIO
    $ withForeignPtr fp
      $ \ptr ->
        do
          let
            startPre, endPre, startPost, endPost :: Ptr Word8
            startMid, endMid :: Ptr Word64
            
            startPre  = Ptr (plusAddr# addr o#)
            endPre    = alignPtrPos ptr
            startMid  = endPre `plusPtr` 1
            endMid    = startPost `plusPtr` (-1)
            startPost = alignPtrNeg endPost
            endPost   = Ptr (plusAddr# addr (o# +# l#))
          
          startIsAscii <- isAsciiPtrW8  (startPre) (endPre) 
          guard startIsAscii  
          endIsAscii   <- isAsciiPtrW8  (startPost) (endPost) 
          guard endIsAscii 
          isAsciiPtrW64 (startMid)  (endMid)
