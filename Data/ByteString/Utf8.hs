{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -O2 #-}

module Data.ByteString.Utf8
  ( isUtf8
  ) where

import Data.Bits ((.|.),(.&.),shiftL)
import Data.ByteString.Internal (ByteString(..))
import Data.Maybe (fromJust)
import Data.Word (Word8, Word32)
import GHC.Base
import GHC.Enum (Enum(..))
import GHC.Num (Num(..))
import GHC.Real (fromIntegral)

import qualified Data.ByteString as B

toW32 :: Word8 -> Word32
toW32 w8 = fromIntegral (fromEnum w8)

isUtf8 :: ByteString -> Bool
isUtf8 (PS _ _ 0) = True
isUtf8 b =
  case B.uncons b of
    Nothing -> True
    Just (w, ws) ->
      case ox of
        _ | ox < 0x80  -> isUtf8 ws
          | ox > 0xff  -> False
          | ox < 0xc0  -> False
          | ox < 0xe0  -> check1
          | ox < 0xf0  -> check_byte 2 0xf 0
          | ox < 0xf8  -> check_byte 3 0x7  0x10000
          | ox < 0xfc  -> check_byte 4 0x3  0x200000
          | ox < 0xfe  -> check_byte 5 0x1  0x4000000
          | otherwise  -> False
      where
        ox = toW32 w
        
        check1 :: Bool
        check1 =
          case B.uncons ws of
            Nothing -> False
            Just (w1, ds)
              | oc .&. 0xc0 /= 0x80 || d < 0x000080 -> False
              | otherwise -> isUtf8 ds
              where
                oc = toW32 w1
                d  = ((ox .&. 0x1f) `shiftL` 6) .|. (oc .&. 0x3f)


        check_byte :: Int -> Word32 -> Word32 -> Bool
        check_byte i mask overlong = aux i ws (ox .&. mask)
          where
             
            aux 0 rs acc
              | overlong <= acc &&
                acc <= 0x10ffff &&
                (acc < 0xd800 || 0xdfff < acc) &&
                (acc < 0xfffe || 0xffff < acc) = isUtf8 rs
              | otherwise = False

            aux n ks acc
              | toW32 r .&. 0xc0 == 0x80 =
                  aux (n-1) rs (acc `shiftL` 6 .|. (toW32 r .&. 0x3f))
              where
                (r,rs) = fromJust $ B.uncons ks
            
            aux _ _ _ = False
