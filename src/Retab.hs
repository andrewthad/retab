{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language PatternSynonyms #-}

module Retab
  ( toDoubleAngleQuotationMark
  , toDoubleAngleQuotationMarkTerse
  ) where

import Data.Word (Word8)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (ByteArray,MutableByteArray)
import Control.Monad.ST (ST,runST)

import qualified Data.Primitive as PM
import qualified Data.Bytes as Bytes

pattern Tab :: Word8
pattern Tab = 0x09

pattern Open :: Word8
pattern Open = 0xAB

pattern Close :: Word8
pattern Close = 0xBB

pattern Newline :: Word8
pattern Newline = 0x0A

-- | Replace leading tabs with the symbols » (U+00BB) and « (U+00AB).
-- This does not use UTF-8. It uses 8859-1 so that the characters are
-- encoded as a single byte. Look in 
toDoubleAngleQuotationMark :: Bytes -> Bytes
toDoubleAngleQuotationMark b@(Bytes src soff slen)
  | Bytes.any (==Tab) b = runST $ do
      dst <- PM.newByteArray (1 + slen * 2)
      len <- action 0 slen 0 src soff dst 0
      PM.shrinkMutableByteArray dst len
      dst' <- PM.unsafeFreezeByteArray dst
      pure $! Bytes.fromByteArray $! dst'
  | otherwise = b

-- | Variant of 'toDoubleAngleQuotationMark'. This does not add newlines
-- after indentation changes. The result is less visually pleasing (bad
-- for pretty printing) but is easier to handle with a parser.
toDoubleAngleQuotationMarkTerse :: Bytes -> Bytes
toDoubleAngleQuotationMarkTerse b@(Bytes src soff slen)
  | Bytes.any (==Tab) b = runST $ do
      dst <- PM.newByteArray (1 + slen * 2)
      len <- action 1 slen 0 src soff dst 0
      PM.shrinkMutableByteArray dst len
      dst' <- PM.unsafeFreezeByteArray dst
      pure $! Bytes.fromByteArray $! dst'
  | otherwise = b

-- Precondition: Cursor is positioned at the beginning of a line,
-- before any tab characters.
action :: Int -> Int -> Int -> ByteArray -> Int -> MutableByteArray s -> Int -> ST s Int
action !isTerse !len0 !indentation !src !srcIx0 !dst !dstIx0
  | len0 < 0 = errorWithoutStackTrace "Retab.action: implementation mistake"
  | otherwise = do
      let applyClose !closeCharsNeeded !dstIx1 = if closeCharsNeeded > 0
            then do
              PM.writeByteArray dst dstIx1 Close
              if isTerse == 1
                then do
                  applyClose (closeCharsNeeded - 1) (dstIx1 + 1)
                else do
                  PM.writeByteArray dst (dstIx1 + 1) Newline
                  applyClose (closeCharsNeeded - 1) (dstIx1 + 2)
            else pure dstIx1
      let applyOpen !openCharsNeeded !dstIx1 = if openCharsNeeded > 0
            then do
              PM.writeByteArray dst dstIx1 Open
              if isTerse == 1
                then do
                  applyOpen (openCharsNeeded - 1) (dstIx1 + 1)
                else do
                  PM.writeByteArray dst (dstIx1 + 1) Newline
                  applyOpen (openCharsNeeded - 1) (dstIx1 + 2)
            else pure dstIx1
      let !leadingTabCount = Bytes.length (Bytes.takeWhile (==Tab) (Bytes src srcIx0 len0))
      dstIx1 <- case compare leadingTabCount indentation of
        LT -> applyClose (indentation - leadingTabCount) dstIx0
        GT -> applyOpen (leadingTabCount - indentation) dstIx0
        EQ -> pure dstIx0
      let !srcIx1 = srcIx0 + leadingTabCount
      let !len1 = len0 - leadingTabCount
      let !srcBytes1 = Bytes src srcIx1 len1
      let !lineLen = Bytes.length (Bytes.takeWhile (/=Newline) srcBytes1)
      case len1 of
        0 -> pure dstIx1
        _ -> if lineLen == len1
          then errorWithoutStackTrace ("Retab.action: figure out what to do when the file does not end with a newline. Line len was " ++ show lineLen ++ " and len1 was " ++ show len1)
          else do
            let lineLen' = lineLen + 1
            Bytes.unsafeCopy dst dstIx1 (Bytes src srcIx1 lineLen')
            action
              isTerse
              (len1 - lineLen')
              leadingTabCount
              src
              (srcIx1 + lineLen')
              dst
              (dstIx1 + lineLen')
