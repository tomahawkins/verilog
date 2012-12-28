-- | Unsigned bit vectors.
module Data.BitVec
  ( BitVec
  , bitVec
  , select
  ) where

import Data.Bits
import Data.Monoid

data BitVec = BitVec Int Integer deriving (Show, Eq)

instance Num BitVec where
  BitVec w1 v1 + BitVec w2 v2 = BitVec (max w1 w2) (v1 + v2)
  BitVec w1 v1 - BitVec w2 v2 = bitVec w $ v1 - v2 where w = max w1 w2
  BitVec w1 v1 * BitVec w2 v2 = BitVec (w1 + w2) (v1 * v2)
  abs = id
  signum (BitVec _ v) = if v == 0 then 0 else 1
  fromInteger = error "BitVec can not be created fromInteger."

instance Bits BitVec where
  BitVec w1 v1 .&.   BitVec w2 v2 = BitVec (max w1 w2) (v1 .&.   v2)
  BitVec w1 v1 .|.   BitVec w2 v2 = BitVec (max w1 w2) (v1 .|.   v2)
  BitVec w1 v1 `xor` BitVec w2 v2 = BitVec (max w1 w2) (v1 `xor` v2)
  complement (BitVec w v) = bitVec w $ complement v
  bitSize (BitVec w _) = w
  isSigned _ = False

instance Monoid BitVec where
  mempty = BitVec 0 0
  mappend (BitVec w1 v1) (BitVec w2 v2) = BitVec (w1 + w2) (shiftL v1 w2 .|. v2)

-- | BitVec construction, given width and value.
bitVec :: Int -> Integer -> BitVec
bitVec w v = BitVec w' $ v .&. ((2 ^ fromIntegral w') - 1)
  where
  w' = max w 0

-- | Bit seclection.  LSB is 0.
select :: BitVec -> (Int, Int) -> BitVec
select (BitVec _ v) (msb, lsb) = bitVec (msb - lsb + 1) $ shiftR v lsb

