{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Types where

import Data.Word (Word8, Word16, Word32, Word64)

import           Foreign.Storable (Storable (..))


data BenchWord
    = BenchWord Word64 Word64 Word32 Word32 Word16 Word16 Word8 Word8
    deriving (Eq, Show)

benchWordSize :: Int
benchWordSize =
    let (BenchWord le64 be64 le32 be32 le16 be16 w8a w8b) = benchWordZero in
    sizeOf le64 + sizeOf be64 + sizeOf le32 + sizeOf be32 + sizeOf le16 + sizeOf be16 + sizeOf w8a + sizeOf w8b


benchWordZero :: BenchWord
benchWordZero = BenchWord 0 0 0 0 0 0 0 0

-- Used in benchmarking to force evaluation.
{-# INLINE getThird #-}
getThird :: BenchWord -> Word32
getThird (BenchWord _ _ le32 _ _ _ _ _) = fromIntegral le32
