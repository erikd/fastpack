{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Types where

import Data.Word (Word8, Word16, Word32, Word64)

data BenchWord
    = BenchWord Word8 Word16 Word32 Word64
    deriving (Eq, Show)
