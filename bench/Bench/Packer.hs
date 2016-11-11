{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Packer where


import           Bench.Types

import qualified Data.Packer as PP
import           Data.ByteString (ByteString)


{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord w8 w16 w32 w64) =
    PP.runPacking (1 + 2 + 4 + 8)  $ do
        PP.putWord8 w8
        PP.putWord16LE w16
        PP.putWord32BE w32
        PP.putWord64LE w64
