{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Cereal where


import           Bench.Types

import qualified Data.Serialize.Put as CP
import           Data.ByteString (ByteString)


{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord w8 w16 w32 w64) =
    CP.runPut $ do
        CP.putWord8 w8
        CP.putWord16le w16
        CP.putWord32be w32
        CP.putWord64le w64
