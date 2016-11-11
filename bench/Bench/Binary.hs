{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Binary where


import           Bench.Types

import qualified Data.Binary.Put as BP
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy.Char8 (toStrict)

{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord w8 w16 w32 w64) =
    toStrict . BP.runPut $ do
        BP.putWord8 w8
        BP.putWord16le w16
        BP.putWord32be w32
        BP.putWord64le w64
