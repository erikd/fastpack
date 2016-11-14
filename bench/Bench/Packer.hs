{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Packer where


import           Bench.Types

import qualified Data.Packer as P
import           Data.ByteString (ByteString)


{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord w8 w16 w32 w64) =
    P.runPacking (1 + 2 + 4 + 8)  $ do
        P.putWord8 w8
        P.putWord16LE w16
        P.putWord32BE w32
        P.putWord64LE w64


{-# NOINLINE getBenchWord #-}
getBenchWord :: ByteString -> BenchWord
getBenchWord bs =
    P.runUnpacking getter bs
  where
    getter = BenchWord
        <$> P.getWord8
        <*> P.getWord16LE
        <*> P.getWord32BE
        <*> P.getWord64LE

{-# NOINLINE sanityBenchWord #-}
sanityBenchWord :: BenchWord -> BenchWord
sanityBenchWord = getBenchWord . putBenchWord
