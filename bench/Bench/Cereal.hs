{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Cereal where


import           Bench.Types

import qualified Data.Serialize.Put as CP
import qualified Data.Serialize.Get as CG
import           Data.ByteString (ByteString)


{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord w8 w16 w32 w64) =
    CP.runPut $ do
        CP.putWord8 w8
        CP.putWord16le w16
        CP.putWord32be w32
        CP.putWord64le w64

{-# NOINLINE getBenchWord #-}
getBenchWord :: ByteString -> BenchWord
getBenchWord bs =
    -- For benchmarking only, to make the type signature of the Cereal version
    -- the same as the Binary version.
    case CG.runGet getter bs of
        Left err -> error err
        Right bw -> bw
  where
    getter = BenchWord
        <$> CG.getWord8
        <*> CG.getWord16le
        <*> CG.getWord32be
        <*> CG.getWord64le

{-# NOINLINE sanityBenchWord #-}
sanityBenchWord :: BenchWord -> BenchWord
sanityBenchWord = getBenchWord . putBenchWord
