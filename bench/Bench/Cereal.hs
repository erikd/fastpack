{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Cereal where


import           Bench.Types

import qualified Data.Serialize.Put as CP
import qualified Data.Serialize.Get as CG
import           Data.ByteString (ByteString)


{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord le64 be64 le32 be32 le16 be16 w8a w8b) =
    CP.runPut $ do
        CP.putWord64le le64
        CP.putWord64be be64
        CP.putWord32le le32
        CP.putWord32be be32
        CP.putWord16le le16
        CP.putWord16be be16
        CP.putWord8 w8a
        CP.putWord8 w8b

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
        <$> CG.getWord64le
        <*> CG.getWord64be
        <*> CG.getWord32le
        <*> CG.getWord32be
        <*> CG.getWord16le
        <*> CG.getWord16be
        <*> CG.getWord8
        <*> CG.getWord8

{-# NOINLINE sanityBenchWord #-}
sanityBenchWord :: BenchWord -> BenchWord
sanityBenchWord = getBenchWord . putBenchWord
