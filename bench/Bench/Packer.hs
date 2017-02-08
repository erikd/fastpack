{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Packer where


import           Bench.Types

import qualified Data.Packer as P
import           Data.ByteString (ByteString)


{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord le64 be64 le32 be32 le16 be16 w8a w8b) =
    P.runPacking benchWordSize $ do
        P.putWord64LE le64
        P.putWord64BE be64
        P.putWord32LE le32
        P.putWord32BE be32
        P.putWord16LE le16
        P.putWord16BE be16
        P.putWord8 w8a
        P.putWord8 w8b


{-# NOINLINE getBenchWord #-}
getBenchWord :: ByteString -> BenchWord
getBenchWord bs =
    P.runUnpacking getter bs
  where
    getter = BenchWord
        <$> P.getWord64LE
        <*> P.getWord64BE
        <*> P.getWord32LE
        <*> P.getWord32BE
        <*> P.getWord16LE
        <*> P.getWord16BE
        <*> P.getWord8
        <*> P.getWord8

{-# NOINLINE sanityBenchWord #-}
sanityBenchWord :: BenchWord -> BenchWord
sanityBenchWord = getBenchWord . putBenchWord
