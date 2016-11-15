{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Binary where


import           Bench.Types

import qualified Data.Binary.Put as BP
import qualified Data.Binary.Get as BG
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS

{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord le64 be64 le32 be32 le16 be16 w8a w8b) =
    LBS.toStrict . BP.runPut $ do
        BP.putWord64le le64
        BP.putWord64be be64
        BP.putWord32le le32
        BP.putWord32be be32
        BP.putWord16le le16
        BP.putWord16be be16
        BP.putWord8 w8a
        BP.putWord8 w8b


{-# NOINLINE getBenchWord #-}
getBenchWord :: ByteString -> BenchWord
getBenchWord bs =
    BG.runGet getter $ LBS.fromStrict bs
  where
    getter = BenchWord
        <$> BG.getWord64le
        <*> BG.getWord64be
        <*> BG.getWord32le
        <*> BG.getWord32be
        <*> BG.getWord16le
        <*> BG.getWord16be
        <*> BG.getWord8
        <*> BG.getWord8

{-# NOINLINE sanityBenchWord #-}
sanityBenchWord :: BenchWord -> BenchWord
sanityBenchWord = getBenchWord . putBenchWord
