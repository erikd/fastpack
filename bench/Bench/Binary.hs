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
putBenchWord (BenchWord w8 w16 w32 w64) =
    LBS.toStrict . BP.runPut $ do
        BP.putWord8 w8
        BP.putWord16le w16
        BP.putWord32be w32
        BP.putWord64le w64


{-# NOINLINE getBenchWord #-}
getBenchWord :: ByteString -> BenchWord
getBenchWord bs =
    BG.runGet getter $ LBS.fromStrict bs
  where
    getter = BenchWord
        <$> BG.getWord8
        <*> BG.getWord16le
        <*> BG.getWord32be
        <*> BG.getWord64le

{-# NOINLINE sanityBenchWord #-}
sanityBenchWord :: BenchWord -> BenchWord
sanityBenchWord = getBenchWord . putBenchWord
