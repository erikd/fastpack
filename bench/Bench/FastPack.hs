{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.FastPack where


import           Bench.Types

import           Data.ByteString (ByteString)

import           Data.FastPack
import           Data.FastPack.Functions


{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord le64 be64 le32 be32 le16 be16 w8a w8b) =
    $(runFastPack
        [ PackNumVar "le64" (PackW64 LE)
        , PackNumVar "be64" (PackW64 BE)
        , PackNumVar "le32" (PackW32 LE)
        , PackNumVar "be32" (PackW32 BE)
        , PackNumVar "le16" (PackW16 LE)
        , PackNumVar "be16" (PackW16 BE)
        , PackNumVar "w8a" PackW8
        , PackNumVar "w8b" PackW8
        ])



{-# INLINE getBenchWord #-}
getBenchWord :: ByteString -> BenchWord
getBenchWord bs =
    -- For benchmarking only, to make the type signature of the FastPack version
    -- the same as the Binary version.
    case getBenchWordRaw bs of
        Left err -> error $ show err
        Right bw -> bw

{-# INLINE getBenchWordRaw #-}
getBenchWordRaw :: ByteString -> Either String BenchWord
getBenchWordRaw bs =
    $(runFastUnpack "bs" "Bench.Types.BenchWord"
        [ GetNum (PackW64 LE)
        , GetNum (PackW64 BE)
        , GetNum (PackW32 LE)
        , GetNum (PackW32 BE)
        , GetNum (PackW16 LE)
        , GetNum (PackW16 BE)
        , GetNum PackW8
        , GetNum PackW8
        ])
