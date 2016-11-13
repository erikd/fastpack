{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.FastPack where


import           Bench.Types

import           Data.ByteString (ByteString)

import           Data.FastPack
import qualified Data.FastPack.Functions


{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord w8 w16 w32 w64) =
    $(runFastPack
        [ PackNumVar "w8" PackW8
        , PackNumVar "w16" (PackW16 LE)
        , PackNumVar "w32" (PackW32 LE)
        , PackNumVar "w64" (PackW64 LE)
        ])

