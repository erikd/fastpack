{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Data where

import Bench.Types


genBenchWord :: Int -> [BenchWord]
genBenchWord count =
    map mkBenchWord [ 1 .. count ]
  where
    mkBenchWord i =
        BenchWord (fromIntegral i) (fromIntegral $ 3 * i)
                (fromIntegral $ 71 * i) (fromIntegral $ 137 * i)
