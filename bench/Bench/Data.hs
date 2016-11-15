{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Data where

import           Bench.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Data.Char (chr)


genBenchData :: Int -> ([BenchWord], [ByteString])
genBenchData count =
    unzip $ map (\ x -> (mkBenchWord x, mkBenchBS x)) [ 1 .. count ]
  where
    mkBenchWord i =
        BenchWord (fromIntegral $ 137 * i) (fromIntegral $ 231 * i)
                    (fromIntegral $ 51 * i) (fromIntegral $ 71 * i)
                    (fromIntegral $ 3 * i) (fromIntegral $ 5 * i)
                    (fromIntegral i) (fromIntegral $ i + 1)

    -- Number (30 in this case) needs to be the same as the packed size.
    mkBenchBS i = BS.replicate benchWordSize (chr i)
