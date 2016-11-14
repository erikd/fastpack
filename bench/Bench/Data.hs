{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.Data where

import           Bench.Types (BenchWord (..))

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Data.Char (chr)


genBenchData :: Int -> ([BenchWord], [ByteString])
genBenchData count =
    unzip $ map (\ x -> (mkBenchWord x, mkBenchBS x)) [ 1 .. count ]
  where
    mkBenchWord i =
        BenchWord (fromIntegral i) (fromIntegral $ 3 * i)
                (fromIntegral $ 71 * i) (fromIntegral $ 137 * i)

    mkBenchBS i = BS.replicate 15 (chr i)
