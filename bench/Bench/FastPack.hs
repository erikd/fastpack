{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.FastPack where


import           Bench.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI

import           Data.FastPack
import qualified Data.FastPack.Functions

import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (castPtr, plusPtr)
import           Foreign.Storable (Storable (..))

import           System.IO.Unsafe (unsafeDupablePerformIO)




{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord w8 w16 w32 w64) =
    $(runFastPack
        [ PackNumVar "w8" PackW8
        , PackNumVar "w16" (PackW16 LE)
        , PackNumVar "w32" (PackW32 LE)
        , PackNumVar "w64" (PackW64 LE)
        ])




-- This version is just for benchmarking the idea.
-- The code being benchmarked here is what FastPack should generate from an
-- EDSL/TemplateHaskell/whatever.

{-# NOINLINE getBenchWord #-}
getBenchWord :: ByteString -> BenchWord
getBenchWord (BSI.PS fp offset len)
    | len < 15 = error "FastPack.getBenchWord"
    | otherwise =
        unsafeDupablePerformIO $ withForeignPtr fp $ \ srcptr ->
            let ptr = plusPtr srcptr offset in
            BenchWord
                <$> peek (castPtr ptr)
                <*> peek (castPtr (plusPtr ptr 1))
                <*> peek (castPtr (plusPtr ptr 3))
                <*> peek (castPtr (plusPtr ptr 7))

{-# NOINLINE sanityBenchWord #-}
sanityBenchWord :: BenchWord -> BenchWord
sanityBenchWord = getBenchWord . putBenchWord
