{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.FastPack where


import           Bench.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI

import           Data.FastPack
import           Data.FastPack.Functions

import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (castPtr, plusPtr)
import           Foreign.Storable (Storable (..))

import           System.IO.Unsafe (unsafeDupablePerformIO)




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



-- This version is just for benchmarking the idea.
-- The code being benchmarked here is what FastPack should generate from an
-- EDSL/TemplateHaskell/whatever.

{-# NOINLINE getBenchWord #-}
getBenchWord :: ByteString -> BenchWord
getBenchWord (BSI.PS fp offset len)
    | len < benchWordSize = error "FastPack.getBenchWord"
    | otherwise =
        unsafeDupablePerformIO $ withForeignPtr fp $ \ srcptr ->
            let ptr = plusPtr srcptr offset in
            BenchWord
                <$> peek (castPtr ptr)
                <*> fmap bswapW64 (peek (castPtr (plusPtr ptr 8)))
                <*> peek (castPtr (plusPtr ptr 16))
                <*> fmap bswapW32 (peek (castPtr (plusPtr ptr 20)))
                <*> peek (castPtr (plusPtr ptr 24))
                <*> fmap bswapW16 (peek (castPtr (plusPtr ptr 26)))
                <*> peek (castPtr (plusPtr ptr 28))
                <*> peek (castPtr (plusPtr ptr 29))


{-# NOINLINE sanityBenchWord #-}
sanityBenchWord :: BenchWord -> BenchWord
sanityBenchWord = getBenchWord . putBenchWord
