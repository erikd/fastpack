{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Bench.FastPack where


import           Bench.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import           Data.Word (Word8)

import           Foreign.Storable (Storable, poke, sizeOf)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)


-- This version is just for benchmarking the idea.
-- The code being benchmarked here is what FastPack should generate from an
-- EDSL/TemplateHaskell/whatever.


{-# INLINE pokeAdvance #-}
pokeAdvance :: Storable a => Ptr Word8 -> a -> IO (Ptr Word8)
pokeAdvance ptr a = do
    poke (castPtr ptr) a
    pure (plusPtr ptr (sizeOf a))


{-# NOINLINE putBenchWord #-}
putBenchWord :: BenchWord -> ByteString
putBenchWord (BenchWord w8 w16 w32 w64) =
    BSI.unsafeCreate (sizeOf w8 + sizeOf w16 + sizeOf w32 + sizeOf w64)
    (       \ p0 -> pokeAdvance p0 w8
        >>= \ p1 -> pokeAdvance p1 w16
        >>= \ p2 -> pokeAdvance p2 w32
        >>= \ p3 -> pokeAdvance p3 w64
        >>= \ _ -> pure ()
        )
