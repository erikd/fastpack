{-# LANGUAGE MagicHash #-}
module Data.FastPack.Functions
    ( module GB
    , ap
    , bsLength
    , bsInternals
    , bswapW16
    , bswapW32
    , bswapW64
    , bsUnsafeCreate
    , castPtr
    , fmap
    , left
    , numLess
    , numPlus
    , peek
    , plusPtr
    , pokeAdvanceBS
    , pokeAdvanceW8
    , pokeAdvanceW16
    , pokeAdvanceW32
    , pokeAdvanceW64
    , right
    , unit
    , unsafeDupablePerformIO
    , withForeignPtr
    ) where


import           Control.Monad (ap)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BSI
import           Data.Functor (fmap)

import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.Storable (Storable (..))

import qualified GHC.Base as GB hiding (ap, fmap)
import           GHC.Word (Word8, Word16 (..), Word32 (..), Word64 (..))

import           System.IO.Unsafe (unsafeDupablePerformIO)


{-# INLINE bsLength #-}
bsLength :: ByteString -> Int
bsLength = BS.length

{-# INLINE bsInternals #-}
bsInternals :: ByteString -> (ForeignPtr Word8, Int, Int)
bsInternals (BSI.PS fp offset len) = (fp, offset, len)

{-# INLINE bsUnsafeCreate #-}
bsUnsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> ByteString
bsUnsafeCreate = BSI.unsafeCreate

{-# INLINE bswapW16 #-}
bswapW16 :: Word16 -> Word16
bswapW16 (W16# w#) = W16# (GB.narrow16Word# (GB.byteSwap16# w#))

{-# INLINE bswapW32 #-}
bswapW32 :: Word32 -> Word32
bswapW32 (W32# w#) = W32# (GB.narrow32Word# (GB.byteSwap32# w#))

{-# INLINE bswapW64 #-}
bswapW64 :: Word64 -> Word64
bswapW64 (W64# w#) = W64# (GB.byteSwap64# w#)

{-# INLINE left #-}
left :: a -> Either a b
left = Left

{-# INLINE numLess #-}
numLess :: Ord a => a -> a -> Bool
numLess = (<)

{-# INLINE numPlus #-}
numPlus :: Num a => a -> a -> a
numPlus = (+)

{-# INLINE pokeAdvanceBS #-}
pokeAdvanceBS :: Ptr Word8 -> ByteString -> IO (Ptr Word8)
pokeAdvanceBS destptr (BSI.PS fp offset len) =
    withForeignPtr fp $ \ srcptr -> do
        BSI.memcpy destptr (plusPtr srcptr offset) len
        pure (plusPtr destptr len)

{-# INLINE pokeAdvanceW8 #-}
pokeAdvanceW8 :: Ptr Word8 -> Word8 -> IO (Ptr Word8)
pokeAdvanceW8 = pokeAdvance

{-# INLINE pokeAdvanceW16 #-}
pokeAdvanceW16 :: Ptr Word8 -> Word16 -> IO (Ptr Word8)
pokeAdvanceW16 = pokeAdvance

{-# INLINE pokeAdvanceW32 #-}
pokeAdvanceW32 :: Ptr Word8 -> Word32 -> IO (Ptr Word8)
pokeAdvanceW32 = pokeAdvance

{-# INLINE pokeAdvanceW64 #-}
pokeAdvanceW64 :: Ptr Word8 -> Word64 -> IO (Ptr Word8)
pokeAdvanceW64 = pokeAdvance

{-# INLINE right #-}
right :: b -> Either a b
right = Right

{-# INLINE unit #-}
unit :: ()
unit = ()

-- -----------------------------------------------------------------------------
-- We don't export this one, because we want the TH splice to only use the
-- explicitly typed versions so we can get type errors when the types are
-- wrong.
{-# INLINE pokeAdvance #-}
pokeAdvance :: Storable a => Ptr Word8 -> a -> IO (Ptr Word8)
pokeAdvance ptr a = do
    poke (castPtr ptr) a
    pure (plusPtr ptr (sizeOf a))

