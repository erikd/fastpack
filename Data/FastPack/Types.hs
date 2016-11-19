module Data.FastPack.Types
    ( Endian (..)
    , PackData (..)
    , PackNumType (..)
    ) where

import           Data.ByteString (ByteString)


data PackData
    = PackNumVar String PackNumType
    | PackNumLit Integer PackNumType
    | PackBsLit ByteString
    | PackBsVar String
    deriving (Eq, Show)

data PackNumType
    = PackW8
    | PackW16 Endian
    | PackW32 Endian
    | PackW64 Endian
    deriving (Eq, Show)

data Endian = BE | LE deriving (Eq, Show)
