{-# LANGUAGE CPP #-}

module Data.FastPack
    ( Endian (..)
    , GetData (..)
    , PackData (..)
    , PackNumType (..)
    , runFastPack
    , runFastUnpack
    ) where

import           Data.FastPack.Get
import           Data.FastPack.Put
import           Data.FastPack.Types


