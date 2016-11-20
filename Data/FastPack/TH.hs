{-# LANGUAGE CPP #-}

module Data.FastPack.TH
    ( mkLitE
    , mkVarE
    , wrapEndian
    ) where

import           Data.FastPack.Types

import           Language.Haskell.TH.Syntax


wrapEndian :: PackNumType -> Exp -> Exp
wrapEndian t expr =
    case t of
        PackW8 -> expr
        PackW16 end -> endian end expr "Data.FastPack.Functions.bswapW16"
        PackW32 end -> endian end expr "Data.FastPack.Functions.bswapW32"
        PackW64 end -> endian end expr "Data.FastPack.Functions.bswapW64"
  where
    endian end ex n
        | end == systemEndianness = ex
        | otherwise = AppE (mkVarE n) ex


mkVarE :: String -> Exp
mkVarE = VarE . mkName

mkLitE :: Integer -> Exp
mkLitE = LitE . IntegerL

systemEndianness :: Endian
#ifdef WORDS_BIGENDIAN
systemEndianness = BE
#else
systemEndianness = LE
#endif
