{-# LANGUAGE CPP #-}

module Data.FastPack.TH
    ( mkConE
    , mkConP
    , mkLitIntE
    , mkLitStrE
    , mkVarE
    , mkVarP
    , systemEndianness
    ) where

import           Data.FastPack.Types

import           Language.Haskell.TH.Syntax


mkConE :: String -> Exp
mkConE = ConE . mkName

mkLitIntE :: Int -> Exp
mkLitIntE = LitE . IntegerL . fromIntegral

mkLitStrE :: String -> Exp
mkLitStrE = LitE . StringL

mkVarE :: String -> Exp
mkVarE = VarE . mkName

mkConP :: String -> [Pat] -> Pat
mkConP name = ConP (mkName name)

mkVarP :: String -> Pat
mkVarP = VarP . mkName


systemEndianness :: Endian
#ifdef WORDS_BIGENDIAN
systemEndianness = BE
#else
systemEndianness = LE
#endif
