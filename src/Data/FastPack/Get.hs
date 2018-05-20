{-# LANGUAGE OverloadedStrings #-}

module Data.FastPack.Get
    ( GetData (..)
    , runFastUnpack
    ) where

import           Data.FastPack.TH
import           Data.FastPack.Types

import qualified Data.List as DL

import           GHC.Err (error)

import           Language.Haskell.TH.Syntax


data GetData
    = GetNum PackNumType
    | GetBs Int
    deriving (Eq, Show)



runFastUnpack :: String -> String -> [GetData] -> Q Exp
runFastUnpack inputBS ctor xs = do
    let (len, exs) = DL.mapAccumL (peekGetData "ptr") 0 xs
    let tailexpr = construct ctor exs
    pure $ LetE [ValD (TupP (DL.map mkVarP ["fp", "offset", "len"]))
        (NormalB (AppE (mkVarE "Data.FastPack.Functions.bsInternals") (mkVarE inputBS))) []]
        (CondE (InfixE (Just (mkVarE "len")) (mkVarE "Data.FastPack.Functions.numLess") (Just (mkLitIntE len)))
        (AppE (mkVarE "Data.FastPack.Functions.left") (mkLitStrE "FastPack.getBenchWord"))
        (AppE (mkVarE "Data.FastPack.Functions.right") (AppE (mkVarE "Data.FastPack.Functions.unsafeDupablePerformIO")
            (AppE (AppE (mkVarE "Data.FastPack.Functions.withForeignPtr") (mkVarE "fp"))
                (LamE [mkVarP "srcptr"]
                (LetE [ValD (mkVarP "ptr") (NormalB (AppE (AppE plusPtrE (mkVarE "srcptr"))
                (mkVarE "offset"))) []] tailexpr))))))


construct :: String -> [Exp] -> Exp
construct _ [] = error "construct"
construct ctor [x] = AppE (AppE fmapE (mkVarE ctor)) x
construct ctor (x:xs) =
    let outer = AppE (AppE fmapE (mkConE ctor)) x in
    mkInner outer xs
  where
    mkInner e [] = e
    mkInner e (y:ys) =
        let outer = AppE (AppE apE e) y in
        mkInner outer ys



peekGetData :: String -> Int -> GetData -> (Int, Exp)
peekGetData pname offset getdata =
    case getdata of
        GetNum t -> (offset + sizePackNumType t, fmapEndian t (peekPtrE pname offset))
        GetBs len -> (offset + len, peekByteStringE pname offset len)


fmapEndian :: PackNumType -> Exp -> Exp
fmapEndian t expr =
    case t of
        PackW8 -> expr
        PackW16 end -> endian end "Data.FastPack.Functions.bswapW16"
        PackW32 end -> endian end "Data.FastPack.Functions.bswapW32"
        PackW64 end -> endian end "Data.FastPack.Functions.bswapW64"
  where
    endian end fname
        | end == systemEndianness = expr
        | otherwise = AppE (AppE  fmapE (mkVarE fname)) expr



sizePackNumType :: PackNumType -> Int
sizePackNumType t =
    case t of
        PackW8 {}-> 1
        PackW16 {} -> 2
        PackW32 {} -> 4
        PackW64 {} -> 8


peekByteStringE :: String -> Int -> Int -> Exp
peekByteStringE pname offset len =
    AppE (AppE (AppE funcE (mkVarE pname)) (mkLitIntE offset)) (mkLitIntE len)
  where
    funcE = mkVarE "Data.FastPack.Functions.peekByteString"


peekPtrE :: String -> Int -> Exp
peekPtrE ptr offset =
    AppE peekE (AppE castPtrE target)
  where
    target
        | offset <= 0 = mkVarE ptr
        | otherwise = AppE (AppE plusPtrE (mkVarE ptr)) (mkLitIntE offset)


peekE :: Exp
peekE = mkVarE "Data.FastPack.Functions.peek"

castPtrE :: Exp
castPtrE = mkVarE "Data.FastPack.Functions.castPtr"

plusPtrE :: Exp
plusPtrE = mkVarE "Data.FastPack.Functions.plusPtr"

fmapE :: Exp
fmapE = mkVarE "Data.FastPack.Functions.fmap"

apE :: Exp
apE = mkVarE "Data.FastPack.Functions.ap"
