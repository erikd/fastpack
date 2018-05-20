
module Data.FastPack.Put
    ( runFastPack
    ) where

import qualified Data.ByteString.Char8 as BS

import           Data.FastPack.TH
import           Data.FastPack.Types

import           Data.Either (partitionEithers)
import qualified Data.List as DL
import           Language.Haskell.TH.Syntax


runFastPack :: [PackData] -> Q Exp
runFastPack xs = do
    pure $ LetE [ValD (VarP $ mkName "size") (NormalB calcSize) []]
            (AppE (AppE (mkVarE "Data.FastPack.Functions.bsUnsafeCreate") (mkVarE "size")) (builder 0 xs))
  where
    calcSize = packDataSize xs


-- -----------------------------------------------------------------------------

packDataSize :: [PackData] -> Exp
packDataSize pds =
    let (ls, rs) = partitionEithers $ map packSize pds in
    mkSizeExp (DL.foldl' (+) 0 ls) rs
  where
    packSize pd =
        case pd of
            PackNumVar _ t -> Left $ sizePackNumType t
            PackNumLit _ t -> Left $ sizePackNumType t
            PackBsLit bs -> Left $ BS.length bs
            PackBsVar s -> Right $ AppE (mkVarE "Data.FastPack.Functions.bsLength") (mkVarE s)
    sizePackNumType t =
        case t of
            PackW8 {}-> 1
            PackW16 {} -> 2
            PackW32 {} -> 4
            PackW64 {} -> 8

    mkSizeExp :: Int -> [Exp] -> Exp
    mkSizeExp known [] = mkLitIntE $ fromIntegral known
    mkSizeExp known xs =
        InfixE (Just . mkLitIntE $ fromIntegral known) numPlus $ sumExp xs

    sumExp [] = Nothing
    sumExp [x] = Just x
    sumExp (x:xs) = Just $ InfixE (Just x) numPlus (sumExp xs)

    numPlus = mkVarE "Data.FastPack.Functions.numPlus"


builder :: Int -> [PackData] -> Exp
builder _ [] = LamE [WildP] (AppE (mkVarE "Data.FastPack.Functions.pure") (mkVarE "Data.FastPack.Functions.unit"))
builder n (x:xs) =
    let ptr = mkName $ "ptr" ++ show n in
    LamE [VarP ptr]
        (InfixE (Just (pokePackData ptr x))
                (mkVarE "Data.FastPack.Functions.>>=") (Just (builder (n + 1) xs)))

pokePackData :: Name -> PackData -> Exp
pokePackData ptr pd =
    AppE (AppE poker (VarE ptr)) (valueToExp pd)
  where
    poker =
        case pd of
            PackNumVar _ t -> pokeType t
            PackNumLit _ t -> pokeType t
            PackBsLit _ -> mkVarE "Data.FastPack.Functions.pokeAdvanceBS"
            PackBsVar _ -> mkVarE "Data.FastPack.Functions.pokeAdvanceBS"
    pokeType t =
        case t of
            PackW8 -> mkVarE "Data.FastPack.Functions.pokeAdvanceW8"
            PackW16 {} -> mkVarE "Data.FastPack.Functions.pokeAdvanceW16"
            PackW32 {} -> mkVarE "Data.FastPack.Functions.pokeAdvanceW32"
            PackW64 {} -> mkVarE "Data.FastPack.Functions.pokeAdvanceW64"


valueToExp :: PackData -> Exp
valueToExp pd =
    case pd of
        PackNumVar s t -> wrapEndian t $ mkVarE s
        PackNumLit i t -> wrapEndian t $ LitE (IntegerL i)
        PackBsLit bs -> LitE (StringL $ BS.unpack bs)
        PackBsVar s -> mkVarE s


wrapEndian :: PackNumType -> Exp -> Exp
wrapEndian t expr =
    case t of
        PackW8 -> expr
        PackW16 end -> endian end "Data.FastPack.Functions.bswapW16"
        PackW32 end -> endian end "Data.FastPack.Functions.bswapW32"
        PackW64 end -> endian end "Data.FastPack.Functions.bswapW64"
  where
    endian end fname
        | end == systemEndianness = expr
        | otherwise = AppE (mkVarE fname) expr
