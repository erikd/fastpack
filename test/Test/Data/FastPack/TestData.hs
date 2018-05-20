{-# LANGUAGE TemplateHaskell #-}
module Test.Data.FastPack.TestData
  ( TestData (..)
  , genTestData
  , packTestData
  , unpackTestData
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.FastPack
import qualified Data.FastPack.Functions
import           Data.Word (Word8, Word16, Word32, Word64)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Numeric (showHex)


data TestData
  = TestLE Word8 Word16 Word32 Word64
  | TestBE Word64 Word32 Word16 Word8
  | TestSimpleBS ByteString Word32      -- The ByteString is of known length
  deriving (Eq, Show)


genTestData :: Gen TestData
genTestData =
  Gen.choice
    [ TestLE <$> Gen.word8 Range.constantBounded <*> Gen.word16 Range.constantBounded
        <*> Gen.word32 Range.constantBounded <*> Gen.word64 Range.constantBounded
    , TestBE <$> Gen.word64 Range.constantBounded <*> Gen.word32 Range.constantBounded
        <*> Gen.word16 Range.constantBounded <*> Gen.word8 Range.constantBounded
    , TestSimpleBS <$> Gen.bytes (Range.singleton 100) <*> Gen.word32 Range.constantBounded
    ]


packTestData :: TestData -> ByteString
packTestData tp =
  case tp of
    TestLE w8 w16 w32 w64 ->
      $(runFastPack
        [ PackNumVar "testPack1" (PackW16 LE)
        , PackNumVar "w8" PackW8
        , PackNumVar "w16" (PackW16 LE)
        , PackNumVar "w32" (PackW32 LE)
        , PackNumVar "w64" (PackW64 LE)
        ])
    TestBE w64 w32 w16 w8 ->
      $(runFastPack
        [ PackNumVar "testPack2" (PackW16 LE)
        , PackNumVar "w64" (PackW64 BE)
        , PackNumVar "w32" (PackW32 BE)
        , PackNumVar "w16" (PackW16 BE)
        , PackNumVar "w8" PackW8
        ])
    TestSimpleBS bs w32 ->
      $(runFastPack
        [ PackNumVar "testPack3" (PackW16 LE)
        , PackBsVar "bs"
        , PackNumVar "w32" (PackW32 LE)
        ])

unpackTestData :: ByteString -> Either String TestData
unpackTestData bs =
  case marker of
    Right 0xcafe -> unpackTestLE rest
    Right 0xbeef -> unpackTestBE rest
    Right 0xc001 -> unpackTestSimpleBS rest
    Right x -> Left $ "unpackTestData: Bad marker 0x" ++ showHex x "."
    Left er -> Left er
  where
    (markerBS, rest) =
      BS.splitAt 2 bs

    marker :: Either String Word16
    marker =
      $(runFastUnpack "markerBS" "id" [ GetNum (PackW16 LE) ])

-- -------------------------------------------------------------------------------------------------

testPack1, testPack2, testPack3 :: Word16
testPack1 = 0xcafe
testPack2 = 0xbeef
testPack3 = 0xc001


unpackTestLE :: ByteString -> Either String TestData
unpackTestLE bs =
  $(runFastUnpack "bs" "Test.Data.FastPack.TestData.TestLE"
    [ GetNum PackW8
    , GetNum (PackW16 LE)
    , GetNum (PackW32 LE)
    , GetNum (PackW64 LE)
    ])

unpackTestBE :: ByteString -> Either String TestData
unpackTestBE bs =
  $(runFastUnpack "bs" "Test.Data.FastPack.TestData.TestBE"
    [ GetNum (PackW64 BE)
    , GetNum (PackW32 BE)
    , GetNum (PackW16 BE)
    , GetNum PackW8
    ])

unpackTestSimpleBS :: ByteString -> Either String TestData
unpackTestSimpleBS bs =
  $(runFastUnpack "bs" "Test.Data.FastPack.TestData.TestSimpleBS"
    [ GetBs 100   -- Known length
    , GetNum (PackW32 LE)
    ])
