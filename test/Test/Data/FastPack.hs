{-# LANGUAGE TemplateHaskell #-}
module Test.Data.FastPack
  ( tests
  ) where

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Test.Data.FastPack.TestData

prop_roundtrip :: Property
prop_roundtrip =
  H.withTests 20000 . H.property $ do
    cmd <- H.forAll genTestData
    H.tripping cmd packTestData unpackTestData

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
