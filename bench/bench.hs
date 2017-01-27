{-# LANGUAGE DataKinds, KindSignatures, RankNTypes, TypeApplications #-}

import qualified Bench.Binary as Binary
import qualified Bench.Cereal as Cereal
import           Bench.Data (genBenchData)
import qualified Bench.FastPack as FastPack
import qualified Bench.Packer as Packer
import           Bench.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as DL
import           Data.Proxy (Proxy(..))

import TestBench

main :: IO ()
main =
  testBench $ do
    compareFunc "Write to ByteString"
                (`withLibrary` (\l -> putBenchTest (putBenchWord l) bws))
                (uncurry baseline (head libraries))
                (mapM_ (uncurry comp) (tail libraries))
    compareFunc "Read from ByteString"
               (`withLibrary` (\l -> getBenchTest (getBenchWord l) bss))
               (uncurry baseline (head libraries))
               (mapM_ (uncurry comp) (tail libraries))
  where
    (bws, bss) = genBenchData 100000

--------------------------------------------------------------------------------
-- The benchmarks.

data Library = Binary
             | Cereal
             | Packer
             | FastPack
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

libraries :: [(String, Library)]
libraries = map ((,) =<< show) [minBound .. maxBound]

withLibrary :: Library -> (forall l. (PutGet l) => Proxy l -> k) -> k
withLibrary Binary   k = (k (Proxy @'Binary))
withLibrary Cereal   k = (k (Proxy @'Cereal))
withLibrary Packer   k = (k (Proxy @'Packer))
withLibrary FastPack k = (k (Proxy @'FastPack))

class PutGet (l :: Library) where

  putBenchWord :: Proxy l -> BenchWord -> ByteString

  getBenchWord :: Proxy l -> ByteString -> BenchWord

instance PutGet 'Binary where
  putBenchWord _ = Binary.putBenchWord
  getBenchWord _ = Binary.getBenchWord

instance PutGet 'Cereal where
  putBenchWord _ = Cereal.putBenchWord
  getBenchWord _ = Cereal.getBenchWord

instance PutGet 'Packer where
  putBenchWord _ = Packer.putBenchWord
  getBenchWord _ = Packer.getBenchWord

instance PutGet 'FastPack where
  putBenchWord _ = FastPack.putBenchWord
  getBenchWord _ = FastPack.getBenchWord

{-# NOINLINE sanityBenchWord #-}
sanityBenchWord :: (PutGet l) => Proxy l -> BenchWord -> BenchWord
sanityBenchWord p = getBenchWord p . putBenchWord p

putBenchTest :: (BenchWord -> ByteString) -> [BenchWord] -> Int
putBenchTest put = DL.foldl' (\ acc bw -> acc + BS.length (put bw)) 0

getBenchTest :: (ByteString -> BenchWord) -> [ByteString] -> Int
getBenchTest get = DL.foldl' (\ acc bs -> acc + fromIntegral (getThird $ get bs)) 0
