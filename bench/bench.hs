
import qualified Bench.Binary as Binary
import qualified Bench.Cereal as Cereal
import           Bench.Data (genBenchWord)
import qualified Bench.FastPack as FastPack
import qualified Bench.Packer as Packer
import           Bench.Types (BenchWord)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as DL

import qualified Criterion.Main as C


main :: IO ()
main = C.defaultMain benchmarks


--------------------------------------------------------------------------------
-- The benchmarks.

benchmarks :: [C.Benchmark]
benchmarks =
    let xs = genBenchWord 100000 in
    [ C.bench "Binary"      $ C.whnf (benchTest Binary.putBenchWord) xs
    , C.bench "Cereal"      $ C.whnf (benchTest Cereal.putBenchWord) xs
    , C.bench "Packer"      $ C.whnf (benchTest Packer.putBenchWord) xs
    , C.bench "FastPack"    $ C.whnf (benchTest FastPack.putBenchWord) xs
    ]


benchTest :: (BenchWord -> ByteString) -> [BenchWord] -> Int
benchTest put = DL.foldl' (\ acc bw -> acc + BS.length (put bw)) 0
