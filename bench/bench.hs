
import qualified Bench.Binary as Binary
import qualified Bench.Cereal as Cereal
import           Bench.Data (genBenchData)
import qualified Bench.FastPack as FastPack
import qualified Bench.Packer as Packer
import           Bench.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as DL

import qualified Criterion.Main as C


main :: IO ()
main = do
    sanityCheck
    putStrLn "\nPassed sanity test.\n"
    C.defaultMain benchmarks


--------------------------------------------------------------------------------
-- The benchmarks.

benchmarks :: [C.Benchmark]
benchmarks =
    let (bws, bss) = genBenchData 100000 in
    [ C.bgroup "Write to ByteString"
        [ C.bench "Binary"      $ C.whnf (putBenchTest Binary.putBenchWord) bws
        , C.bench "Cereal"      $ C.whnf (putBenchTest Cereal.putBenchWord) bws
        , C.bench "Packer"      $ C.whnf (putBenchTest Packer.putBenchWord) bws
        , C.bench "FastPack"    $ C.whnf (putBenchTest FastPack.putBenchWord) bws
        ]
    , C.bgroup "Read from ByteString"
        [ C.bench "Binary"      $ C.whnf (getBenchTest Binary.getBenchWord) bss
        , C.bench "Cereal"      $ C.whnf (getBenchTest Cereal.getBenchWord) bss
        , C.bench "Packer"      $ C.whnf (getBenchTest Packer.getBenchWord) bss
        , C.bench "FastPack"    $ C.whnf (getBenchTest FastPack.getBenchWord) bss
        ]
    ]



putBenchTest :: (BenchWord -> ByteString) -> [BenchWord] -> Int
putBenchTest put = DL.foldl' (\ acc bw -> acc + BS.length (put bw)) 0

getBenchTest :: (ByteString -> BenchWord) -> [ByteString] -> Int
getBenchTest get = DL.foldl' (\ acc bs -> acc + fromIntegral (getThird $ get bs)) 0


sanityCheck :: IO ()
sanityCheck = do
    assert "Binary" $ Binary.sanityBenchWord bw == bw
    assert "Cereal" $ Cereal.sanityBenchWord bw == bw
    assert "Packer" $ Packer.sanityBenchWord bw == bw
    assert "FastPack" $ FastPack.sanityBenchWord bw == bw
  where
    bw = BenchWord  0x123456789abcdef  0xabcdef123456789
                    0x87654321 0x76543210 0x1234 0x2345 0x55 0xaa

    assert name prop =
        if prop
            then pure ()
            else error $ name ++ " failed assertion!"
