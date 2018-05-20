
import           Control.Monad (unless)

import           System.Exit (exitFailure)

import qualified Test.Data.FastPack

main :: IO ()
main = runTests
  [ Test.Data.FastPack.tests
  ]

runTests :: [IO Bool] -> IO ()
runTests tests = do
  result <- and <$> sequence tests
  unless result
    exitFailure
