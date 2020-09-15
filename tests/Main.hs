module Main where

import qualified Data.List as List
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Test.SmallCheck.Series.Instances.Internal

main :: IO ( )
main = defaultMain $ testGroup "Verify `sets`."
  [ testGroup "An element is present in a set at most once"
    [ testProperty "Int" $ over sets $ \xs -> (xs :: [Int]) == List.nub xs
    , testProperty "Bool" $ over sets $ \xs -> (xs :: [Bool]) == List.nub xs
    ]
  , testProperty "All possible sets within depth limit are generated (for a large enough type)"
    $ \(NonNegative n) -> length (list n sets :: [[Int]]) == 2^n
  , testCase "All 4 sets of depth 2 are generated"
    $ assertEqual "" [[ ], [0], [1], [0, 1]] (list 2 sets :: [[Int]])
  ]
