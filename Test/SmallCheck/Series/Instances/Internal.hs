{-# language FlexibleContexts #-}
{-# language CPP #-}

module Test.SmallCheck.Series.Instances.Internal where

import Test.SmallCheck.Series
import Data.Functor.Identity
#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

sets :: (Ord a, Serial m a, Serial Identity a) => Series m [a]
sets = do
  depth <- getDepth
  let xs = list depth series  -- We are going to generate all subsets of this enumeration.
  i <- localDepth (const (2^min depth (length xs) - 1)) series
    -- This complicated expression ensures that a number is chosen from just a
    -- large enough subset of `Word`.
    -- - For finite types, the enumeration `xs` can inherently be no longer than
    --     their cardinality.
    -- - For infinite types, we make sure that `i` has at most `depth` binary
    --     digits.
    -- For the depth of −1 the series for `Word` is empty, and for depth n > 0
    -- it equals {0… n}. So, we adjust depth by −1 to make sure there are
    -- exactly `depth` elements: |{0… n−1}| = n.

  let pattern = binaryExpansion i
      ys = snd . unzip . filter fst . zip pattern $ xs
  return ys

binaryExpansion :: Word -> [Bool]
binaryExpansion 0 = [ ]
binaryExpansion i = ((i `mod` 2) == 1): binaryExpansion (i `div` 2)
