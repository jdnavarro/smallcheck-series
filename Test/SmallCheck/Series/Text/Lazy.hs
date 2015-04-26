{-# LANGUAGE OverloadedStrings #-}
module Test.SmallCheck.Series.Text.Lazy where

import Prelude hiding (replicate)
import Data.Text.Lazy (Text, replicate)
import Test.SmallCheck.Series

simple :: Monad m => Series m Text
simple = generate $ \d -> (`rep` "a") <$> [0..d]
  where
    rep = replicate . fromIntegral
