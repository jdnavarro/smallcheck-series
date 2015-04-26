{-# LANGUAGE OverloadedStrings #-}
module Test.SmallCheck.Series.Text where

import Prelude hiding (replicate)
import Data.Text (Text, replicate)
import Test.SmallCheck.Series

simple :: Monad m => Series m Text
simple = generate $ \d -> (`replicate` "a") <$> [0..d]
