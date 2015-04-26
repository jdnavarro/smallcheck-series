module Test.SmallCheck.Series.ByteString.Lazy where

import Prelude hiding (replicate)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (replicate)
import Test.SmallCheck.Series

simple :: Monad m => Series m ByteString
simple = generate $ \d -> (`rep` 'a') <$> [0..d]
  where
    rep = replicate . fromIntegral
