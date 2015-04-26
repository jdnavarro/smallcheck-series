module Test.SmallCheck.Series.ByteString where

import Prelude hiding (replicate)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (replicate)
import Test.SmallCheck.Series

simple :: Monad m => Series m ByteString
simple = generate $ \d -> (`replicate` 'a') <$> [0..d]
