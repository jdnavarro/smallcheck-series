{-|
Following the convention from "Data.ByteString.Lazy", this module is intended to be
imported @qualified@. For example:

> import qualified Test.SmallCheck.Series.ByteString.Lazy as L.Series
-}
module Test.SmallCheck.Series.ByteString.Lazy
  (
  -- * Replication
    aaa
  , zzz
  , replicated
  -- * Enumeration
  , ascii
  , alpha
  , enumerated
  -- * Printing
  , jack
  ) where

import Prelude hiding (replicate)
import Control.Applicative ((<$>))
import Data.ByteString.Lazy.Char8 (ByteString, pack, replicate)
import Test.SmallCheck.Series

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing with an extra
--   /byte/ representing the 'a' 'Char' in @ASCII@.
--
-- >>> list 4 aaa
-- ["","a","aa","aaa","aaaa"]
--
-- Use this when you don't care about the /byte/ inside 'Data.ByteString.Lazy.ByteString'.
aaa :: Series m ByteString
aaa = replicated 'a'

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing with an extra @NUL@ byte.
--
-- >>> list 4 zzz
-- ["","\NUL","\NUL\NUL","\NUL\NUL\NUL","\NUL\NUL\NUL\NUL"]
zzz :: Series m ByteString
zzz = replicated '\0'

replicated :: Char -> Series m ByteString
replicated c = generate $ \d -> (`rep` c) <$> [0..d]
  where
    rep = replicate . fromIntegral

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing with an extra custom byte.
--
-- >>> list 4 $ replicated '@'
-- ["","@","@@","@@@","@@@@"]

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing with the @ASCII@
--   representation of the alphabet.
--
-- >>> list 4 alpha
-- ["","a","ab","abc","abcd"]
alpha :: Series m ByteString
alpha = enumerated ['a'..'z']

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing by counting bytes.
--
-- >>> list 4 ascii
-- ["","\NUL","\NUL\SOH","\NUL\SOH\STX","\NUL\SOH\STX\ETX"]
ascii :: Series m ByteString
ascii = enumerated ['\0'..'\255']

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing with the given byte set.
--
-- >>> list 4 $ enumerated "abc"
-- ["","a","ab","abc","abc"]
enumerated  :: String -> Series m ByteString
enumerated cs = generate $ \d -> (\n -> pack $ take n cs) <$> [0..d]

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' with a dummy @ASCII@ sentence.
--   This can be used when you want to print a 'Series' to the screen.
--
-- >>> let s = list 20 jack
-- >>> take 3 s
-- ["","All","All work"]
-- >>> s !! 10
-- "All work and no play makes Jack a dull boy"
jack :: Series m ByteString
jack = generate $ \d ->
    (\n -> pack . unwords . take n . words $ sentence) <$> [0..d]
  where
    sentence = "All work and no play makes Jack a dull boy"
