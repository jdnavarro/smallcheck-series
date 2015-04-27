{-|
Following the convention from "Data.ByteString", this module is intended to be
imported @qualified@. For example:

> import qualified Test.SmallCheck.Series.ByteString as B.Series
-}
module Test.SmallCheck.Series.ByteString
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
import Data.Char (ord)
import Data.Word (Word8)
import Data.ByteString (ByteString, pack, replicate)
import qualified Data.ByteString.Char8 as B8 (pack)
import Test.SmallCheck.Series

-- | Create a 'Data.ByteString.ByteString' 'Series' growing with an extra
--   /byte/ representing the 'a' 'Char' in @ASCII@.
--
-- >>> list 4 aaa
-- ["","a","aa","aaa","aaaa"]
--
-- Use this when you don't care about the /byte/ inside 'Data.ByteString.ByteString'.
aaa :: Series m ByteString
aaa = replicated . fromIntegral $ ord 'a'

-- | Create a 'Data.ByteString.ByteString' 'Series' growing with an extra @NUL@ byte.
--
-- >>> list 4 zzz
-- ["","\NUL","\NUL\NUL","\NUL\NUL\NUL","\NUL\NUL\NUL\NUL"]
zzz :: Series m ByteString
zzz = replicated 0

-- | Create a 'Data.ByteString.ByteString' 'Series' growing with an extra custom byte.
--
-- >>> list 4 . replicated . fromIntegral $ ord '@'
-- ["","@","@@","@@@","@@@@"]
replicated :: Word8 -> Series m ByteString
replicated c = generate $ \d -> (`replicate` c) <$> [0..d]

-- | Create a 'Data.ByteString.ByteString' 'Series' growing with the @ASCII@
--   representation of the alphabet.
--
-- >>> list 4 alpha
-- ["","a","ab","abc","abcd"]
alpha :: Series m ByteString
alpha = enumerated $ fromIntegral . ord <$> ['a'..'z']

-- | Create a 'Data.ByteString.ByteString' 'Series' growing by counting bytes.
--
-- >>> list 4 ascii
-- ["","\NUL","\NUL\SOH","\NUL\SOH\STX","\NUL\SOH\STX\ETX"]
ascii :: Series m ByteString
ascii = enumerated [0..255]

-- | Create a 'Data.ByteString.ByteString' 'Series' growing with the given byte set.
--
-- >>> list 4 . enumerated $ fromIntegral . ord <$> "abc"
-- ["","a","ab","abc","abc"]
enumerated  :: [Word8] -> Series m ByteString
enumerated cs = generate $ \d -> (\n -> pack $ take n cs) <$> [0..d]

-- | Create a 'Data.ByteString.ByteString' 'Series' with a dummy @ASCII@ sentence.
--   This can be used when you want to print a 'Series' to the screen.
--
-- >>> let s = list 20 jack
-- >>> take 3 s
-- ["","All","All work"]
-- >>> s !! 10
-- "All work and no play makes Jack a dull boy"
jack :: Series m ByteString
jack = generate $ \d ->
    (\n -> B8.pack . unwords . take n . words $ sentence) <$> [0..d]
  where
    sentence = "All work and no play makes Jack a dull boy"
