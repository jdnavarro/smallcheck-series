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

import Data.Char (ord)
import Data.List (inits)
import Data.Word (Word8)
import Data.ByteString.Lazy (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as L8 (pack)
import Test.SmallCheck.Series

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing with an extra
--   /byte/ representing the 'a' 'Char' in @ASCII@.
--
-- >>> list 4 aaa
-- ["","a","aa","aaa","aaaa"]
--
-- Use this when you don't care about the /byte/ inside 'Data.ByteString.Lazy.ByteString'.
aaa :: Series m ByteString
aaa = replicated . fromIntegral $ ord 'a'

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing with an extra @NUL@ byte.
--
-- >>> list 4 zzz
-- ["","\NUL","\NUL\NUL","\NUL\NUL\NUL","\NUL\NUL\NUL\NUL"]
zzz :: Series m ByteString
zzz = replicated 0

replicated :: Word8 -> Series m ByteString
replicated c =
    generate $ \d -> fmap pack . inits $ (replicate . fromIntegral) d c

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing with an extra custom byte.
--
-- >>> list 4 . replicated . fromIntegral $ ord '@'
-- ["","@","@@","@@@","@@@@"]

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing with the @ASCII@
--   representation of the alphabet.
--
-- >>> list 4 alpha
-- ["","a","ab","abc","abcd"]
alpha :: Series m ByteString
alpha = enumerated $ fmap (fromIntegral . ord) ['a'..'z']

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing by counting bytes.
--
-- >>> list 4 ascii
-- ["","\NUL","\NUL\SOH","\NUL\SOH\STX","\NUL\SOH\STX\ETX"]
ascii :: Series m ByteString
ascii = enumerated [0..255]

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' growing with the given byte set.
--
-- >>> list 4 . enumerated $ fmap (fromIntegral . ord) "abc"
-- ["","a","ab","abc"]
enumerated  :: [Word8] -> Series m ByteString
enumerated cs = generate $ \d -> fmap pack . inits $ take d cs

-- | Create a 'Data.ByteString.Lazy.ByteString' 'Series' with a dummy @ASCII@ sentence.
--   This can be used when you want to print a 'Series' to the screen.
--
-- >>> let s = list 20 jack
-- >>> take 3 s
-- ["","All","All work"]
-- >>> last s
-- "All work and no play makes Jack a dull boy. All work and no play makes Jack a dull boy."
jack :: Series m ByteString
jack = generate $ \d ->
    fmap (L8.pack . unwords) . inits . take d . cycle . words $
        "All work and no play makes Jack a dull boy."
