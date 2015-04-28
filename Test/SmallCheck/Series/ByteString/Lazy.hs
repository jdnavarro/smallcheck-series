{-|
Following the convention from "Data.ByteString.Lazy", this module is intended to be
imported @qualified@. For example:

> import qualified Test.SmallCheck.Series.ByteString.Lazy as L.Series
-}
module Test.SmallCheck.Series.ByteString.Lazy
  (
  -- * Replication
    replicateA
  , replicate0
  , replicateW8
  -- * Enumeration
  , enumW8s
  , enumAlphabet
  , enumList
  -- * Printing
  , jack
  ) where

import Data.Char (ord)
import Data.List (inits)
import Data.Word (Word8)
import Data.ByteString.Lazy (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as L8 (pack)
import Test.SmallCheck.Series

-- | A 'Data.ByteString.Lazy.ByteString' 'Series' that grows by replicating
--   the @97@ 'Word8', which encodes the 'a' 'Char' in @ASCII@.
--
-- >>> list 4 replicateA
-- ["","a","aa","aaa","aaaa"]
--
-- Use this when you don't care about the 'Word8' inside 'Data.ByteString.Lazy.ByteString'.
replicateA :: Series m ByteString
replicateA = replicateW8 97

-- | A 'Data.ByteString.Lazy.ByteString' 'Series' that grows by replicating
--   the @0@ 'Word8'.
--
-- >>> list 4 replicate0
-- ["","\NUL","\NUL\NUL","\NUL\NUL\NUL","\NUL\NUL\NUL\NUL"]
replicate0 :: Series m ByteString
replicate0 = replicateW8 0

-- | A 'Data.ByteString.Lazy.ByteString' 'Series' that grows by replicating
--   the given 'Word8'.
--
-- >>> list 4 $ replicateW8 64
-- ["","@","@@","@@@","@@@@"]
replicateW8 :: Word8 -> Series m ByteString
replicateW8 b =
    generate $ \d -> fmap pack . inits $ (replicate . fromIntegral) d b

-- | A 'Data.ByteString.Lazy.ByteString' 'Series' that grows by enumerating
--   every 'Word8'.
--
-- >>> list 4 enumW8s
-- ["","\NUL","\NUL\SOH","\NUL\SOH\STX","\NUL\SOH\STX\ETX"]
enumW8s :: Series m ByteString
enumW8s = enumList [0..255]

-- | A 'Data.ByteString.Lazy.ByteString' 'Series' that grows by enumerating
--   the 'Word8's which encode the latin alphabet in @ASCII@.
--
-- >>> list 4 enumAlphabet
-- ["","a","ab","abc","abcd"]
enumAlphabet :: Series m ByteString
enumAlphabet = enumList $ fmap (fromIntegral . ord) ['a'..'z']

-- | A 'Data.ByteString.ByteString' 'Series' that grows by enumerating
--   every 'Word8' in the given list.
--
-- >>> list 4 . enumList $ fmap (fromIntegral . ord) "abc"
-- ["","a","ab","abc"]
enumList  :: [Word8] -> Series m ByteString
enumList cs = generate $ \d -> fmap pack . inits $ take d cs

-- | A 'Data.ByteString.Lazy.ByteString' 'Series' that grows with @ASCII@
--   dummy English words encoded in @ASCII@.
--
--   This is useful when you want to print 'Series'.
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
