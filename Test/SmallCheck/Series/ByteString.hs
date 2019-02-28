{-# LANGUAGE CPP #-}
{-|
Following the convention from "Data.ByteString", this module is intended to be
imported @qualified@. For example:

> import qualified Test.SmallCheck.Series.ByteString as B.Series
-}
module Test.SmallCheck.Series.ByteString
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

import Data.Word (Word8)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L (toStrict)
import Test.SmallCheck.Series

import qualified Test.SmallCheck.Series.ByteString.Lazy as L.Series

-- | A 'Data.ByteString.ByteString' 'Series' that grows by replicating
--   the @97@ 'Word8', which encodes the @\'a\'@ 'Char' in @ASCII@.
--
-- >>> list 4 replicateA
-- ["","a","aa","aaa","aaaa"]
--
-- Use this when you don't care about the 'Word8' inside 'Data.ByteString.ByteString'.
replicateA :: Series m ByteString
replicateA = L.toStrict <$> L.Series.replicateA

-- | A 'Data.ByteString.ByteString' 'Series' that grows by replicating
--   the @0@ 'Word8'.
--
-- >>> list 4 replicate0
-- ["","\NUL","\NUL\NUL","\NUL\NUL\NUL","\NUL\NUL\NUL\NUL"]
replicate0 :: Series m ByteString
replicate0 = L.toStrict <$> L.Series.replicate0

-- | A 'Data.ByteString.ByteString' 'Series' that grows by replicating
--   the given 'Word8'.
--
-- >>> list 4 $ replicateW8 64
-- ["","@","@@","@@@","@@@@"]
replicateW8 :: Word8 -> Series m ByteString
replicateW8 = fmap L.toStrict . L.Series.replicateW8

-- | A 'Data.ByteString.ByteString' 'Series' that grows by enumerating
--   every 'Word8'.
--
-- >>> list 4 enumW8s
-- ["","\NUL","\NUL\SOH","\NUL\SOH\STX","\NUL\SOH\STX\ETX"]
enumW8s :: Series m ByteString
enumW8s = L.toStrict <$> L.Series.enumW8s

-- | A 'Data.ByteString.ByteString' 'Series' that grows by enumerating
--   the 'Word8's which encode the latin alphabet in @ASCII@.
--
-- >>> list 4 enumAlphabet
-- ["","a","ab","abc","abcd"]
enumAlphabet :: Series m ByteString
enumAlphabet = L.toStrict <$> L.Series.enumAlphabet

-- | A 'Data.ByteString.ByteString' 'Series' that grows by enumerating
--   every 'Word8' in the given list.
--
-- >>> import Data.Char (ord)
-- >>> list 4 . enumList $ fmap (fromIntegral . ord) "abc"
-- ["","a","ab","abc"]
enumList  :: [Word8] -> Series m ByteString
enumList = fmap L.toStrict . L.Series.enumList

-- | A 'Data.ByteString.ByteString' 'Series' that grows with @ASCII@
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
jack = L.toStrict <$> L.Series.jack
