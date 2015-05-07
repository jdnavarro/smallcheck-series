{-# LANGUAGE CPP #-}
{-|
Following the convention from "Data.Text", this module is intended to be
imported @qualified@. For example:

> import qualified Test.SmallCheck.Series.Text as T.Series
-}
module Test.SmallCheck.Series.Text
  (
  -- * Replication
    replicateA
  , replicateNull
  , replicateChar
  -- * Enumeration
  , enumAlphabet
  , enumChars
  , enumString
  -- * Printing
  , jack
  -- * Extra Unicode planes
  , enumNonBmp
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Text (Text)
import qualified Data.Text.Lazy as L (toStrict)
import Test.SmallCheck.Series

import qualified Test.SmallCheck.Series.Text.Lazy as L.Series

-- | A 'Data.Text.Text' 'Series' that grows by replicating the @a@ 'Char'.
--
-- >>> list 4 replicateA
-- ["","a","aa","aaa","aaaa"]
--
-- Use this when you don't care about the 'Char's inside 'Data.Text.Text'.
replicateA :: Series m Text
replicateA = L.toStrict <$> L.Series.replicateA

-- | A 'Data.Text.Text' 'Series' that grows by replicating the @NUL@ 'Char'.
--
-- >>> list 4 replicateNull
-- ["","\NUL","\NUL\NUL","\NUL\NUL\NUL","\NUL\NUL\NUL\NUL"]
replicateNull :: Series m Text
replicateNull = L.toStrict <$> L.Series.replicateNull

-- | A 'Data.Text.Text' 'Series' that grows by replicating the given 'Char'.
--
-- >>> list 4 $ replicateChar '☃'
-- ["","\9731","\9731\9731","\9731\9731\9731","\9731\9731\9731\9731"]
replicateChar :: Char -> Series m Text
replicateChar = fmap L.toStrict . L.Series.replicateChar

-- | A 'Data.Text.Text' 'Series' that grows by enumerating the latin alphabet.
--
-- >>> list 4 enumAlphabet
-- ["","a","ab","abc","abcd"]
enumAlphabet :: Series m Text
enumAlphabet = L.toStrict <$> L.Series.enumAlphabet

-- | A 'Data.Text.Text' 'Series' that grows by enumerating every 'Char'.
--
-- >>> list 4 enumChars
-- ["","\NUL","\NUL\SOH","\NUL\SOH\STX","\NUL\SOH\STX\ETX"]
enumChars :: Series m Text
enumChars = L.toStrict <$> L.Series.enumChars

-- | A 'Data.Text.Text' 'Series' that grows by enumerating every 'Char' in the
--   given 'String'. Notice that the 'String' can be infinite.
--
-- >>> list 5 $ enumString "xyz"
-- ["","x","xy","xyz"]
enumString :: String -> Series m Text
enumString = fmap L.toStrict . L.Series.enumString

-- | A 'Data.Text.Text' 'Series' that grows with English words.
--
--   This is useful when you want to print 'Series'.
--
-- >>> let s = list 20 jack
-- >>> take 3 s
-- ["","All","All work"]
-- >>> last s
-- "All work and no play makes Jack a dull boy. All work and no play makes Jack a dull boy."
jack :: Series m Text
jack = L.toStrict <$> L.Series.jack

-- | A 'Data.Text.Text' 'Series' that grows with the first character of each
--   <https://en.wikipedia.org/wiki/Plane_(Unicode) Unicode plane>.
--
-- >>> list 3 enumNonBmp
-- ["","\NUL","\NUL\65536","\NUL\65536\131072"]
--
-- Notice that this covers the 16 unicode planes.
--
-- >>> last (list 16 enumNonBmp) == last (list 17 enumNonBmp)
-- True
enumNonBmp :: Series m Text
enumNonBmp = enumString ['\0','\x10000'..'\xF0000']
