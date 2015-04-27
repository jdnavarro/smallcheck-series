module Test.SmallCheck.Series.ByteString
  ( replicated
  , aaa
  , zzz
  , enumerated
  , ascii
  , alpha
  , jack
  , jack'
  ) where

import Prelude hiding (replicate)
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, replicate)
import Test.SmallCheck.Series

replicated :: Char -> Series m ByteString
replicated c = generate $ \d -> (`replicate` c) <$> [0..d]

zzz :: Series m ByteString
zzz = replicated '\0'

aaa :: Series m ByteString
aaa = replicated 'a'

enumerated  :: String -> Series m ByteString
enumerated cs = generate $ \d -> (\n -> pack $ take n cs) <$> [0..d]

ascii :: Series m ByteString
ascii = enumerated ['\0'..'\255']

alpha :: Series m ByteString
alpha = enumerated ['a'..'z']

jack :: Series m ByteString
jack = jacked cycle

jack' :: Series m ByteString
jack' = jacked id

jacked :: (String -> String) -> Series m ByteString
jacked f = generate $ \d -> fmap pack . take d . words
         $ f "All work and no play makes Jack a dull boy\n"
