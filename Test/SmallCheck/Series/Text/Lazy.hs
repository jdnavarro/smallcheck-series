module Test.SmallCheck.Series.Text.Lazy
  ( replicated
  , aaa
  , zzz
  , enumerated
  , ascii
  , alpha
  , jack
  , jack'
  , nonBmp
  ) where

import Prelude hiding (replicate)
import Control.Applicative ((<$>))
import Data.Text.Lazy (Text, pack, singleton, replicate)
import Test.SmallCheck.Series

replicated :: Char -> Series m Text
replicated c = generate $ \d -> (`rep` singleton c) <$> [0..d]
  where
    rep = replicate . fromIntegral

zzz :: Series m Text
zzz = replicated '\0'

aaa :: Series m Text
aaa = replicated 'a'

enumerated :: String -> Series m Text
enumerated cs = generate $ \d -> (\n -> pack $ take n cs) <$> [0..d]

ascii :: Series m Text
ascii = enumerated ['\0'..'\255']

alpha :: Series m Text
alpha = enumerated ['a'..'z']

jack :: Series m Text
jack = jacked cycle

jack' :: Series m Text
jack' = jacked id

jacked :: (String -> String) -> Series m Text
jacked f = generate $ \d -> fmap pack . take d . words
         $ f "All work and no play makes Jack a dull boy\n"

nonBmp :: Series m Text
nonBmp = enumerated ['\0','\x1000'..'\xF0000']
