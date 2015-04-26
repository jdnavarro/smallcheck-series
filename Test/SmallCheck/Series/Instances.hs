{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
'Serial' instances are provided for the following types:

* 'Data.ByteString.ByteString'
* 'Data.ByteString.Lazy.ByteString'
* 'Data.Text.Text'
* 'Data.Text.Lazy.Text'
-}
module Test.SmallCheck.Series.Instances () where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.SmallCheck.Series

import Test.SmallCheck.Series.ByteString as Series.ByteString
import Test.SmallCheck.Series.ByteString.Lazy as Series.ByteString.Lazy
import Test.SmallCheck.Series.Text as Series.Text
import Test.SmallCheck.Series.Text.Lazy as Series.Text.Lazy

instance Monad m => Serial m B.ByteString where
    series = Series.ByteString.aaa

instance Monad m => Serial m BL.ByteString where
    series = Series.ByteString.Lazy.aaa

instance Monad m => Serial m T.Text where
    series = Series.Text.aaa

instance Monad m => Serial m TL.Text where
    series = Series.Text.Lazy.aaa
