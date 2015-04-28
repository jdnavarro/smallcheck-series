{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
'Serial' instances are provided for the following types:

* 'Data.ByteString.ByteString'
* 'Data.ByteString.Lazy.ByteString'
* 'Data.Text.Text'
* 'Data.Text.Lazy.Text'

By default the most economical but less exhaustive series are provided.
You may want to create your own instances for your own tests if this setting
doesn't apply for your own tests. Check the utilities in the other modules
in this package to create custom 'Series'.

Make sure the module where you import these instances is not meant to be
imported, otherwise the orphan instances might create issues.
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
    series = Series.ByteString.replicateA

instance Monad m => Serial m BL.ByteString where
    series = Series.ByteString.Lazy.replicateA

instance Monad m => Serial m T.Text where
    series = Series.Text.replicateA

instance Monad m => Serial m TL.Text where
    series = Series.Text.Lazy.replicateA
