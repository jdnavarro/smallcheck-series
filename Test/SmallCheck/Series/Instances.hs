{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.SmallCheck.Series

instance Monad m => Serial m B.ByteString where
    series = generate $ \d -> (`B8.replicate` 'a') <$> [0..d]

instance Monad m => Serial m BL.ByteString where
    series = generate $ \d -> (`rep` 'a') <$> [0..d]
      where
        rep = L8.replicate . fromIntegral

instance Monad m => Serial m T.Text where
    series = generate $ \d -> (`T.replicate` "a") <$> [0..d]

instance Monad m => Serial m TL.Text where
    series = generate $ \d -> (`rep` "a") <$> [0..d]
      where
        rep = TL.replicate . fromIntegral
