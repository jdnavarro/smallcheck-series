{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
'Serial' and 'CoSerial' instances are provided for the following types:

* 'Data.ByteString.ByteString'
* 'Data.ByteString.Lazy.ByteString'
* 'Data.Text.Text'
* 'Data.Text.Lazy.Text'
-}
module Test.SmallCheck.Instances () where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.SmallCheck.Series

instance Monad m => Serial m B.ByteString where
    series = cons0 mempty \/ cons2 B.append
instance Monad m => CoSerial m B.ByteString where
  coseries rs =
    alts0 rs >>- \y ->
    alts2 rs >>- \f ->
    return $ \bs -> case B.uncons bs of
                         Just (b,bs') -> f (B.singleton b) bs'
                         Nothing -> y

instance Monad m => Serial m BL.ByteString where
    series = cons0 mempty \/ cons2 BL.append
instance Monad m => CoSerial m BL.ByteString where
  coseries rs =
    alts0 rs >>- \y ->
    alts2 rs >>- \f ->
    return $ \bs -> case BL.uncons bs of
                         Just (b,bs') -> f (BL.singleton b) bs'
                         Nothing -> y

instance Monad m => Serial m T.Text where
    series = cons0 mempty \/ cons2 T.append
instance Monad m => CoSerial m T.Text where
  coseries rs =
    alts0 rs >>- \y ->
    alts2 rs >>- \f ->
    return $ \bs -> case T.uncons bs of
                         Just (b,bs') -> f (T.singleton b) bs'
                         Nothing -> y

instance Monad m => Serial m TL.Text where
    series = cons0 mempty \/ cons2 TL.append
instance Monad m => CoSerial m TL.Text where
  coseries rs =
    alts0 rs >>- \y ->
    alts2 rs >>- \f ->
    return $ \bs -> case TL.uncons bs of
                         Just (b,bs') -> f (TL.singleton b) bs'
                         Nothing -> y
