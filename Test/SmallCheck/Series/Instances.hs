{-# LANGUAGE CPP #-}
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

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Map
import qualified Data.Map as Map
import Test.SmallCheck.Series


import Test.SmallCheck.Series.ByteString as Series.ByteString
import Test.SmallCheck.Series.ByteString.Lazy as Series.ByteString.Lazy
import Test.SmallCheck.Series.Text as Series.Text
import Test.SmallCheck.Series.Text.Lazy as Series.Text.Lazy

instance Monad m => Serial m B.ByteString where
    series = Series.ByteString.replicateA
instance Monad m => CoSerial m B.ByteString where
    coseries rs =
        alts0 rs >>- \y ->
        alts2 rs >>- \f ->
            return $ \bs -> case B.uncons bs of
                Nothing -> y
                Just (b,bs') -> f (B.singleton b) bs'

instance Monad m => Serial m BL.ByteString where
    series = Series.ByteString.Lazy.replicateA
instance Monad m => CoSerial m BL.ByteString where
    coseries rs =
        alts0 rs >>- \y ->
        alts2 rs >>- \f ->
            return $ \bs -> case BL.uncons bs of
                Nothing -> y
                Just (b,bs') -> f (BL.singleton b) bs'

instance Monad m => Serial m T.Text where
    series = Series.Text.replicateA
instance Monad m => CoSerial m T.Text where
    coseries rs =
        alts0 rs >>- \y ->
        alts2 rs >>- \f ->
            return $ \bs -> case T.uncons bs of
                Nothing -> y
                Just (b,bs') -> f (T.singleton b) bs'

instance Monad m => Serial m TL.Text where
    series = Series.Text.Lazy.replicateA
instance Monad m => CoSerial m TL.Text where
    coseries rs =
        alts0 rs >>- \y ->
        alts2 rs >>- \f ->
            return $ \bs -> case TL.uncons bs of
                Nothing -> y
                Just (b,bs') -> f (TL.singleton b) bs'

instance (Serial m k, Serial m v) => Serial m (Map k v) where
    series = Map.singleton <$> series <~> series
instance (Ord k, CoSerial m k, CoSerial m v) => CoSerial m (Map k v) where
    coseries rs =
        alts0 rs >>- \y ->
        alts2 rs >>- \f ->
            return $ \m -> case pop m of
                Nothing -> y
                Just ((k,v), m') -> f (Map.singleton k v) m'
      where
        pop m = case Map.toList m of
                     [] -> Nothing
                     (kv:its) -> Just (kv, Map.fromList its)
