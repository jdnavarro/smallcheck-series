{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
'Serial' instances are provided for the following types:

* 'Data.Word'
* 'Data.Word8'
* 'Data.Word16'
* 'Data.Word32'
* 'Data.Word64'
* 'Data.Int8'
* 'Data.Int16'
* 'Data.Int32'
* 'Data.Int64'
* 'Data.ByteString.ByteString'
* 'Data.ByteString.Lazy.ByteString'
* 'Data.Text.Text'
* 'Data.Text.Lazy.Text'
* 'Data.Text.Lazy.Text'
* 'Data.Map.Map'

By default the most exhaustive series are provided which can lead to
combinatorial explosion if you are not careful. In such case, you may want to
use the functions provided in the other modules in this package to create your
own custom series.

Make sure the module where you import these instances will not be imported,
otherwise you might get conflicts between orphan instances defined in different
modules.
-}
module Test.SmallCheck.Series.Instances () where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Int
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Map
import qualified Data.Map as Map
import Test.SmallCheck.Series

instance Monad m => Serial m Int8 where
    series = (fromIntegral :: Int -> Int8) <$> series
instance Monad m => CoSerial m Int8 where
    coseries rs = (. (fromIntegral :: Int8 -> Int)) <$> coseries rs

instance Monad m => Serial m Int16 where
    series = (fromIntegral :: Int -> Int16) <$> series
instance Monad m => CoSerial m Int16 where
    coseries rs = (. (fromIntegral :: Int16 -> Int)) <$> coseries rs

instance Monad m => Serial m Int32 where
    series = (fromIntegral :: Int -> Int32) <$> series
instance Monad m => CoSerial m Int32 where
    coseries rs = (. (fromIntegral :: Int32 -> Int)) <$> coseries rs

instance Monad m => Serial m Int64 where
    series = (fromIntegral :: Int -> Int64) <$> series
instance Monad m => CoSerial m Int64 where
    coseries rs = (. (fromIntegral :: Int64 -> Int)) <$> coseries rs

instance Monad m => Serial m Word where series = positive
instance Monad m => CoSerial m Word where coseries = copositive

instance Monad m => Serial m Word8 where series = positive
instance Monad m => CoSerial m Word8 where coseries = copositive

instance Monad m => Serial m Word16 where series = positive
instance Monad m => CoSerial m Word16 where coseries = copositive

instance Monad m => Serial m Word32 where series = positive
instance Monad m => CoSerial m Word32 where coseries = copositive

instance Monad m => Serial m Word64 where series = positive
instance Monad m => CoSerial m Word64 where coseries = copositive

instance Monad m => Serial m B.ByteString where
    series = cons0 B.empty \/ cons2 B.cons
instance Monad m => CoSerial m B.ByteString where
    coseries rs =
        alts0 rs >>- \y ->
        alts2 rs >>- \f ->
            return $ \bs -> case B.uncons bs of
                Nothing -> y
                Just (b,bs') -> f (B.singleton b) bs'

instance Monad m => Serial m BL.ByteString where
    series = cons0 BL.empty \/ cons2 BL.cons
instance Monad m => CoSerial m BL.ByteString where
    coseries rs =
        alts0 rs >>- \y ->
        alts2 rs >>- \f ->
            return $ \bs -> case BL.uncons bs of
                Nothing -> y
                Just (b,bs') -> f (BL.singleton b) bs'

instance Monad m => Serial m T.Text where
    series = cons0 T.empty \/ cons2 T.cons
instance Monad m => CoSerial m T.Text where
    coseries rs =
        alts0 rs >>- \y ->
        alts2 rs >>- \f ->
            return $ \bs -> case T.uncons bs of
                Nothing -> y
                Just (b,bs') -> f (T.singleton b) bs'

instance Monad m => Serial m TL.Text where
    series = cons0 TL.empty \/ cons2 TL.cons
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

-- * Internal
positive :: Integral n => Series m n
positive = generate $ \d -> [0..fromIntegral d]

copositive :: (Num a, Ord a, CoSerial m a) => Series m b -> Series m (a -> b)
copositive rs =
    alts0 rs >>- \z ->
    alts1 rs >>- \f ->
    return $ \w ->
      if w > 0
        then f (w-1)
        else z
