{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
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
import Control.Applicative ((<$>), pure)
#endif
import Control.Applicative ((<|>), empty)
import Data.Int
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Map (Map)
import qualified Data.Map as Map
import Test.SmallCheck.Series
import Control.Monad.Logic (interleave)

#if !MIN_VERSION_smallcheck(1,1,4)

instance Monad m => Serial m Int8 where series = ints
instance Monad m => CoSerial m Int8 where coseries = coInts

instance Monad m => Serial m Int16 where series = ints
instance Monad m => CoSerial m Int16 where coseries = coInts

instance Monad m => Serial m Int32 where series = ints
instance Monad m => CoSerial m Int32 where coseries = coInts

instance Monad m => Serial m Int64 where series = ints
instance Monad m => CoSerial m Int64 where coseries = coInts

ints :: (Monad m, Integral n, Bounded n) => Series m n
ints = generate (\d -> if d >= 0 then pure 0 else empty) <|>
    nats `interleave` (fmap negate nats)
  where
    nats = generate $ \d -> take d [1..maxBound]

coInts :: (Integral n, CoSerial m n) => Series m b -> Series m (n -> b)
coInts rs =
    alts0 rs >>- \z ->
    alts1 rs >>- \f ->
    alts1 rs >>- \g ->
    return $ \i -> if
      | i > 0 -> f (i - 1)
      | i < 0 -> g ((abs i - 1))
      | otherwise -> z

#if !MIN_VERSION_smallcheck(1,1,3)
instance Monad m => Serial m Word where series = nats0
instance Monad m => CoSerial m Word where coseries = conats0
#endif

instance Monad m => Serial m Word8 where series = nats0
instance Monad m => CoSerial m Word8 where coseries = conats0

instance Monad m => Serial m Word16 where series = nats0
instance Monad m => CoSerial m Word16 where coseries = conats0

instance Monad m => Serial m Word32 where series = nats0
instance Monad m => CoSerial m Word32 where coseries = conats0

instance Monad m => Serial m Word64 where series = nats0
instance Monad m => CoSerial m Word64 where coseries = conats0

nats0 :: (Integral n, Bounded n) => Series m n
nats0 = generate $ \d -> take (d+1) [0..maxBound]

conats0 :: (Integral a, CoSerial m a) => Series m b -> Series m (a -> b)
conats0 rs =
    alts0 rs >>- \z ->
    alts1 rs >>- \f ->
    return $ \n ->
    if n > 0
        then f (n-1)
        else z

#endif

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
