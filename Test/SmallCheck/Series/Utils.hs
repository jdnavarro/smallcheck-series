{-|
Extra utility functions to manipulate 'Test.SmallCheck.Series'.
-}
module Test.SmallCheck.Series.Utils
  (
  -- * Zipping
    zipLogic
  , zipLogic3
  ) where

import Control.Monad.Logic ((<=<), MonadLogic(msplit), lift, mplus, mzero)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

-- $setup
-- >>> import Data.Char
-- >>> import Data.Functor.Identity
-- >>> import Data.Text (Text)
-- >>> import Test.SmallCheck.Series
-- >>> import Test.SmallCheck.Series.Instances

-- | /One-to-One/ zipping of 2 'MonadLogic' instances. You can use for
--   'Test.SmallCheck.Series' like this:
--
-- >>> list 2 $ (series :: Series Identity Char) `zipLogic` (series :: Series Identity Int)
-- [('a',0),('b',1),('c',-1)]
--
-- Notice the difference with 'Test.SmallCheck.Series.><':
--
-- >>> list 2 $ (series :: Series Identity Char) >< (series :: Series Identity Int)
-- [('a',0),('b',0),('a',1),('c',0),('a',-1),...,('b',-2),('c',-2)]

-- Thanks to Roman Cheplyaka: https://groups.google.com/d/msg/haskell-tasty/k0dXCx9EBsc/XYkCTjYKqswJ
zipLogic :: MonadLogic m => m a -> m b -> m (a, b)
zipLogic gx gy =
  maybe mzero return <=< runMaybeT $ do
    (x, rx) <- MaybeT (msplit gx)
    (y, ry) <- MaybeT (msplit gy)
    lift $ return (x, y) `mplus` zipLogic rx ry

-- | /One-to-One/ zipping of 3 'MonadLogic' instances. You can use for
--   'Test.SmallCheck.Series' like this:
--
-- >>> list 3 $ zipLogic3 (series :: Series Identity Char) (series :: Series Identity Int) (series :: Series Identity Text)
-- [('a',0,""),('b',1,"a"),('c',-1,"b"),('d',2,"aa")]

-- Thanks to Roman Cheplyaka: https://groups.google.com/d/msg/haskell-tasty/k0dXCx9EBsc/XYkCTjYKqswJ
zipLogic3 :: MonadLogic m => m a -> m b -> m c -> m (a, b, c)
zipLogic3 gx gy gz =
  maybe mzero return <=< runMaybeT $ do
    (x, rx) <- MaybeT (msplit gx)
    (y, ry) <- MaybeT (msplit gy)
    (z, rz) <- MaybeT (msplit gz)
    lift $ return (x, y, z) `mplus` zipLogic3 rx ry rz
