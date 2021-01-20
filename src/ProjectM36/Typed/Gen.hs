{-# LANGUAGE UndecidableInstances #-}

module ProjectM36.Typed.Gen where

import RIO

import qualified System.Random as R
import Control.Monad(ap)
import Test.QuickCheck.Gen as QC
import Test.QuickCheck.Random as QC
import qualified System.Random.SplitMix as SMGen (SMGen, newSMGen)

gFromGen :: QC.Gen SMGen.SMGen
gFromGen = QC.MkGen (\(QC.QCGen gen) _ -> gen)


newtype GenT m a = GenT { unGenT :: SMGen.SMGen -> Int -> m a }


runGenT :: MonadIO m => GenT m a -> m a
runGenT (GenT f) = do
  g <- liftIO SMGen.newSMGen
  f g 30

instance (Functor m) => Functor (GenT m) where
  fmap f m = GenT $ \r n -> fmap f $ unGenT m r n

instance (Monad m) => Monad (GenT m) where
  return a = GenT (\_ _ -> return a)
  m >>= k = GenT $ \r n -> do
    let (r1, r2) = R.split r
    a <- unGenT m r1 n
    unGenT (k a) r2 n

instance MonadFail m => MonadFail (GenT m) where
  fail msg = GenT (\_ _ -> fail msg)

instance (Functor m, Monad m) => Applicative (GenT m) where
  pure = return
  (<*>) = ap

instance MonadTrans GenT where
  lift m = GenT (\_ _ -> m)


instance (MonadIO m) => MonadIO (GenT m) where
  liftIO = lift . liftIO


mapGenT :: (m a -> n a) -> GenT m a -> GenT n a
mapGenT f (GenT fa) = GenT (\g i -> f (fa g i))


instance (MonadReader env m) => MonadReader env (GenT m) where
  ask = lift ask
  local = mapGenT . local

class (Applicative g, Monad g) => RandomM g where
  randomM :: (R.Random a) => g a
instance (Applicative m, Monad m) => RandomM (GenT m) where
  randomM = GenT $ \g _ -> pure . fst . R.random $ g

{- -- liftGen gen = GenT $ \r n -> return $ QC.unGen gen r n
  choose rng = GenT $ \r _ -> return $ fst $ R.randomR rng r
  variant k (GenT g) = GenT $ \r n -> g (var k r) n
  sized f = GenT $ \r n -> let GenT g = f n in g r n
  resize n (GenT g) = GenT $ \r _ -> g r n
-}
{-class (Applicative g, Monad g) => MonadGen g where
 -- liftGen :: QC.Gen a -> g a
  variant :: Integral n => n -> g a -> g a
  sized :: (Int -> g a) -> g a
  resize :: Int -> g a -> g a
  choose :: R.Random a => (a, a) -> g a

instance (Applicative m, Monad m) => MonadGen (GenT m) where
 -- liftGen gen = GenT $ \r n -> return $ QC.unGen gen r n
  choose rng = GenT $ \r _ -> return $ fst $ R.randomR rng r
  variant k (GenT g) = GenT $ \r n -> g (var k r) n
  sized f = GenT $ \r n -> let GenT g = f n in g r n
  resize n (GenT g) = GenT $ \r _ -> g r n-}