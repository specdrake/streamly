-- |
-- Module      : Streamly.Internal.Data.Scan.Types
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE ExistentialQuantification #-}

module Streamly.Internal.Data.Scan.Types where

import Control.Category

import qualified Streamly.Internal.Data.Stream.Prelude as P
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Strict (Tuple'(..))

data Scan m a b =
    -- | @Scan @ @ step @ @ initial @ @ extract@
    forall s. Scan (s -> a -> m (Tuple' s b)) s

instance Functor m => Functor (Scan m a) where
    fmap f (Scan step initial) =
        Scan
            (\s a -> fmap (\(Tuple' s' b) -> Tuple' s' (f b)) (step s a))
            initial

instance Monad m => Applicative (Scan m a) where
    pure b = Scan (\_ _ -> pure (Tuple' () b)) ()

    -- (<*>) :: Scan m a (b -> c) -> Scan m a b -> Scan m a c
    Scan stepL initialL <*> Scan stepR initialR =
        Scan step initial
      where
        initial = Tuple' initialL initialR
        step (Tuple' xL xR) a = do
          Tuple' sL bcL <- stepL xL a
          Tuple' sR bR  <- stepR xR a
          pure $ Tuple' (Tuple' sL sR) (bcL bR)

{-
scan :: Monad m => Scan m a b -> SerialT m a -> SerialT m b
scan (Scan step initial extract) = P.scanlMx' step initial extract
-}

mapM :: Monad m => (b -> m c) -> Scan m a b -> Scan m a c
mapM f (Scan step initial) = Scan step' initial
  where
    step' s a = do
        Tuple' sb b <- step s a
        Tuple' sb <$> f b

lmapM :: Monad m => (a -> m b) -> Scan m b c -> Scan m a c
lmapM f (Scan step initial) = Scan step' initial
  where
    step' s a = do
        b <- f a
        step s b

lmap :: Monad m => (a -> b) -> Scan m b c -> Scan m a c
lmap f = lmapM (return Prelude.. f)

dot :: Monad m => Scan m b c -> Scan m a b -> Scan m a c
Scan stepL initialL `dot` Scan stepR initialR = Scan step initial
  where
    initial = Tuple' initialR initialL
    step (Tuple' sa sb) a = do
      Tuple' sa' b <- stepR sa a
      Tuple' sb' c <- stepL sb b
      return $ Tuple' (Tuple' sa' sb') c

instance Monad m => Category (Scan m) where
    -- id :: Scan m a a
    id = Scan (\_ a -> return (Tuple' () a)) ()

    (.) = dot
