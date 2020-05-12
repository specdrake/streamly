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

import qualified Streamly.Internal.Data.Stream.Prelude as P
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Strict (Tuple'(..))

data Scan m a b =
    -- | @Scan @ @ step @ @ initial @ @ extract@
    forall s. Scan (s -> a -> m s) (m s) (s -> m b)

instance Functor m => Functor (Scan m a) where
    fmap f (Scan step initial extract) = Scan step initial (fmap f . extract)

instance Applicative m => Applicative (Scan m a) where
    pure b = Scan (\_ _ -> pure ()) (pure ()) (\() -> pure b)
    Scan stepL beginL extractL <*> Scan stepR beginR extractR =
        Scan step begin extract
      where
        begin = Tuple' <$> beginL <*> beginR
        step (Tuple' xL xR) a = Tuple' <$> stepL xL a <*> stepR xR a
        extract (Tuple' xL xR) = extractL xL <*> extractR xR

scan :: Monad m => Scan m a b -> SerialT m a -> SerialT m b
scan (Scan step initial extract) = P.scanlMx' step initial extract

mapM :: Monad m => (b -> m c) -> Scan m a b -> Scan m a c
mapM f (Scan step initial extract) = Scan step initial extract'
  where
    extract' s = do
        b <- extract s
        f b

lmapM :: Monad m => (a -> m b) -> Scan m b c -> Scan m a c
lmapM f (Scan step initial extract) = Scan step' initial extract
  where
    step' s a = do
        b <- f a
        step s b

lmap :: Monad m => (a -> b) -> Scan m b c -> Scan m a c
lmap f = lmapM (return . f)
