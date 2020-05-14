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

import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Fold.Types as FL
import Streamly.Internal.Data.SVar (adaptState)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Strict (Tuple'(..))
import Streamly (IsStream)

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

scan :: (IsStream t, Monad m) => Scan m a b -> t m a -> t m b
scan s = D.fromStreamD Prelude.. scanD s Prelude.. D.toStreamD

scanD :: Monad m => Scan m a b -> D.Stream m a -> D.Stream m b
scanD (Scan scan_step initial) (D.UnStream stream_step stream_state) =
    D.UnStream step (stream_state, initial)
  where
    step gst (st, acc) = do
        r <- stream_step (adaptState gst) st
        case r of
            D.Yield x s -> do
                Tuple' acc' b <- scan_step acc x
                return $ D.Yield b (s, acc')
            D.Skip s -> return $ D.Skip (s, acc)
            D.Stop -> return D.Stop

scanFromFold :: Monad m => FL.Fold m a b -> Scan m a b
scanFromFold (FL.Fold fl_step fl_initial fl_extract) = Scan step fl_initial
  where
    step ms a = do
        s <- ms
        s' <- fl_step s a
        b <- fl_extract s'
        return $ Tuple' (return s') b
