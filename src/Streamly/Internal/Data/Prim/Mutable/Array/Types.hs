{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Streamly.Internal.Data.Prim.Mutable.Array.Types
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : streamly@composewell.com
-- Portability : non-portable
--
-- Arrays of unboxed primitive types. The function provided by this module
-- match the behavior of those provided by @Data.Primitive.ByteArray@, and
-- the underlying types and primops that back them are the same.
-- However, the type constructors 'PrimArray' and 'MutablePrimArray' take one additional
-- argument than their respective counterparts 'ByteArray' and 'MutableByteArray'.
-- This argument is used to designate the type of element in the array.
-- Consequently, all function this modules accepts length and incides in
-- terms of elements, not bytes.
--
-- @since 0.6.4.0
module Streamly.Internal.Data.Prim.Mutable.Array.Types
  ( -- * Types
    Array(..)
    -- * Allocation
  , newArray
  , resizeArray
  , shrinkArray
    -- * Element Access
  , writeArray
  , index
    -- * Information
  , length
    -- * Folding
  , foldr
  , foldl'
  ) where

import Prelude hiding (length, foldr)

import GHC.Exts

import Data.Primitive.Types
import Data.Primitive.ByteArray (ByteArray(..))
import Control.Monad.Primitive
import qualified Data.Primitive.ByteArray as PB

data Array a = Array ByteArray#

{-# INLINE newArray #-}
newArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (Array a)
newArray (I# n#)
  = primitive (\s# ->
      case newByteArray# (n# *# sizeOf# (undefined :: a)) s# of
        (# s'#, arr# #) -> (# s'#, Array (unsafeCoerce# arr#) #)
    )

{-# INLINE resizeArray #-}
resizeArray :: forall m a. (PrimMonad m, Prim a)
  => Array a
  -> Int -- ^ new size
  -> m (Array a)
resizeArray (Array arr#) (I# n#) =
    primitive
        (\s# ->
             case resizeMutableByteArray#
                      (unsafeCoerce# arr#)
                      (n# *# sizeOf# (undefined :: a))
                      s# of
                 (# s'#, arr'# #) ->
                     case unsafeFreezeByteArray# arr'# s'# of
                         (# s''#, arr''# #) -> (# s''#, Array arr''# #))

{-# INLINE shrinkArray #-}
shrinkArray :: forall m a. (PrimMonad m, Prim a)
  => Array a
  -> Int -- ^ new size
  -> m ()
shrinkArray (Array arr#) (I# n#) =
    primitive_ (shrinkMutableByteArray# (unsafeCoerce# arr#) (n# *# sizeOf# (undefined :: a)))


-- | Write an element to the given index.
{-# INLINE writeArray #-}
writeArray ::
     (Prim a, PrimMonad m)
  => Array a -- ^ array
  -> Int -- ^ index
  -> a -- ^ element
  -> m ()
writeArray (Array arr#) (I# i#) x
  = primitive_ (writeByteArray# (unsafeCoerce# arr#) i# x)

-- | Read a primitive value from the primitive array.
{-# INLINE index #-}
index :: forall a. Prim a => Array a -> Int -> a
index (Array arr#) (I# i#) = indexByteArray# arr# i#

-- | Get the size, in elements, of the primitive array.
{-# INLINE length #-}
length :: forall a. Prim a => Array a -> Int
length (Array arr#) = I# (quotInt# (sizeofByteArray# arr#) (sizeOf# (undefined :: a)))

-- | Lazy right-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldr #-}
foldr :: forall a b. Prim a => (a -> b -> b) -> b -> Array a -> b
foldr f z arr = go 0
  where
    !sz = length arr
    go !i
      | sz > i = f (index arr i) (go (i+1))
      | otherwise = z

-- | Strict left-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldl' #-}
foldl' :: forall a b. Prim a => (b -> a -> b) -> b -> Array a -> b
foldl' f z0 arr = go 0 z0
  where
    !sz = length arr
    go !i !acc
      | i < sz = go (i + 1) (f acc (index arr i))
      | otherwise = acc
