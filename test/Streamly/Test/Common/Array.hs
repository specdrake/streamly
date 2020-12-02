-- |
-- Module      : Main
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

-- This is a common array test module that gets included in different
-- Array test modules with the corresponding macro defined.

import Foreign.Storable (Storable(..))

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import Test.Hspec as H

import Streamly.Prelude (SerialT)
import qualified Streamly.Prelude as S
#ifdef TEST_SMALL_ARRAY
import qualified Streamly.Internal.Data.SmallArray as A
type Array = A.SmallArray
#elif defined(TEST_ARRAY)
import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Stream.IsStream as IP
type Array = A.Array
#elif defined(DATA_ARRAY_PRIM_PINNED)
import qualified Streamly.Internal.Data.Array.Prim.Pinned as A
import qualified Streamly.Internal.Data.Array.Prim.Pinned.Types as A
import qualified Streamly.Internal.Data.Stream.IsStream as IP
type Array = A.Array
#elif defined(DATA_ARRAY_PRIM)
import qualified Streamly.Internal.Data.Array.Prim as A
import qualified Streamly.Internal.Data.Array.Prim.Types as A
import qualified Streamly.Internal.Data.Stream.IsStream as IP
type Array = A.Array
#else
import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
type Array = A.Array
#endif


-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

-- XXX this should be in sync with the defaultChunkSize in Array code, or we
-- should expose that and use that. For fast testing we could reduce the
-- defaultChunkSize under CPP conditionals.
--
defaultChunkSize :: Int
defaultChunkSize = 32 * k - allocOverhead
   where k = 1024

maxArrLen :: Int
maxArrLen = defaultChunkSize * 8

genericTestFrom ::
       (Int -> SerialT IO Int -> IO (Array Int))
    -> Property
genericTestFrom arrFold =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- run $ arrFold len $ S.fromList list
                assert (A.length arr == len)

testLength :: Property
testLength = genericTestFrom (\n -> S.fold (A.writeN n))

testLengthFromStreamN :: Property
testLengthFromStreamN = genericTestFrom A.fromStreamN

#ifndef TEST_SMALL_ARRAY
testLengthFromStream :: Property
testLengthFromStream = genericTestFrom (const A.fromStream)

testFromListN :: Property
testFromListN =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    let arr = A.fromListN n list
                    assert (A.toList arr == take n list)

testFromList :: Property
testFromList =
    forAll (choose (0, maxArrLen)) $ \len ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    let arr = A.fromList list
                    assert (A.toList arr == list)
#endif

genericTestFromTo ::
       (Int -> SerialT IO Int -> IO (Array Int))
    -> (Array Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> Property
genericTestFromTo arrFold arrUnfold listEq =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- run $ arrFold len $ S.fromList list
                xs <- run $ S.toList $ arrUnfold arr
                assert (listEq xs list)

testFoldNUnfold :: Property
testFoldNUnfold =
    genericTestFromTo (\n -> S.fold (A.writeN n)) (S.unfold A.read) (==)

testFoldNToStream :: Property
testFoldNToStream =
    genericTestFromTo (\n -> S.fold (A.writeN n)) A.toStream (==)

testFoldNToStreamRev :: Property
testFoldNToStreamRev =
    genericTestFromTo
        (\n -> S.fold (A.writeN n))
        A.toStreamRev
        (\xs list -> xs == reverse list)

testFromStreamNUnfold :: Property
testFromStreamNUnfold = genericTestFromTo A.fromStreamN (S.unfold A.read) (==)

testFromStreamNToStream :: Property
testFromStreamNToStream = genericTestFromTo A.fromStreamN A.toStream (==)

#ifndef TEST_SMALL_ARRAY
testFromStreamToStream :: Property
testFromStreamToStream = genericTestFromTo (const A.fromStream) A.toStream (==)

testFoldUnfold :: Property
testFoldUnfold = genericTestFromTo (const (S.fold A.write)) (S.unfold A.read) (==)
#endif

#if defined(TEST_ARRAY) ||\
    defined(DATA_ARRAY_PRIM) ||\
    defined(DATA_ARRAY_PRIM_PINNED)

testArraysOf :: Property
testArraysOf =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                xs <- run
                    $ S.toList
                    $ S.concatUnfold A.read
                    $ arraysOf 240
                    $ S.fromList list
                assert (xs == list)
  where
    arraysOf n = IP.chunksOf n (A.writeNUnsafe n)

#endif

#ifdef TEST_ARRAY

lastN :: Int -> [a] -> [a]
lastN n l = drop (length l - n) l

testLastN :: Property
testLastN =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    xs <- run
                        $ fmap A.toList
                        $ S.fold (A.lastN n)
                        $ S.fromList list
                    assert (xs == lastN n list)

testLastN_LN :: Int -> Int -> IO Bool
testLastN_LN len n = do
    let list = [1..len]
    l1 <- fmap A.toList $ S.fold (A.lastN n) $ S.fromList list
    let l2 = lastN n list
    return $ l1 == l2
#endif

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe "Construction" $ do
            prop "length . writeN n === n" testLength
            prop "length . fromStreamN n === n" testLengthFromStreamN
            prop "read . writeN === id " testFoldNUnfold
            prop "toStream . writeN === id" testFoldNToStream
            prop "toStreamRev . writeN === reverse" testFoldNToStreamRev
            prop "read . fromStreamN === id" testFromStreamNUnfold
            prop "toStream . fromStreamN === id" testFromStreamNToStream
#ifndef TEST_SMALL_ARRAY
            prop "length . fromStream === n" testLengthFromStream
            prop "toStream . fromStream === id" testFromStreamToStream
            prop "read . write === id" testFoldUnfold
            prop "First N elements of a list" testFromListN
            prop "From a list" testFromList
#endif

#if defined(TEST_ARRAY) ||\
    defined(DATA_ARRAY_PRIM) ||\
    defined(DATA_ARRAY_PRIM_PINNED)
            prop "arraysOf concats to original" testArraysOf
#endif

#ifdef TEST_ARRAY
        describe "Fold" $ do
            prop "lastN : 0 <= n <= len" $ testLastN
            describe "lastN boundary conditions" $ do
                it "lastN -1" (testLastN_LN 10 (-1) `shouldReturn` True)
                it "lastN 0" (testLastN_LN 10 0 `shouldReturn` True)
                it "lastN length" (testLastN_LN 10 10 `shouldReturn` True)
                it "lastN (length + 1)" (testLastN_LN 10 11 `shouldReturn` True)
#endif
