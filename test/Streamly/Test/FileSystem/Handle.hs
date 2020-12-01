-- |
-- Module      : Streamly.Test.FileSystem.Handle
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.FileSystem.Handle (main) where

import Data.Word (Word8)
import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import System.IO
    ( Handle
    , IOMode(..)
    , SeekMode(..)
    , hClose
    , hFlush
    , hSeek
    , openFile
    )
import System.IO.Temp (writeSystemTempFile)
import Streamly.FileSystem.Handle as H

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
import qualified Streamly.Internal.Unicode.Stream as U
import Data.Functor.Identity (runIdentity)
import Foreign.Storable (Storable(..))

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

maxTestCount :: Int
maxTestCount = 10

chooseWord8 :: (Word8, Word8) -> Gen Word8
chooseWord8 = choose

utf8ToString :: A.Array Word8 -> String
utf8ToString = runIdentity . S.toList . U.decodeUtf8' . A.toStream

testData :: String
testData = "This is the test data for FileSystem.Handle ??`!@#$%^&*~~))`]"

testDataLarge :: String
testDataLarge = concat $ replicate 6000 testData

tempHandle :: IO Handle
tempHandle = do
    fp <- writeSystemTempFile "fs_handle" testDataLarge
    openFile fp ReadMode

tempHandleW :: IO Handle
tempHandleW = do
    fp <- writeSystemTempFile "fs_handle" ""
    openFile fp ReadWriteMode

readFromHandle :: IO String
readFromHandle = do
    h <- tempHandle
    ls <- S.toList $ S.unfold H.read h
    hClose h
    let arr = A.fromList ls
    return $ utf8ToString arr

readWithBufferFromHandle :: IO String
readWithBufferFromHandle = do
    h <- tempHandle
    ls <- S.toList $ S.unfold H.readWithBufferOf (1024, h)
    hClose h
    let arr = A.fromList ls
    return $ utf8ToString arr

readChunksFromHandle :: IO String
readChunksFromHandle = do
    h <- tempHandle
    ls <- S.toList $ S.unfold H.readChunks h
    hClose h
    let arr = A.fromList $ concat $ fmap A.toList ls
    return $ utf8ToString arr

readChunksWithBuffer :: IO String
readChunksWithBuffer = do
    h <- tempHandle
    ls <- S.toList $ S.unfold H.readChunksWithBufferOf (1024, h)
    hClose h
    let arr = A.fromList $ concat $ fmap A.toList ls
    return $ utf8ToString arr

testRead :: IO String -> Property
testRead fn = monadicIO $ do
    v1 <- run $ fn
    assert (v1 == testDataLarge)

testWrite :: (Handle -> FL.Fold IO Word8 ()) -> Property
testWrite hfold = forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len $ chooseWord8 (0, 255)) $ \list ->
            monadicIO $ do
                h <- run $ tempHandleW
                run $ hSeek h AbsoluteSeek 0
                run $ S.fold (hfold h) $ S.fromList list
                run $ hFlush h
                run $ hSeek h AbsoluteSeek 0
                ls <- run $ S.toList $ S.unfold H.read h
                assert (ls == list)

testWriteWithChunk :: Property
testWriteWithChunk =
            monadicIO $ do
                hr <- run $ tempHandle
                h <- run $ tempHandleW
                run $ hSeek h AbsoluteSeek 0
                run $ S.fold (H.writeChunks h)
                    $ S.unfold H.readChunksWithBufferOf (1024, hr)
                run $ hFlush h
                run $ hSeek h AbsoluteSeek 0
                ls <- run $ S.toList $ S.unfold H.read h
                let arr = A.fromList ls
                assert (testDataLarge == utf8ToString arr)

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe "Construction" $ do
            prop "testRead" $ testRead readFromHandle
            prop "testReadWithBuffer" $ testRead readWithBufferFromHandle
            prop "testReadChunks" $ testRead readChunksFromHandle
            prop "testReadChunksWithBuffer" $ testRead readChunksWithBuffer
            prop "testWrite" $ testWrite H.write
            prop "testWriteWithBufferOf" $ testWrite $ H.writeWithBufferOf 1024
            prop "testWriteWithChunk" $ testWriteWithChunk
