module Main (main) where

import Data.Semigroup as SG
import Prelude hiding 
   (maximum, minimum, elem, notElem, null, product, sum, head, last)
import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck 
   ( Property
   , forAll
   , Gen
   , vectorOf
   , arbitrary
   , choose
   , withMaxSuccess
   , listOf1
   )
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Streamly.Internal.Data.Fold as F
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL

maxStreamLen :: Int
maxStreamLen = 1000

intMin :: Int
intMin = minBound

intMax :: Int
intMax = maxBound

{-# INLINE maxStreamLen #-}
{-# INLINE intMin #-}
{-# INLINE intMax #-}

rollingHashFirstN :: Property
rollingHashFirstN =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
                a <- run $ S.fold F.rollingHash $ S.take n $ S.fromList vec
                b <- run $ S.fold (F.rollingHashFirstN n) $ S.fromList vec
                assert $ a == b


head :: [Int] -> Expectation
head ls = S.fold FL.head (S.fromList ls) `shouldReturn` headl ls
            where
            headl [] = Nothing
            headl (x:_) = Just x


length :: [Int] -> Expectation
length ls = S.fold FL.length (S.fromList ls) `shouldReturn` Prelude.length ls


sum :: [Int] -> Expectation
sum ls = S.fold FL.sum (S.fromList ls) `shouldReturn` foldl (+) 0 ls


product :: [Int] -> Expectation
product ls = S.fold FL.product (S.fromList ls) `shouldReturn` foldl (*) 1 ls


lesser :: (a -> a -> Ordering) -> a -> a -> a
lesser f x y = if f x y == LT then x else y


greater :: (a -> a -> Ordering) -> a -> a -> a
greater f x y = if f x y == GT then x else y


foldMaybe :: (b -> a -> b) -> b -> [a] -> Maybe b
foldMaybe f acc ls =
                  case ls of
                     [] -> Nothing
                     _ -> Just (foldl f acc ls)


maximumBy :: (Ord a, Show a) => a -> (a -> a -> Ordering) -> [a] -> Expectation
maximumBy genmin f ls = S.fold (FL.maximumBy f) (S.fromList ls) `shouldReturn` foldMaybe (greater f) genmin ls


maximum :: (Show a, Ord a) => a -> [a] -> Expectation
maximum genmin ls = S.fold FL.maximum (S.fromList ls) `shouldReturn` foldMaybe (greater compare) genmin ls


minimumBy :: (Ord a, Show a) => a -> (a -> a -> Ordering) -> [a] -> Expectation
minimumBy genmax f ls = S.fold (FL.minimumBy f) (S.fromList ls) `shouldReturn` foldMaybe (lesser f) genmax ls


minimum :: (Show a, Ord a) => a -> [a] -> Expectation
minimum genmax ls = S.fold FL.minimum (S.fromList ls) `shouldReturn` foldMaybe (lesser compare) genmax ls


toList :: [Int] -> Expectation
toList ls = S.fold FL.toList (S.fromList ls) `shouldReturn` ls


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs


last :: [String] -> Expectation
last ls = S.fold FL.last (S.fromList ls) `shouldReturn` safeLast ls

mapMaybe :: [Int] -> Expectation
mapMaybe ls =
    (let f = F.mapMaybe (\x -> if even x then Just x else Nothing) FL.toList
      in S.fold f (S.fromList ls))
     `shouldReturn` filter even ls

nth :: Int -> [a] -> Maybe a
nth idx (x:xs) = if idx == 0
                 then Just x
                 else if idx < 0
                      then Nothing
                      else nth (idx - 1) xs
nth _ [] = Nothing


index :: Int -> [String] -> Expectation
index idx ls = let x = S.fold (FL.index idx) (S.fromList ls)
                in x `shouldReturn` (nth idx ls)


find :: (Show a, Eq a) => (a -> Bool) -> [a] -> Expectation
find f ls = do
   let x = S.fold (FL.findIndex f) (S.fromList ls)
   y <- x
   case y of
      Nothing  -> S.fold (FL.find f) (S.fromList ls) `shouldReturn` Nothing
      Just idx -> S.fold (FL.any f) (S.fromList $ take idx ls) `shouldReturn` False


neg :: (a -> Bool) -> a -> Bool
neg f x = if f x == True then False else True

findIndex :: (a -> Bool) -> [a] -> Expectation
findIndex f ls = do
   let x = S.fold (FL.findIndex f) (S.fromList ls)
   y <- x
   case y of
      Nothing  -> S.fold (FL.all $ neg f) (S.fromList ls) `shouldReturn` True
      Just idx -> if idx == 0
                  then S.fold (FL.all f) (S.fromList []) `shouldReturn` True
                  else S.fold (FL.all f) (S.fromList $ (take idx ls)) `shouldReturn` False

predicate :: Int -> Bool
predicate x = if x * x < 100 then True else False

elemIndex :: Int -> [Int] -> Expectation
elemIndex elm ls = do
   let x = S.fold (FL.elemIndex elm) (S.fromList ls)
   y <- x
   case y of
      Nothing  -> S.fold (FL.any (\z -> if z == elm
                                        then True
                                        else False)) (S.fromList ls) `shouldReturn` False
      Just idx -> S.fold (FL.any (\z -> if z == elm
                                        then True
                                        else False)) (S.fromList $ (take idx ls)) `shouldReturn` False

null :: [Int] -> Expectation
null ls = S.fold FL.null (S.fromList ls) `shouldReturn` case ls of
                                                            [] -> True
                                                            _ -> False
elem :: Int -> [Int] -> Expectation
elem elm ls = do
   let x = S.fold (FL.elem elm) (S.fromList ls)
   y <- x
   S.fold (FL.any (\z -> if z == elm
                         then True
                         else False)) (S.fromList ls) `shouldReturn` y


notElem :: Int -> [Int] -> Expectation
notElem elm ls = do
   let x = S.fold (FL.notElem elm) (S.fromList ls)
   y <- x
   S.fold (FL.any (\z -> if z == elm
                         then True
                         else False)) (S.fromList ls) `shouldReturn` (if y == True
                                                                      then False
                                                                      else True)


all :: (a -> Bool) -> [a] -> Expectation
all f ls = S.fold (FL.all f) (S.fromList ls) `shouldReturn` Prelude.and (map f ls)


any :: (a -> Bool) -> [a] -> Expectation
any f ls = S.fold (FL.any f) (S.fromList ls) `shouldReturn` Prelude.any f ls


and :: [Bool] -> Expectation
and ls = S.fold FL.and (S.fromList ls) `shouldReturn` Prelude.and ls


or :: [Bool] -> Expectation
or ls = S.fold FL.or (S.fromList ls) `shouldReturn` Prelude.or ls

chooseInt :: (Int, Int) -> Gen Int
chooseInt = choose

chooseFloat :: (Float, Float) -> Gen Float
chooseFloat = choose

drain :: [Int] -> Expectation
drain ls = S.fold FL.drain (S.fromList ls) `shouldReturn` ()

drainBy :: [Int] -> Expectation
drainBy ls = S.fold (FL.drainBy return) (S.fromList ls) `shouldReturn` ()

mean :: Property
mean = forAll (listOf1 (chooseFloat (-100.0, 100.0))) $ \ls ->
   withMaxSuccess 1000 $
      monadicIO $ do
         v1 <- run $ S.fold FL.mean (S.fromList ls)  
         let v2 = (foldl (+) 0 ls) / fromIntegral (Prelude.length ls)
         assert (abs (v1 - v2) < 0.0001 )

stdDev :: Property
stdDev = forAll (listOf1 (chooseFloat (-100.0, 100.0))) $ \ls ->
   withMaxSuccess 1000 $
      monadicIO $ do
         v1 <- run $ S.fold FL.stdDev (S.fromList ls)  
         let avg = (foldl (+) 0 ls) / fromIntegral (Prelude.length ls)
             se = (foldl (+) 0  (map (\x -> (x - avg) * (x - avg)) ls)) 
             sd = sqrt $ se / fromIntegral (Prelude.length ls)
         assert (abs (v1 - sd) < 0.0001 )   

variance :: Property
variance = forAll (listOf1 (chooseFloat (-100.0, 100.0))) $ \ls ->
   withMaxSuccess 1000 $
      monadicIO $ do
         v1 <- run $ S.fold FL.variance (S.fromList ls)  
         let avg = (foldl (+) 0 ls) / fromIntegral (Prelude.length ls)
             se = (foldl (+) 0  (map (\x -> (x - avg) * (x - avg)) ls)) 
             vr = se / fromIntegral (Prelude.length ls)
         assert (abs (v1 - vr) < 0.01 )  

mconcat :: Property
mconcat = forAll (listOf1 (chooseInt (intMin, intMax))) $ \ls ->  
      monadicIO $ do
         v1 <- run $ S.fold FL.mconcat (S.map SG.Sum $ S.fromList ls)
         let v2 = (foldl (+) 0 ls)        
         assert (SG.getSum v1 == v2)        

foldMap :: Property
foldMap = forAll (listOf1 (chooseInt (intMin, intMax))) $ \ls ->  
      monadicIO $ do
         v1 <- run $ S.fold (FL.foldMap SG.Sum) $ S.fromList ls
         let v2 = (foldl (+) 0 ls)        
         assert (SG.getSum v1 == v2)               

foldMapM :: Property
foldMapM = forAll (listOf1 (chooseInt (intMin, intMax))) $ \ls ->  
      monadicIO $ do
         v1 <- run $ S.fold (FL.foldMapM (return . SG.Sum)) $ S.fromList ls
         let v2 = (foldl (+) 0 ls)        
         assert (SG.getSum v1 == v2) 

lookup :: Property
lookup = forAll (chooseInt (1, 15)) $ \key ->  
   monadicIO $ do          
      let ls = [(1, "first"), (2, "second"), (3, "third"), (4, "fourth") 
                  , (5, "fifth"), (6, "fifth+first"), (7, "fifth+second")
                  , (8, "fifth+third"), (9, "fifth+fourth")
                  , (10, "fifth+fifth")]     
      v1 <- run $ S.fold (FL.lookup key) $ S.fromList ls             
      let v2 = Prelude.lookup key ls
      assert (v1 == v2) 

mapM :: Property
mapM = forAll (listOf1 (chooseInt (intMin, intMax))) $ \ls ->   
   monadicIO $ do   
      v1 <- run 
         $ S.fold (FL.mapM (\x -> return (x+(Prelude.length ls))) FL.sum)
         $ S.fromList ls
      let v2 = foldl (+) (Prelude.length ls) ls      
      assert (v1 == v2)   

teeWithLength :: Property
teeWithLength = forAll (listOf1 (chooseInt (intMin, intMax))) $ \ls ->   
   monadicIO $ do   
      v1 <- run $ S.fold (FL.tee FL.sum FL.length) $ S.fromList ls
      let v2 = foldl (+) 0 ls   
          v3 = Prelude.length ls   
      assert (v1 == (v2, v3))

teeWithMax :: Property
teeWithMax = forAll (listOf1 (chooseInt (intMin, intMax))) $ \ls ->   
   monadicIO $ do   
      v1 <- run $ S.fold (FL.tee FL.sum FL.maximum) $ S.fromList ls
      let v2 = foldl (+) 0 ls   
          v3 = foldMaybe (greater compare) intMin ls
      assert (v1 == (v2, v3))

distribute :: Property
distribute = forAll (listOf1 (chooseInt (intMin, intMax))) $ \ls ->   
   monadicIO $ do   
      v1 <- run $ S.fold (FL.distribute [FL.sum, FL.length]) $ S.fromList ls
      let v2 = foldl (+) 0 ls   
          v3 = Prelude.length ls
      assert (v1 == [v2, v3])

partition :: Property
partition =   
   monadicIO $ do   
      v1 :: (Int, [String]) <- run $ S.fold (FL.partition FL.sum FL.toList) 
         $ S.fromList [Left 1, Right "abc",Left 3, Right "xy", Right "pp2"]
      let v2 = (4,["abc","xy","pp2"])             
      assert (v1 == v2)      

unzip :: Property
unzip =   
   monadicIO $ do   
      v1 :: (Int, [String]) <- run $ S.fold (FL.unzip FL.sum FL.toList) 
         $ S.fromList [(1, "aa"), (2, "bb"), (3, "cc")]
      let v2 = (6, ["aa", "bb", "cc"])             
      assert (v1 == v2) 

main :: IO ()
main = hspec $
    describe "Fold s" $ do
        prop "RollingHashFirstN" rollingHashFirstN
        prop "Index" $ index
        prop "Head" head
        prop "Last" last
        prop "Length" Main.length
        prop "Sum" sum
        prop "Product" product
        prop "MaximumBy" $ maximumBy intMin compare
        prop "Maximum" $ maximum intMin
        prop "MinimumBy" $ minimumBy intMax compare
        prop "Minimum" $ minimum intMax
        prop "ToList" toList
        prop "Find" $ find predicate
        prop "FindIndex" $ findIndex predicate
        prop "ElemIndex" $ elemIndex 10
        prop "Null" null
        prop "Elem" $ elem 10
        prop "NotElem" $ notElem 10
        prop "All" $ Main.all predicate
        prop "Any" $ Main.any predicate
        prop "And" Main.and
        prop "Or" Main.or
        prop "mapMaybe" mapMaybe
        prop "drain" Main.drain
        prop "drainBy" Main.drainBy
        prop "mean" Main.mean
        prop "stdDev" Main.stdDev
        prop "variance" Main.variance
        prop "mconcat" Main.mconcat
        prop "foldMap" Main.foldMap
        prop "foldMapM" Main.foldMapM
        prop "lookup" Main.lookup
        prop "mapM" Main.mapM
        prop "teeWithLength" Main.teeWithLength
        prop "teeWithMax" Main.teeWithMax
        prop "distribute" Main.distribute
        prop "partition" Main.partition
        prop "unzip" Main.unzip
