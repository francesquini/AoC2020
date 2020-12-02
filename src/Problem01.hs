{-# LANGUAGE FlexibleInstances #-}

module Problem01 where

import qualified Data.Map            as M
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap         as IM
import qualified Data.Set            as S
import qualified Data.IntSet         as IS

import Data.Maybe
import Control.Monad

import qualified Data.List     as L
import qualified Data.Sequence as SQ
import qualified Data.Vector   as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Array                   as DA

class Conjunto t where
  fromList :: [Int] -> t
  member   :: Int -> t -> Bool
  remove   :: Int -> t -> t
  dobra    :: (b -> Int -> b) -> b -> t -> b

instance Conjunto (M.Map Int Int) where
  fromList xs = M.fromList $ zip xs xs
  member = M.member
  remove = M.delete
  dobra  = M.foldl

instance Conjunto (HM.HashMap Int Int) where
  fromList xs = HM.fromList $ zip xs xs
  member = HM.member
  remove = HM.delete
  dobra = HM.foldl'

instance Conjunto (IM.IntMap Int) where
  fromList xs = IM.fromList $ zip xs xs
  member = IM.member
  remove = IM.delete
  dobra = IM.foldl

instance Conjunto (S.Set Int) where
  fromList xs = S.fromList xs
  member = S.member
  remove = S.delete
  dobra = S.foldl

instance Conjunto IS.IntSet where
  fromList xs = IS.fromList xs
  member = IS.member
  remove = IS.delete
  dobra = IS.foldl

binSearch :: (t -> Int -> Int) -> t -> Int -> Int -> Int -> Int
binSearch at xs k l r
   | r < l           = -1
   | xs `at` mid > k = rec l (mid - 1)
   | xs `at` mid < k = rec (mid + 1) r
   | otherwise       = mid
   where
     rec = binSearch at xs k
     mid = l + ((r - l) `div` 2)

binElem :: (t -> Int -> Int) -> t -> Int -> Int -> Int -> Bool
binElem at xs k l r = (-1) /= binSearch at xs k l r

data Lista = Lista Int [Int]

instance Conjunto Lista where
  fromList xs = Lista (length xs) (L.sort xs)
  member k (Lista len xs) = binElem (!!) xs k 0 len
  remove k (Lista n xs) = Lista (n - 1) (filter (/=k) xs)
  dobra f acc (Lista _ xs) = foldl f acc xs

instance Conjunto (SQ.Seq Int) where
  fromList xs = SQ.sort $ SQ.fromList xs
  member k sq = binElem atSeq sq k 0 (SQ.length sq)
    where
      atSeq s i = fromJust $ s SQ.!? i
  remove k sq = SQ.deleteAt pos sq
    where
      pos = binSearch atSeq sq k 0 (SQ.length sq)
        where
          atSeq s i = fromJust $ s SQ.!? i
  dobra = foldl

instance Conjunto (V.Vector Int) where
  fromList xs = V.modify VA.sort $ V.fromList xs
  member k vs = binElem (V.!) vs k 0 (V.length vs)
  remove k sq = V.filter (/=k) sq
  dobra = foldl

instance Conjunto (DA.Array Int Int) where
  fromList xs = DA.array
    (0, length xs - 1)
    (zip [0..] (L.sort xs))
  member k da = binElem (DA.!) da k 0 (snd $ DA.bounds da)
  remove k arr = DA.array (0, snd (DA.bounds arr) - 1)
    $ zip [0..] [e | (_, e) <- DA.assocs arr, e /= k]
  dobra f acc xs = foldl f acc (DA.elems xs)

withSeq :: Int -> SQ.Seq Int -> (Int, Int)
withSeq tot xs0 = findMatch $ SQ.sort xs0
  where
    findMatch ~(x SQ.:<| (xs SQ.:|> y))
      | x+y == tot = (x,y)
      | x+y <  tot = findMatch (xs SQ.:|> y)
      | otherwise  = findMatch (x SQ.:<| xs)

solCjto :: Conjunto c => Int -> c -> [Int]
solCjto target xs =
  catMaybes $ dobra (\a x -> lu x : a) [] xs
  where
    lu x = do
      guard $ member dif xs
      return $ x * dif
      where
        dif = target - x

solLista, solSeq, solVector, solArray, solMap, solHashMap, solIntMap, solSet, solIntSet, solListComprehension, solSeqBorder  :: Int -> [Int] -> Int
solLista   t xs = head $ solCjto t (fromList xs :: Lista)
solSeq     t xs = head $ solCjto t (fromList xs ::SQ.Seq Int)
solVector  t xs = head $ solCjto t (fromList xs ::V.Vector Int)
solArray   t xs = head $ solCjto t (fromList xs ::DA.Array Int Int)
solMap     t xs = head $ solCjto t (fromList xs ::M.Map Int Int)
solHashMap t xs = head $ solCjto t (fromList xs ::HM.HashMap Int Int)
solIntMap  t xs = head $ solCjto t (fromList xs ::IM.IntMap Int)
solSet     t xs = head $ solCjto t (fromList xs ::S.Set Int)
solIntSet  t xs = head $ solCjto t (fromList xs ::IS.IntSet)
solSeqBorder target xs =
  let (x, y) = withSeq target $ SQ.fromList xs in
    x * y
solListComprehension target xs =
  product $ head [[x,y] | x <- xs, y <- xs, x + y == target]

solCjto3 :: Conjunto c => ([Int] -> c) -> Int -> [Int] -> Int
solCjto3 mkCj target xs =
   head sols
   where
     cj = mkCj xs
     listas = map (\x -> (x, remove x cj)) xs
     sols = concatMap (\(x, cj') -> (x*) <$> solCjto (target - x) cj') listas

solLista3, solSeq3, solVector3, solArray3, solMap3, solHashMap3, solIntMap3, solSet3, solIntSet3, solListComprehension3 :: Int -> [Int] -> Int
solLista3   = solCjto3 (fromList :: [Int] -> Lista)
solSeq3     = solCjto3 (fromList :: [Int] -> SQ.Seq Int)
solVector3  = solCjto3 (fromList :: [Int] -> V.Vector Int)
solArray3   = solCjto3 (fromList ::  [Int] -> DA.Array Int Int)
solMap3     = solCjto3 (fromList ::  [Int] -> M.Map Int Int)
solHashMap3 = solCjto3 (fromList ::  [Int] -> HM.HashMap Int Int)
solIntMap3  = solCjto3 (fromList ::  [Int] -> IM.IntMap Int)
solSet3     = solCjto3 (fromList ::  [Int] -> S.Set Int)
solIntSet3  = solCjto3 (fromList ::  [Int] -> IS.IntSet)
solListComprehension3 target xs =
  product $ head [[x,y,z] | x <- xs, y <- xs, z <- xs, x + y + z == target]




main :: IO ()
main = do
  input <- map read . lines <$> readFile "input01"
  putStrLn $ "Input 1 size: " ++ show (length input)

  let t1 = 2020
  mapM_ (print . (\f -> f t1 input)) [
      solListComprehension
    , solLista
    , solSeq
    , solVector
    , solArray
    , solMap
    , solHashMap
    , solIntMap
    , solSet
    , solIntSet
    , solSeqBorder]

  mapM_ (print . (\f -> f t1 input)) [
      solListComprehension3
    , solLista3
    , solSeq3
    , solVector3
    , solArray3
    , solMap3
    , solHashMap3
    , solIntMap3
    , solSet3
    , solIntSet3
    ]
