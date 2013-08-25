-- |
-- Module      :  Data.Counts
-- Copyright   :  (c) 2012 Jan Snajder
-- License     :  BSD-3 (see the LICENSE file)
--
-- Maintainer  :  Jan Snajder <jan.snajder@fer.hr>
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple data structure for counting values (similar to Data.Multiset).
--
-------------------------------------------------------------------------------

module Data.Counts (
  Counts,
  fromList,
  fromSet,
  toList,
  toSet,
  elems,
  counts,
  total,
  size,
  countOf,
  empty,
  set,
  inc,
  dec,
  remove,
  removeBelow,
  removeList,
  removeSet,
  member,
  union,
  difference,
  sumCounts,
  fromCounts,
  probOf,
  probs,
  logProb,
  logProbs) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Ord (comparing)
import Data.List (sortBy,foldl')

data Counts a = Counts {
  counts_ :: M.Map a Int,
  total   :: !Int }
  deriving Eq

instance Show a => Show (Counts a) where
  show cs = "fromCounts " ++ show (counts cs)

instance (Ord a, Read a) => Read (Counts a) where
  readsPrec _ s | p == "fromCounts " = 
                    case readsPrec 0 s2 of
                      [(cs,s3)] -> [(fromCounts cs,s3)]
                      []        -> []
                | otherwise = []
     where (p,s2) = splitAt 11 s

empty :: Ord a => Counts a
empty = fromCounts []

union :: Ord a => Counts a -> Counts a -> Counts a
union cs1 cs2 = Counts { counts_ = m, total = total cs1 + total cs2 }
  where m = M.unionWith (+) (counts_ cs1) (counts_ cs2)

sumCounts :: Ord a => [Counts a] -> Counts a
sumCounts = foldl1 union

difference :: Ord a => Counts a -> Counts a -> Counts a
difference cs1 cs2 = Counts { counts_ = m, total = sum $ M.elems m }
  where m = M.differenceWith f (counts_ cs1) (counts_ cs2)
        f c1 c2 = let d=c1-c2 in if d<=0 then Nothing else Just d

size :: Counts a -> Int
size = M.size . counts_

counts :: Counts a -> [(a,Int)] 
counts = M.toAscList . counts_

fromList :: (Ord a) => [a] -> Counts a
fromList xs = fromCounts $ zip xs (repeat 1)

fromSet :: (Ord a) => S.Set a -> Counts a
fromSet xs = Counts {
  counts_ = m, total = sum $ M.elems m }
  where m = M.fromAscList $ zip (S.toAscList xs) (repeat 1)

toList :: (Ord a) => Counts a -> [a]
toList = M.keys . counts_

toSet :: (Ord a) => Counts a -> S.Set a
toSet = M.keysSet . counts_

elems :: (Ord a) => Counts a -> [a]
elems = toList

fromListWith' :: (Ord k) => (a -> a -> a) -> [(k,a)] -> M.Map k a
fromListWith' f = foldl' (\m (k,x) -> M.insertWith f k x m) M.empty

fromCounts :: (Ord a) => [(a,Int)] -> Counts a
fromCounts xs = Counts {
  counts_ = m, total = sum $ M.elems m} 
  where m = M.fromListWith (+) . filter ((>0).snd) $ xs

countOf :: (Ord a) => a -> Counts a -> Int
countOf x cs = M.findWithDefault 0 x (counts_ cs)

set :: (Ord a) => a -> Int -> Counts a -> Counts a
set x c cs = Counts {
  counts_ = if c<=0 then M.delete x (counts_ cs) 
            else M.insert x c (counts_ cs),
  total   = total cs + c - M.findWithDefault 0 x (counts_ cs)}

remove :: (Ord a) => a -> Counts a -> Counts a
remove x cs = set x 0 cs

removeBelow :: (Ord a) => Int -> Counts a -> Counts a
removeBelow t cs =
  Counts { counts_ = m, total = sum $ M.elems m }
  where m = M.filter (>=t) (counts_ cs)

removeList :: (Ord a) => [a] -> Counts a -> Counts a
removeList xs cs = foldl (flip $ remove) cs xs

removeSet :: (Ord a) => S.Set a -> Counts a -> Counts a
removeSet xs cs = S.fold remove cs xs

member :: (Ord a) => a -> Counts a -> Bool
member x = M.member x . counts_

inc :: (Ord a) => a -> Counts a -> Counts a
inc x cs = Counts {
  counts_ = M.adjust (+1) x (counts_ cs),
  total   = total cs + 1}

dec :: (Ord a) => a -> Counts a -> Counts a
dec x cnts@(Counts {counts_ = cs, total = t}) = 
  case M.lookup x cs of
    Nothing -> cnts
    Just 1  -> Counts { counts_ = M.delete x cs, total = t - 1 }
    Just c  -> Counts { counts_ = M.insert x (c-1) cs, total = t - 1}

probOf :: (Ord a) => a -> Counts a -> Double
probOf x cs = realToFrac (countOf x cs) / realToFrac (total cs)

logProb :: (Ord a) => a -> Counts a -> Double
logProb a = log . probOf a

probs :: (Ord a) => Counts a -> [(a,Double)]
probs cs = 
  map (\(x,c) -> (x, realToFrac c / realToFrac (total cs))) (counts cs)

logProbs :: (Ord a) => Counts a -> [(a,Double)]
logProbs = map (\(x,c) -> (x,log c)) . probs

