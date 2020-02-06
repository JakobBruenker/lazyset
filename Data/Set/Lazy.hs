{-| 
Module      : LazySet
Description : A truly lazy Set. 
Copyright   : (c) Carlos Freund, 2016
License     : MIT
Maintainer  : carlosfreund@gmail.com
Stability   : experimental

A Set that can be created from lazy, ordered, infinite lists. 
-}
module Data.Set.Lazy (
    
    -- * Types
    LazySet,

    -- * Query    
    member,
    lookup,
    null,
    size,
    
    -- * Combine
    spanAntitone ,
    union,

    -- * Build
    empty,
    fromAscList,
    growFromAscList,
    fromList,
    fromDescList,
    build,
    
    -- * Export
    toList,

    )where 

import Prelude hiding (lookup, null)
import Data.Foldable (asum)
import qualified Data.List as List
import qualified Data.Set as NSet
import qualified Data.List.Ordered as OList
import Data.Maybe (isJust)
import Data.Ord

-- A bounded consists of its greatest member as well as a set of all its
-- members that are smaller
data BoundedSet a = BoundedSet { bsSet :: !(NSet.Set a)
                               , bsMax :: !a
                               } deriving (Eq)

newtype LazySet a = LazySet [BoundedSet a]
    deriving (Eq)

instance (Ord a, Show a) => Show (LazySet a) where
  showsPrec n = showsPrec n . toList

-- | Checks if the value is a member of the 'LazySet'. 
-- Performance: O(m)=log m  
--Where m is the position of the element beeing searched for. 
-- This only applies after the element has been fetched from the underlying list.
member :: Ord a => a -> LazySet a ->  Bool
member e set = isJust $ lookup e set             
    
-- | Searches for a value in a Set. If it can not be found returns 'Nothing' otherwise 
-- Returns 'Just a' if it can find it. The returned value will be the one from the set, not the one that was passed. 
lookup :: Ord a => a -> LazySet a -> Maybe a
lookup e (LazySet ls) = go ls
    where go (BoundedSet set me : bsets) = case compare e me of
            GT -> go bsets
            EQ -> Just me
            LT -> NSet.lookupLE e set
          go [] = Nothing

-- | Returns true if the 'LazySet' is empty. 
null :: LazySet a -> Bool
null (LazySet []) = True
null _ = False

-- | Returns the size of the set. Do not use this on infinite Sets.
size :: Ord a => LazySet a -> Int
size = length . toList 

-- | Splits the 'LazySet' into two parts. 
-- The first containing all consecutive elements of the Set where the predicate applies.
-- The second contains the (infinite) rest. 
spanAntitone :: Ord a => (a -> Bool) -> LazySet a -> (LazySet a, LazySet a)
spanAntitone p (LazySet sets) = let
    (lesser, (BoundedSet middle middleHighest : higher)) =
      List.span (p . bsMax) sets
    (middleLesser, middleHigher) = NSet.spanAntitone p middle
    middleLesserBounded = BoundedSet middleLesserNoMax middleLesserHighest
    (middleLesserHighest, middleLesserNoMax) = NSet.deleteFindMax middleLesser
    in (LazySet (lesser ++ [middleLesserBounded]),
       LazySet (BoundedSet middleHigher middleHighest : higher))
    
-- | Union of two LazySets.     
union :: Ord a => LazySet a -> LazySet a -> LazySet a
union s1 s2 = let  
    in fromList $ OList.union (toList s1) (toList s2)

-- | Create an 'empty' 'LazySet'.    
empty :: Ord a => LazySet a    
empty = fromList []
    
  
-- | Builds a 'LazySet' from an ascending ordered list.
-- If the list is not ordered an error is thrown. 
fromAscList :: Ord a => [a] -> LazySet a
fromAscList = growFromAscList 2.0               


-- | Like 'fromAscList' but with a custom growth-factor. 
growFromAscList :: Ord a => 
    Float   -- ^ The factor by which the subtrees grow. 
    --Must be >= 1.0. A growth of 1.0 makes the 'LazySet' behave like a List. 
    -- The higher it is set, the more it behaves like a 'Data.Set'.
    -- The downside of a higher growth-factor is that bigger batches are extracted from the source-list at once. 
    -> [a]  -- ^ An ascending List
    -> LazySet a     
growFromAscList growth _ | growth < 1.0 = error "growth must be at least 1" 
growFromAscList growth xs = LazySet (build 0 growth (checkDir xs))
    where checkDir (a:b:s) | a > b = error "Elements must be ascending." 
          checkDir (x:xs) = x : checkDir xs
          checkDir [] = []

-- | Alias for 'fromAscList'.
fromList :: Ord a => [a] -> LazySet a
fromList = fromAscList
          
-- | Create a 'LazSet' from a descending list.           
fromDescList :: Ord a => [a] -> LazySet (Down a)
fromDescList xs = fromAscList (map Down xs)
    
-- | Kind of internal. 
build :: Ord a => Int -- ^ starting-depth 
    -> Float -- ^ growth-factor
    -> [a] -- ^Ascending source-list 
    -> [BoundedSet a]
build _ _ [] = []
build level growth xs = let 
    (thisLevel', rest) =
      List.splitAt (ceiling (growth^level) - 1) xs
    ((thisLevel, maxElem), elementsFurtherDown) = case rest of
      y:ys -> ((thisLevel', y), ys)
      [] -> (unsnoc thisLevel', [])
    thisLevelBounded = BoundedSet (NSet.fromAscList thisLevel) maxElem
    in thisLevelBounded : build (level + 1) growth elementsFurtherDown
    where unsnoc [x] = ([], x)
          unsnoc (x:xs) = let (a, b) = unsnoc xs in (x:a, b)
      
-- | List with all elements in order.
toList :: Ord a => LazySet a -> [a]
toList (LazySet sets) =
  concatMap (\bs -> NSet.toAscList (bsSet bs) ++ [bsMax bs]) sets
