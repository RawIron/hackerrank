{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- |
-- hackerrank does _not_ support the
-- Haskell package
--  Data.Vector.Algorithms
-- 
-- this module contains
-- _only_ a copy of the code
-- which is required to use
--  the @sort@ functions from
--  Data.Vector.Algorithms.Intro


module VectorSort where

import Prelude hiding (length)
import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Word (Word)
import Data.Vector.Generic.Mutable hiding (read, drop)
import qualified Data.Vector.Internal.Check as Ck


--
-- >> Data.Vector.Algorithms.Heap
--

-- | Sorts a portion of an array [l,u) using a custom ordering
heapSortByBounds
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m ()
heapSortByBounds cmp a l u
  | len < 2   = return ()
  | len == 2  = sort2ByOffset cmp a l
  | len == 3  = sort3ByOffset cmp a l
  | len == 4  = sort4ByOffset cmp a l
  | otherwise = heapify cmp a l u >> sortHeap cmp a l (l+4) u >> sort4ByOffset cmp a l
 where len = u - l
{-# INLINE heapSortByBounds #-}

-- | Constructs a heap in a portion of an array [l, u), using the values therein.
--
-- Note: 'heapify' is more efficient than constructing a heap by repeated
-- insertion. Repeated insertion has complexity O(n*log n) while 'heapify' is able
-- to construct a heap in O(n), where n is the number of elements in the heap.
heapify
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m ()
heapify cmp a l u = loop $ (len - 1) `shiftR` 2
  where
 len = u - l
 loop k
   | k < 0     = return ()
   | otherwise = unsafeRead a (l+k) >>= \e ->
                   siftByOffset cmp a e l k len >> loop (k - 1)
{-# INLINE heapify #-}

-- | Given a heap stored in a portion of an array [l,u), sorts the
-- highest values into [m,u). The elements in [l,m) are not in any
-- particular order.
sortHeap
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower heap index, l
  -> Int -- ^ lower bound of final sorted portion, m
  -> Int -- ^ upper heap index, u
  -> m ()
sortHeap cmp a l m u = loop (u-1) >> unsafeSwap a l m
 where
 loop k
   | m < k     = pop cmp a l k >> loop (k-1)
   | otherwise = return ()
{-# INLINE sortHeap #-}

-- | Given a heap stored in a portion of an array [l,u), swaps the
-- top of the heap with the element at u and rebuilds the heap.
pop
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower heap index, l
  -> Int -- ^ upper heap index, u
  -> m ()
pop cmp a l u = popTo cmp a l u u
{-# INLINE pop #-}

-- | Given a heap stored in a portion of an array [l,u) swaps the top
-- of the heap with the element at position t, and rebuilds the heap.
popTo
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower heap index, l
  -> Int -- ^ upper heap index, u
  -> Int -- ^ index to pop to, t
  -> m ()
popTo cmp a l u t = do al <- unsafeRead a l
                       at <- unsafeRead a t
                       unsafeWrite a t al
                       siftByOffset cmp a at l 0 (u - l)
{-# INLINE popTo #-}

-- Rebuilds a heap with a hole in it from start downwards. Afterward,
-- the heap property should apply for [start + off, len + off). val
-- is the new value to be put in the hole.
siftByOffset :: (PrimMonad m, MVector v e)
             => Comparison e -> v (PrimState m) e -> e -> Int -> Int -> Int -> m ()
siftByOffset cmp a val off start len = sift val start len
 where
 sift val root len
   | child < len = do (child', ac) <- maximumChild cmp a off child len
                      case cmp val ac of
                        LT -> unsafeWrite a (root + off) ac >> sift val child' len
                        _  -> unsafeWrite a (root + off) val
   | otherwise = unsafeWrite a (root + off) val
  where child = root `shiftL` 2 + 1
{-# INLINE siftByOffset #-}


-- Finds the maximum child of a heap node, given the indx of the first child.
maximumChild :: (PrimMonad m, MVector v e)
             => Comparison e -> v (PrimState m) e -> Int -> Int -> Int -> m (Int,  e)
maximumChild cmp a off child1 len
  | child4 < len = do ac1 <- unsafeRead a (child1 + off)
                      ac2 <- unsafeRead a (child2 + off)
                      ac3 <- unsafeRead a (child3 + off)
                      ac4 <- unsafeRead a (child4 + off)
                      return $ case cmp ac1 ac2 of
                                 LT -> case cmp ac2 ac3 of
                                         LT -> case cmp ac3 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child3, ac3)
                                         _  -> case cmp ac2 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child2, ac2)
                                 _  -> case cmp ac1 ac3 of
                                         LT -> case cmp ac3 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child3, ac3)
                                         _  -> case cmp ac1 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child1, ac1)
  | child3 < len = do ac1 <- unsafeRead a (child1 + off)
                      ac2 <- unsafeRead a (child2 + off)
                      ac3 <- unsafeRead a (child3 + off)
                      return $ case cmp ac1 ac2 of
                                 LT -> case cmp ac2 ac3 of
                                         LT -> (child3, ac3)
                                         _  -> (child2, ac2)
                                 _  -> case cmp ac1 ac3 of
                                         LT -> (child3, ac3)
                                         _  -> (child1, ac1)
  | child2 < len = do ac1 <- unsafeRead a (child1 + off)
                      ac2 <- unsafeRead a (child2 + off)
                      return $ case cmp ac1 ac2 of
                                 LT -> (child2, ac2)
                                 _  -> (child1, ac1)
  | otherwise    = do ac1 <- unsafeRead a (child1 + off) ; return (child1, ac1)
 where
 child2 = child1 + 1
 child3 = child1 + 2
 child4 = child1 + 3
{-# INLINE maximumChild #-}
--
-- << Data.Vector.Algorithms.Heap
--


--
-- >> Data.Vector.Algorithms.Optimal
--

-- | Sorts the elements at the positions 'off' and 'off + 1' in the given
-- array using the comparison.
sort2ByOffset :: (PrimMonad m, MVector v e)
              => Comparison e -> v (PrimState m) e -> Int -> m ()
sort2ByOffset cmp a off = sort2ByIndex cmp a off (off + 1)
{-# INLINABLE sort2ByOffset #-}

-- | Sorts the three elements starting at the given offset in the array.
sort3ByOffset :: (PrimMonad m, MVector v e)
              => Comparison e -> v (PrimState m) e -> Int -> m ()
sort3ByOffset cmp a off = sort3ByIndex cmp a off (off + 1) (off + 2)
{-# INLINABLE sort3ByOffset #-}

-- | Sorts the four elements beginning at the offset.
sort4ByOffset :: (PrimMonad m, MVector v e)
              => Comparison e -> v (PrimState m) e -> Int -> m ()
sort4ByOffset cmp a off = sort4ByIndex cmp a off (off + 1) (off + 2) (off + 3)
{-# INLINABLE sort4ByOffset #-}

-- | Sorts the elements at the two given indices using the comparison. This
-- is essentially a compare-and-swap, although the first index is assumed to
-- be the 'lower' of the two.
sort2ByIndex :: (PrimMonad m, MVector v e)
             => Comparison e -> v (PrimState m) e -> Int -> Int -> m ()
--sort2ByIndex cmp a i j = Ck.checkIndex Ck.Unsafe (i) (length a)
--                       $ Ck.checkIndex Ck.Unsafe (j) (length a) $  do
sort2ByIndex cmp a i j = do
  a0 <- unsafeRead a i
  a1 <- unsafeRead a j
  case cmp a0 a1 of
    GT -> unsafeWrite a i a1 >> unsafeWrite a j a0
    _  -> return ()
{-# INLINABLE sort2ByIndex #-}


-- | Sorts the elements at the three given indices. The indices are assumed
-- to be given from lowest to highest, so if 'l < m < u' then
-- 'sort3ByIndex cmp a m l u' essentially sorts the median of three into the
-- lowest position in the array.
sort3ByIndex :: (PrimMonad m, MVector v e)
             => Comparison e -> v (PrimState m) e -> Int -> Int -> Int -> m ()
--sort3ByIndex cmp a i j k = Ck.checkIndex Ck.Unsafe "sort3ByIndex" (i) (length a)
--                         $ CHECK_INDEX("sort3ByIndex", j, length a)
--                         $ CHECK_INDEX("sort3ByIndex", k, length a) $ do
sort3ByIndex cmp a i j k = do
  a0 <- unsafeRead a i
  a1 <- unsafeRead a j
  a2 <- unsafeRead a k
  case cmp a0 a1 of
    GT -> case cmp a0 a2 of
            GT -> case cmp a2 a1 of
                    LT -> do unsafeWrite a i a2
                             unsafeWrite a k a0
                    _  -> do unsafeWrite a i a1
                             unsafeWrite a j a2
                             unsafeWrite a k a0
            _  -> do unsafeWrite a i a1
                     unsafeWrite a j a0
    _  -> case cmp a1 a2 of
            GT -> case cmp a0 a2 of
                    GT -> do unsafeWrite a i a2
                             unsafeWrite a j a0
                             unsafeWrite a k a1
                    _  -> do unsafeWrite a j a2
                             unsafeWrite a k a1
            _  -> return ()
{-# INLINABLE sort3ByIndex #-}

-- | Sorts the elements at the four given indices. Like the 2 and 3 element
-- versions, this assumes that the indices are given in increasing order, so
-- it can be used to sort medians into particular positions and so on.
sort4ByIndex :: (PrimMonad m, MVector v e)
             => Comparison e -> v (PrimState m) e -> Int -> Int -> Int -> Int -> m ()
--sort4ByIndex cmp a i j k l = CHECK_INDEX("sort4ByIndex", i, length a)
--                          $ CHECK_INDEX("sort4ByIndex", j, length a)
--                           $ CHECK_INDEX("sort4ByIndex", k, length a)
--                           $ CHECK_INDEX("sort4ByIndex", l, length a) $ do
sort4ByIndex cmp a i j k l = do
  a0 <- unsafeRead a i
  a1 <- unsafeRead a j
  a2 <- unsafeRead a k
  a3 <- unsafeRead a l
  case cmp a0 a1 of
    GT -> case cmp a0 a2 of
            GT -> case cmp a1 a2 of
                    GT -> case cmp a1 a3 of
                            GT -> case cmp a2 a3 of
                                    GT -> do unsafeWrite a i a3
                                             unsafeWrite a j a2
                                             unsafeWrite a k a1
                                             unsafeWrite a l a0
                                    _  -> do unsafeWrite a i a2
                                             unsafeWrite a j a3
                                             unsafeWrite a k a1
                                             unsafeWrite a l a0
                            _  -> case cmp a0 a3 of
                                    GT -> do unsafeWrite a i a2
                                             unsafeWrite a j a1
                                             unsafeWrite a k a3
                                             unsafeWrite a l a0
                                    _  -> do unsafeWrite a i a2
                                             unsafeWrite a j a1
                                             unsafeWrite a k a0
                                             unsafeWrite a l a3
                    _ -> case cmp a2 a3 of
                           GT -> case cmp a1 a3 of
                                   GT -> do unsafeWrite a i a3
                                            unsafeWrite a j a1
                                            unsafeWrite a k a2
                                            unsafeWrite a l a0
                                   _  -> do unsafeWrite a i a1
                                            unsafeWrite a j a3
                                            unsafeWrite a k a2
                                            unsafeWrite a l a0
                           _  -> case cmp a0 a3 of
                                   GT -> do unsafeWrite a i a1
                                            unsafeWrite a j a2
                                            unsafeWrite a k a3
                                            unsafeWrite a l a0
                                   _  -> do unsafeWrite a i a1
                                            unsafeWrite a j a2
                                            unsafeWrite a k a0
                                            -- unsafeWrite a l a3
            _  -> case cmp a0 a3 of
                    GT -> case cmp a1 a3 of
                            GT -> do unsafeWrite a i a3
                                     -- unsafeWrite a j a1
                                     unsafeWrite a k a0
                                     unsafeWrite a l a2
                            _  -> do unsafeWrite a i a1
                                     unsafeWrite a j a3
                                     unsafeWrite a k a0
                                     unsafeWrite a l a2
                    _  -> case cmp a2 a3 of
                            GT -> do unsafeWrite a i a1
                                     unsafeWrite a j a0
                                     unsafeWrite a k a3
                                     unsafeWrite a l a2
                            _  -> do unsafeWrite a i a1
                                     unsafeWrite a j a0
                                     -- unsafeWrite a k a2
                                     -- unsafeWrite a l a3
    _  -> case cmp a1 a2 of
            GT -> case cmp a0 a2 of
                    GT -> case cmp a0 a3 of
                            GT -> case cmp a2 a3 of
                                    GT -> do unsafeWrite a i a3
                                             unsafeWrite a j a2
                                             unsafeWrite a k a0
                                             unsafeWrite a l a1
                                    _  -> do unsafeWrite a i a2
                                             unsafeWrite a j a3
                                             unsafeWrite a k a0
                                             unsafeWrite a l a1
                            _  -> case cmp a1 a3 of
                                    GT -> do unsafeWrite a i a2
                                             unsafeWrite a j a0
                                             unsafeWrite a k a3
                                             unsafeWrite a l a1
                                    _  -> do unsafeWrite a i a2
                                             unsafeWrite a j a0
                                             unsafeWrite a k a1
                                             -- unsafeWrite a l a3
                    _  -> case cmp a2 a3 of
                            GT -> case cmp a0 a3 of
                                    GT -> do unsafeWrite a i a3
                                             unsafeWrite a j a0
                                             -- unsafeWrite a k a2
                                             unsafeWrite a l a1
                                    _  -> do -- unsafeWrite a i a0
                                             unsafeWrite a j a3
                                             -- unsafeWrite a k a2
                                             unsafeWrite a l a1
                            _  -> case cmp a1 a3 of
                                    GT -> do -- unsafeWrite a i a0
                                             unsafeWrite a j a2
                                             unsafeWrite a k a3
                                             unsafeWrite a l a1
                                    _  -> do -- unsafeWrite a i a0
                                             unsafeWrite a j a2
                                             unsafeWrite a k a1
                                             -- unsafeWrite a l a3
            _  -> case cmp a1 a3 of
                    GT -> case cmp a0 a3 of
                            GT -> do unsafeWrite a i a3
                                     unsafeWrite a j a0
                                     unsafeWrite a k a1
                                     unsafeWrite a l a2
                            _  -> do -- unsafeWrite a i a0
                                     unsafeWrite a j a3
                                     unsafeWrite a k a1
                                     unsafeWrite a l a2
                    _  -> case cmp a2 a3 of
                            GT -> do -- unsafeWrite a i a0
                                     -- unsafeWrite a j a1
                                     unsafeWrite a k a3
                                     unsafeWrite a l a2
                            _  -> do -- unsafeWrite a i a0
                                     -- unsafeWrite a j a1
                                     -- unsafeWrite a k a2
                                     -- unsafeWrite a l a3
                                     return ()
{-# INLINABLE sort4ByIndex #-}
--
-- << Data.Vector.Algorithms.Optimal
--


--
-- >> Data.Vector.Algorithms.Insertion
--

-- | Sorts the portion of an array delimited by [l,u)
insertSortByBounds :: (PrimMonad m, MVector v e)
             => Comparison e -> v (PrimState m) e -> Int -> Int -> m ()
insertSortByBounds cmp a l u
  | len < 2   = return ()
  | len == 2  = sort2ByOffset cmp a l
  | len == 3  = sort3ByOffset cmp a l
  | len == 4  = sort4ByOffset cmp a l
  | otherwise = sort4ByOffset cmp a l >> sortByBounds' cmp a l (l + 4) u
 where
 len = u - l
{-# INLINE insertSortByBounds #-}

-- | Sorts the portion of the array delimited by [l,u) under the assumption
-- that [l,m) is already sorted.
sortByBounds' :: (PrimMonad m, MVector v e)
              => Comparison e -> v (PrimState m) e -> Int -> Int -> Int -> m ()
sortByBounds' cmp a l m u = sort m
 where
 sort i
   | i < u     = do v <- unsafeRead a i
                    insert cmp a l v i
                    sort (i+1)
   | otherwise = return ()
{-# INLINE sortByBounds' #-}

-- Given a sorted array in [l,u), inserts val into its proper position,
-- yielding a sorted [l,u]
insert :: (PrimMonad m, MVector v e)
       => Comparison e -> v (PrimState m) e -> Int -> e -> Int -> m ()
insert cmp a l = loop
 where
 loop val j
   | j <= l    = unsafeWrite a l val
   | otherwise = do e <- unsafeRead a (j - 1)
                    case cmp val e of
                      LT -> unsafeWrite a j e >> loop val (j - 1)
                      _  -> unsafeWrite a j val
{-# INLINE insert #-}
--
-- << Data.Vector.Algorithms.Insertion
--


--
-- >> Data.Vector.Algorithms.Common
--

-- | A type of comparisons between two values of a given type.
type Comparison e = e -> e -> Ordering

midPoint :: Int -> Int -> Int
midPoint a b =
  toInt $ (toWord a + toWord b) `div` 2
  where
    toWord :: Int -> Word
    toWord = fromIntegral

    toInt :: Word -> Int
    toInt = fromIntegral
{-# INLINE midPoint #-}

-- Adapted from Andrew Martin's uniquqMutable in the primitive-sort package
uniqueMutableBy :: forall m v a . (PrimMonad m, MVector v a)
  => Comparison a -> v (PrimState m) a -> m (v (PrimState m) a)
uniqueMutableBy cmp mv = do
  let !len = basicLength mv
  if len > 1
    then do
      !a0 <- unsafeRead mv 0
      let findFirstDuplicate :: a -> Int -> m Int
          findFirstDuplicate !prev !ix = if ix < len
            then do
              a <- unsafeRead mv ix
              if cmp a prev == EQ
                then return ix
                else findFirstDuplicate a (ix + 1)
            else return ix
      dupIx <- findFirstDuplicate a0 1
      if dupIx == len
        then return mv
        else do
          let deduplicate :: a -> Int -> Int -> m Int
              deduplicate !prev !srcIx !dstIx = if srcIx < len
                then do
                  a <- unsafeRead mv srcIx
                  if cmp a prev == EQ
                    then deduplicate a (srcIx + 1) dstIx
                    else do
                      unsafeWrite mv dstIx a
                      deduplicate a (srcIx + 1) (dstIx + 1)
                else return dstIx
          !a <- unsafeRead mv dupIx
          !reducedLen <- deduplicate a (dupIx + 1) dupIx
          resizeVector mv reducedLen
    else return mv
{-# INLINABLE uniqueMutableBy #-}

-- Used internally in uniqueMutableBy: copies the elements of a vector to one
-- of a smaller size.
resizeVector
  :: (MVector v a, PrimMonad m)
  =>  v (PrimState m) a -> Int -> m (v (PrimState m) a)
resizeVector !src !sz = do
  dst <- unsafeNew sz
  copyToSmaller dst src
  pure dst
{-# inline resizeVector #-}

-- Used internally in resizeVector: copy a vector from a larger to
-- smaller vector. Should not be used if the source vector
-- is smaller than the target vector.
copyToSmaller
  :: (MVector v a, PrimMonad m)
  => v (PrimState m) a -> v (PrimState m) a -> m ()
copyToSmaller !dst !src = stToPrim $ do_copy 0
    where
      !n = basicLength dst

      do_copy i | i < n = do
                            x <- basicUnsafeRead src i
                            basicUnsafeWrite dst i x
                            do_copy (i+1)
                | otherwise = return ()
--
-- << Data.Vector.Algorithms.Common
--


--
-- >> Data.Vector.Algorithms.Intro
--

-- | Sorts an entire array using the default ordering.
sort :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> m ()
sort = sortBy compare
{-# INLINABLE sort #-}

-- | A variant on `sort` that returns a vector of unique elements.
sortUniq :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> m (v (PrimState m) e)
sortUniq = sortUniqBy compare
{-# INLINABLE sortUniq #-}

-- | A variant on `sortBy` which returns a vector of unique elements.
sortBy :: (PrimMonad m, MVector v e) => Comparison e -> v (PrimState m) e -> m ()
sortBy cmp a = sortByBounds cmp a 0 (length a)
{-# INLINE sortBy #-}

-- | Sorts an entire array using a custom ordering returning a vector of
-- the unique elements.
sortUniqBy :: (PrimMonad m, MVector v e) => Comparison e -> v (PrimState m) e -> m (v (PrimState m) e)
sortUniqBy cmp a = do
  sortByBounds cmp a 0 (length a)
  uniqueMutableBy cmp a
{-# INLINE sortUniqBy #-}

-- | Sorts a portion of an array [l,u) using a custom ordering
sortByBounds
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m ()
sortByBounds cmp a l u
  | len < 2   = return ()
  | len == 2  = sort2ByOffset cmp a l
  | len == 3  = sort3ByOffset cmp a l
  | len == 4  = sort4ByOffset cmp a l
  | otherwise = introsort cmp a (ilg len) l u
 where len = u - l
{-# INLINE sortByBounds #-}

-- Internal version of the introsort loop which allows partial
-- sort functions to call with a specified bound on iterations.
introsort :: (PrimMonad m, MVector v e)
          => Comparison e -> v (PrimState m) e -> Int -> Int -> Int -> m ()
introsort cmp a i l u = sort i l u >> insertSortByBounds cmp a l u
 where
 sort 0 l u = heapSortByBounds cmp a l u
 sort d l u
   | len < threshold = return ()
   | otherwise = do sort3ByIndex cmp a c l (u-1) -- sort the median into the lowest position
                    p <- unsafeRead a l
                    mid <- partitionBy cmp a p (l+1) u
                    unsafeSwap a l (mid - 1)
                    sort (d-1) mid u
                    sort (d-1) l   (mid - 1)
  where
  len = u - l
  c   = midPoint u l
{-# INLINE introsort #-}

partitionBy :: forall m v e. (PrimMonad m, MVector v e)
            => Comparison e -> v (PrimState m) e -> e -> Int -> Int -> m Int
partitionBy cmp a = partUp
 where
 partUp :: e -> Int -> Int -> m Int
 partUp p l u
   | l < u = do e <- unsafeRead a l
                case cmp e p of
                  LT -> partUp p (l+1) u
                  _  -> partDown p l (u-1)
   | otherwise = return l

 partDown :: e -> Int -> Int -> m Int
 partDown p l u
   | l < u = do e <- unsafeRead a u
                case cmp p e of
                  LT -> partDown p l (u-1)
                  _  -> unsafeSwap a l u >> partUp p (l+1) u
   | otherwise = return l
{-# INLINE partitionBy #-}

-- computes the number of recursive calls after which heapsort should
-- be invoked given the lower and upper indices of the array to be sorted
ilg :: Int -> Int
ilg m = 2 * loop m 0
 where
 loop 0 !k = k - 1
 loop n !k = loop (n `shiftR` 1) (k+1)

-- the size of array at which the introsort algorithm switches to insertion sort
threshold :: Int
threshold = 18
{-# INLINE threshold #-}
--
-- << Data.Vector.Algorithms.Intro
--
