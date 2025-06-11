-- |
-- Module: Data.List1
-- Description: Helpers for working with NonEmpty lists.
-- Copyright: (c) Melanie Phoenix Brown, 2023-2025
-- Maintainer: brown.m@proton.me
module Data.List1 (
  List1 {- ((:|)) -},
  pattern Sole,
  pattern (:||),
  pattern (:?),
  (<|),
  (|>),
  (|:),
  (||:),
  (?:),
  list1,
  toList,
  unList1,
  onList,
  asList,
  ifList1,
  withList1,
  whenList1,
  has01,
  has1Plus,
  uncons,
  unsnoc,
  (++),
  reverse,
  head,
  tail,
  init,
  last,
  inits,
  tails,
  take,
  drop,
  takeWhile,
  dropWhile,
  delete,
  deleteBy,
  (\\),
  filter,
  span,
  break,
  partition,
  splitAt,
  index,
  elem,
  notElem,
  elemIndex,
  elemIndices,
  find,
  findIndex,
  findIndices,
  (!?),
  lookup,
  map,
  foldMap1,
  mapMaybe,
  catMaybes,
  zip,
  zipWith,
  unzip,
  accuml,
  accumr,
  scanl,
  scanl',
  scanl1,
  scanl1',
  scanr,
  scanr1,
  unfoldr,
  build1,
  sort,
  sortOn,
  sortBy,
  group,
  groupOn,
  groupBy,
  intersect,
  intersectOn,
  intersectBy,
  union,
  unionOn,
  unionBy,
  nub,
  nubOn,
  nubBy,
  maximum,
  maximumOf,
  maximumOn,
  maximumBy,
  minimum,
  minimumOf,
  minimumOn,
  minimumBy,
  iterate,
  iterated,
  repeat,
  replicate,
  cycle,
  intersperse,
  intercalate,
  transpose,
  subsequences,
  windows,
  consecutiveSubsequences,
  permutations,
  diagonally,
  diagonals,
  insertions,
  -- zipWithTruncate,
  -- zipWithTruncate',
  -- zipWithTruncate1,
) where

import Control.Applicative (Alternative (empty), Applicative (pure))
import Control.Monad (ap, guard, join, liftM2, (<=<), (=<<), (>>=))
import Control.Monad.Fix (fix)
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Bits ((.&.))
import Data.Bool (Bool (..), not, otherwise, (||))
import Data.Eq (Eq (..))
import Data.Foldable qualified as Fold
import Data.Foldable1 (Foldable1 (foldMap1))
import Data.Function (const, flip, id, on, ($), (.))
import Data.Functor (Functor, fmap, ($>), (<$>), (<&>))
import Data.Int (Int)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (Maybe (..), fromJust, fromMaybe, isJust, maybe)
import Data.Ord (Ord (..), Ordering (..), comparing)
import Data.Semigroup (Semigroup ((<>)))
import Data.Tuple (fst, snd)
import Data.Word (Word)
import GHC.Enum (Enum (pred, succ))
import GHC.Err (error)
import GHC.Num qualified as Num
import GHC.Real (Integral)
import GHC.Stack (HasCallStack)
import Prelude ()

infixr 5 {- :|, -} :||, :?, |:, ||:, ?:

infixl 4 <|, |>

type List1 = NonEmpty

-- data List1 x = x :| [x]
--   deriving
--     ( Eq
--     , Ord
--     , Show
--     , Read
--     , Data
--     , Generic
--     , Generic1
--     , Functor
--     , Foldable
--     , Traversable
--     )

-- | Match a singleton 'List1'.
pattern Sole :: x -> List1 x
pattern Sole x = x :| []

-- | Match a 'List1' of length at least 2.
pattern (:||) :: x -> List1 x -> List1 x
pattern x :|| y <- (x :| (list1 -> Just y))
  where
    x :|| ~(y :| ys) = x :| (y : ys)

{-# COMPLETE Sole, (:||) #-}

-- | Isomorphic to '(:|)', but instead with a 'Maybe' 'List1'.
pattern (:?) :: x -> Maybe (List1 x) -> List1 x
pattern x :? y <- (x :| ~(list1 -> y))
  where
    x :? y = maybe (Sole x) (x :||) y

{-# COMPLETE (:?) #-}

-- | Prepend a 'List1' to a list.
(<|) :: List1 x -> [x] -> List1 x
(x :| xs) <| ys = x :| (xs <> ys)

-- | Append a 'List1' to a list.
(|>) :: [x] -> List1 x -> List1 x
xs |> ys = has01 xs ys \(x :| zs) -> x :|| (zs |> ys)

-- | Append an element to a list. C.f. '(:|)'.
(|:) :: [x] -> x -> List1 x
ys |: x = ys |> Sole x

-- | Append an element to a 'List1'. C.f. '(:||)'.
(||:) :: List1 x -> x -> List1 x
ys ||: x = ys <> Sole x

-- | Append an element to a 'Maybe' 'List1'. C.f. '(:?)'.
(?:) :: Maybe (List1 x) -> x -> List1 x
ys ?: x = maybe (Sole x) (||: x) ys

-- | Together with 'unList1', witness the isomorphism @[x] ~ Maybe (List1 x)@.
list1 :: [x] -> Maybe (List1 x)
list1 xs = has01 xs Nothing Just

-- | Forget the nonemptiness information.
toList :: List1 x -> [x]
toList (x :| xs) = x : xs

-- | Together with 'list1', witness the isomorphism @[x] ~ Maybe (List1 x)@.
unList1 :: Maybe (List1 x) -> [x]
unList1 = maybe [] toList

-- | Apply a 'List1' function on a regular list.
onList :: (List1 x -> List1 x) -> [x] -> [x]
onList f = maybe [] (toList . f) . list1

-- | Apply a regular list function on a 'List1'. Avoid shortening the list.
asList :: (HasCallStack) => ([x] -> [x]) -> List1 x -> List1 x
asList f xs = has01 (f (toList xs)) (error "Data.List1.asList: list got shortened") id

-- | Apply a 'List1' function if the list is not empty.
ifList1 :: (Alternative m) => [x] -> (List1 x -> y) -> m y
ifList1 xs = has01 xs empty . (pure .)

-- | Flipped version of 'has01', consistent with other libraries' @withNonEmpty@.
withList1 :: y -> (List1 x -> y) -> [x] -> y
withList1 y f xs = has01 xs y f

-- | Run an action taking a 'List1' if the list is not empty.
whenList1 :: (Applicative m) => [x] -> (List1 x -> m ()) -> m ()
whenList1 = (`has01` pure ())

-- |
-- Case split on a list with a default value and a 'List1' function.
-- Flipped variant of what some call @withNonEmpty@ or @withNotNull@.
has01 :: [x] -> y -> (List1 x -> y) -> y
has01 lx y xy = case lx of [] -> y; x : xs -> xy (x :| xs)

-- |
-- Case split on a 'List1' with a simple function and a 'List1' function.
has1Plus :: List1 x -> (x -> y) -> (x -> List1 x -> y) -> y
has1Plus lx y xy = case lx of Sole x -> y x; x :|| xs -> xy x xs

-- instance GHC.IsList (List1 x) where
--   type Item (List1 x) = x

--   fromList :: [x] -> List1 x
--   fromList = fromMaybe (error "Data.List.List1.fromList []") . list1

--   toList :: List1 x -> [x]
--   toList = toList

-- instance Semigroup (List1 x) where
--   (<>) :: List1 x -> List1 x -> List1 x
--   (x :| xs) <> ys = x :| (xs <> Fold.toList ys)

-- | Type-restricted concatenation.
(++) :: List1 x -> List1 x -> List1 x
(++) = (<>)

-- | 'List1' the elements backwards.
reverse :: List1 x -> List1 x
reverse = fix \rec (x :| xs) -> has01 xs (Sole x) ((||: x) . rec)

-- instance Foldable1 List1 where
--   foldMap1 :: (Semigroup s) => (x -> s) -> List1 x -> s
--   foldMap1 f = \case
--     Sole x -> f x
--     x :|| y -> f x <> foldMap1 f y

-- instance Applicative List1 where
--   pure :: x -> List1 x
--   pure = Sole

--   (<*>) :: List1 (x -> y) -> List1 x -> List1 y
--   (<*>) = ap

-- instance Monad List1 where
--   (>>=) :: List1 x -> (x -> List1 y) -> List1 y
--   (>>=) = flip foldMap1

-- instance MonadZip List1 where
--   mzip :: List1 x -> List1 y -> List1 (x, y)
--   mzip = zip
--   mzipWith :: (x -> y -> z) -> List1 x -> List1 y -> List1 z
--   mzipWith = zipWith
--   munzip :: List1 (x, y) -> (List1 x, List1 y)
--   munzip = unzip

-- instance MonadFix List1 where
--   mfix :: (x -> List1 x) -> List1 x
--   mfix f = case fix (f . head) of (x :| _) -> x :| mfix (tail . f)

-- | Extract the first element of a 'List1'.
head :: List1 x -> x
head (x :| _) = x

-- | Extract all but the first element of a 'List1'.
tail :: List1 x -> [x]
tail (_ :| xs) = xs

-- | Extract all but the last element of a 'List1'.
init :: List1 x -> [x]
init = fix \rec xs -> has1Plus xs (const []) \y ys -> y : rec ys

-- | Extract the last element of a 'List1'.
last :: List1 x -> x
last = fix \rec xs -> has1Plus xs id (const rec)

-- | Convenience function for decomposing 'List1' into its 'head' and 'tail'.
uncons :: List1 x -> (x, [x])
uncons (x :| xs) = (x, xs)

-- | Convenience function for decomposing 'List1' into its 'init' and 'last'.
unsnoc :: List1 x -> ([x], x)
unsnoc = fix \rec (x :| xs) -> has01 xs ([], x) (first (x :) . rec)

data Snoc1 x = Snoc1 {-# UNPACK #-} !Word (List1 x) [x]

-- | The sequence of prefixes of a 'List1', from shortest to longest.
inits :: List1 x -> List1 (List1 x)
inits (x :| xs) =
  scanl' snoc (snoc1 1 (Sole x) []) xs
    <&> \(Snoc1 _ front rear) -> front <| List.reverse rear
 where
  snoc1 :: Word -> List1 x -> [x] -> Snoc1 x
  snoc1 len front rear
    | len < 255 || (len .&. succ len) /= 0 = Snoc1 len front rear
    | otherwise = Snoc1 len (front <| List.reverse rear) []

  snoc :: Snoc1 x -> x -> Snoc1 x
  snoc (Snoc1 len front rear) y = snoc1 (succ len) front (y : rear)

-- | The 'List1' analogue of 'build'.
build1 :: forall x. (forall y. (x -> Maybe y -> y) -> Maybe y -> y) -> List1 x
build1 f = f (:?) Nothing

-- | The sequence of suffixes of a 'List1', from longest to shortest.
tails :: List1 x -> List1 (List1 x)
tails xs = build1 \(.?) end ->
  fix (\rec x@(_ :? y) -> x .? maybe end (Just . rec) y) xs

-- | Pointwise product of two 'List1's.
zip :: List1 x -> List1 y -> List1 (x, y)
zip = zipWith (,)

-- | Pointwise application of two 'List1's.
zipWith :: (x -> y -> z) -> List1 x -> List1 y -> List1 z
zipWith f = (fst .) . zipWithTruncate1 f

-- | Decompose a 'List1' of pairs into a pair of 'List1's.
unzip :: List1 (x, y) -> (List1 x, List1 y)
unzip = fix \rec -> \case
  Sole (x, y) -> (Sole x, Sole y)
  (x, y) :|| xys -> case rec xys of (xs, ys) -> (x :|| xs, y :|| ys)

-- | Traverse a 'List1' with an accumulating parameter from left to right.
accuml :: (a -> x -> (a, y)) -> a -> List1 x -> (a, List1 y)
accuml (+) = fix \rec a0 -> \case
  Sole x -> Sole <$> (a0 + x)
  x :|| xs -> case a0 + x of (a, y) -> (y :||) <$> rec a xs

-- | Traverse a 'List1' with an accumulating parameter from right to left.
accumr :: (a -> x -> (a, y)) -> a -> List1 x -> (a, List1 y)
accumr (+) a0 = fix \rec -> \case
  Sole x -> Sole <$> (a0 + x)
  x :|| xs -> case rec xs of (a, ys) -> (a + x) <&> (:|| ys)

-- | 'scanl' is similar to 'foldl', but returns a 'List1' of successive reduced values from the left.
scanl :: (y -> x -> y) -> y -> [x] -> List1 y
scanl (+) = fix \rec y zs ->
  y :? ifList1 zs \(x :| xs) -> rec (y + x) xs

-- | Strict version of 'scanl'.
scanl' :: (y -> x -> y) -> y -> [x] -> List1 y
scanl' (+) = fix \rec !y zs ->
  y :? ifList1 zs \(x :| xs) -> rec (y + x) xs

-- | A variant of 'scanl' that has no starting value argument and works on a 'List1'.
scanl1 :: (x -> x -> x) -> List1 x -> List1 x
scanl1 f (x :| xs) = scanl f x xs

-- | Strict version of 'scanl1'.
scanl1' :: (x -> x -> x) -> List1 x -> List1 x
scanl1' f (x :| xs) = scanl' f x xs

-- | 'scanr' is the right-to-left dual of 'scanl'. Note that the parameters of the accumulating function are also reversed.
scanr :: (x -> y -> y) -> y -> [x] -> List1 y
scanr (+) = fix \rec y zs ->
  y :? ifList1 zs \(x :| xs) -> rec (x + y) xs

-- | A variant of 'scanr' with no starting value argument and works on a 'List1'.
scanr1 :: (x -> x -> x) -> List1 x -> List1 x
scanr1 f (x :| xs) = scanr f x xs

-- | Build a 'List1' from a generating function and seed value.
unfoldr :: (x -> (y, Maybe x)) -> x -> List1 y
unfoldr f x = case f x of (y, mx) -> y :? fmap (unfoldr f) mx

-- | Apply a function to every element of a 'List1'.
map :: (x -> y) -> List1 x -> List1 y
map f = \case
  Sole x -> Sole (f x)
  x :|| xs -> f x :|| map f xs

-- | A version of 'map' that can eliminate (possibly all) values from a 'List1'.
mapMaybe :: (x -> Maybe y) -> List1 x -> Maybe (List1 y)
mapMaybe f = fix \rec (x :? xs) -> maybe id (\fx -> Just . (fx :?)) (f x) (rec =<< xs)

-- | Returns a list of all (possibly no) 'Just' values in a 'List1'.
catMaybes :: List1 (Maybe x) -> Maybe (List1 x)
catMaybes = mapMaybe id

-- | Take the first (possibly no) elements of a 'List1'.
take :: Int -> List1 x -> Maybe (List1 x)
take = fix \rec n (x :? xs) -> guard (n > 0) $> (x :? (rec (pred n) =<< xs))

-- | Get rid of the first (possibly all) elements of a 'List1'.
drop :: Int -> List1 x -> Maybe (List1 x)
drop = fix \rec n (x :? xs) -> if n <= 0 then Just (x :? xs) else rec (pred n) =<< xs

-- | Keep the longest prefix of elements of a 'List1' that satisfy a predicate.
takeWhile :: (x -> Bool) -> List1 x -> Maybe (List1 x)
takeWhile p = fix \rec (x :? xs) -> guard (p x) $> x :? (rec =<< xs)

-- | Drop the longest prefix of elements of a 'List1' that satisfy a predicate.
dropWhile :: (x -> Bool) -> List1 x -> Maybe (List1 x)
dropWhile p = fix \rec (x :? xs) -> if p x then rec =<< xs else Just (x :? xs)

-- | Remove the first occurrence of the given element from a 'List1'.
delete :: (Eq x) => x -> List1 x -> Maybe (List1 x)
delete = deleteBy (==)

-- | Remove an element from a 'List1' according to a supplied equality test.
deleteBy :: (x -> x -> Bool) -> x -> List1 x -> Maybe (List1 x)
deleteBy eq y = fix \rec (x :? xs) -> if eq x y then xs else Just (x :? (rec =<< xs))

-- | Remove all of the elements of the second argument from the first argument.
(\\) :: (Eq x) => List1 x -> List1 x -> Maybe (List1 x)
xs \\ os = filter (not . (`elem` os)) xs

-- | Keep only (possibly no) elements satisfying a predicate.
filter :: (x -> Bool) -> List1 x -> Maybe (List1 x)
filter p = fix \rec (x :? xs) -> (if p x then Just . (x :?) else id) (rec =<< xs)

-- | The prefix and suffix of a 'List1' where the elements of the prefix satisfy the predicate.
span :: (x -> Bool) -> List1 x -> ([x], [x])
span p = List.span p . toList

-- | The prefix and suffix of a 'List1' where the elements of the prefix /do not/ satisfy the predicate.
break :: (x -> Bool) -> List1 x -> ([x], [x])
break p = List.break p . toList

-- | The elements of a 'List1' that do and do not satisfy the predicate, in order.
partition :: (x -> Bool) -> List1 x -> ([x], [x])
partition p = List.partition p . toList

-- | Split a 'List1' at the given index.
splitAt :: Int -> List1 x -> ([x], [x])
splitAt n xs = (unList1 (take n xs), unList1 (drop n xs))

-- | Attach the index to each element of a 'List1'.
index :: (Integral n) => List1 x -> List1 (n, x)
index = zip (iterated succ 0)

-- | Whether the given element is not in the 'List1'.
notElem :: (Eq x) => x -> List1 x -> Bool
notElem = (not .) . elem

-- | Whether the given element is found in the 'List1'.
elem :: (Eq x) => x -> List1 x -> Bool
elem = (isJust .) . elemIndex

-- | The first index of the element, if it is found, within the 'List1'.
elemIndex :: (Eq x) => x -> List1 x -> Maybe Int
elemIndex = findIndex . (==)

-- | All the indices of the element, if it is found, within the 'List1'.
elemIndices :: (Eq x) => x -> List1 x -> Maybe (List1 Int)
elemIndices = findIndices . (==)

-- | The first element, if any, to satisfy a predicate.
find :: (x -> Bool) -> List1 x -> Maybe x
find p = fmap head . filter p

-- | The index of the first element, if any, to satisfy a predicate.
findIndex :: (x -> Bool) -> List1 x -> Maybe Int
findIndex p = fmap head . findIndices p

-- | All of the positions of the elements satisfying a predicate.
findIndices :: (x -> Bool) -> List1 x -> Maybe (List1 Int)
findIndices p xs = flip mapMaybe (index xs) \(i, x) -> guard (p x) $> i

-- | The element at a given index.
(!?) :: List1 x -> Int -> Maybe x
(x :? xs) !? n
  | n < 0 = Nothing
  | n == 0 = Just x
  | otherwise = xs >>= (!? pred n)

-- | Given a 'List1' of pairs, find the second coordinate of the first element matching in the first coordinate.
lookup :: (Eq x) => x -> List1 (x, y) -> Maybe y
lookup x = fmap snd . find ((x ==) . fst)

-- | Sort a 'List1'.
sort :: (Ord x) => List1 x -> List1 x
sort = asList List.sort

-- | Sort a 'List1' using the projection.
sortOn :: (Ord y) => (x -> y) -> List1 x -> List1 x
sortOn = asList . List.sortOn

-- | Sort a 'List1' using an explicit comparison.
sortBy :: (x -> x -> Ordering) -> List1 x -> List1 x
sortBy = asList . List.sortBy

-- | Group the elements of a 'List1' by equality.
group :: (Eq x) => List1 x -> List1 (List1 x)
group = groupBy (==)

-- | Group the elements of a 'List1' by equality on a projection.
groupOn :: (Eq y) => (x -> y) -> List1 x -> List1 (List1 x)
groupOn f = groupBy (on (==) f)

-- | Group the elements of a 'List1' with an explicit equality test.
groupBy :: (x -> x -> Bool) -> List1 x -> List1 (List1 x)
groupBy eq = fix \rec (x :| lx) -> case List.span (eq x) lx of
  (xs, ys) -> (x :| xs) :? ifList1 ys rec

-- | Find the (possibly no) elements that are in both 'List1's.
intersect :: (Eq x) => List1 x -> List1 x -> Maybe (List1 x)
intersect = intersectBy (==)

-- | Find the (possibly no) elements that are found in both 'List1's using a projection.
intersectOn :: (Eq y) => (x -> y) -> List1 x -> List1 x -> Maybe (List1 x)
intersectOn f = intersectBy (on (==) f)

-- | Find the (possibly no) elements in the first 'List1' that match any element of the second 'List1' using an explicit equality test.
intersectBy :: (x -> y -> Bool) -> List1 x -> List1 y -> Maybe (List1 x)
intersectBy eq xs ys = flip mapMaybe xs \x -> guard (Fold.any (eq x) ys) $> x

-- | Combine two 'List1's, keeping only those elements from the second 'List1' that are not already in the first.
union :: (Eq x) => List1 x -> List1 x -> List1 x
union = unionBy (==)

-- | Similar to 'union' but using equality on a projection.
unionOn :: (Eq y) => (x -> y) -> List1 x -> List1 x -> List1 x
unionOn f = unionBy (on (==) f)

-- | Similar to 'union' but with an explicit equality test.
unionBy :: (x -> x -> Bool) -> List1 x -> List1 x -> List1 x
unionBy eq xs ys =
  xs <> Fold.foldr ((fromJust .) . deleteBy eq) (nubBy eq ys) (toList xs)

-- | Keep only one copy of each element.
nub :: (Eq x) => List1 x -> List1 x
nub = nubBy (==)

-- | Keep only one copy of each element whose projections match.
nubOn :: (Eq y) => (x -> y) -> List1 x -> List1 x
nubOn f = nubBy (on (==) f)

-- | Keep only one copy of each element whose projections match the explicit equality test.
nubBy :: (x -> x -> Bool) -> List1 x -> List1 x
nubBy eq (x :| xs) = x :| List.nubBy eq (List.filter (not . eq x) xs)

-- | Find the maximum of a 'List1'.
maximum :: (Ord x) => List1 x -> x
maximum = Fold.maximum

-- | Find the maximum of a projection function.
maximumOf :: (Ord y) => (x -> y) -> List1 x -> y
maximumOf f = maximum . fmap f

-- | Find the element with maximal projection.
maximumOn :: (Ord y) => (x -> y) -> List1 x -> x
maximumOn f = maximumBy (comparing f)

-- | Find the maximum using an explicit comparison function.
maximumBy :: (x -> x -> Ordering) -> List1 x -> x
maximumBy = Fold.maximumBy

-- | Find the minimum of a 'List1'.
minimum :: (Ord x) => List1 x -> x
minimum = Fold.minimum

-- | Find the minimum of a projection function.
minimumOf :: (Ord y) => (x -> y) -> List1 x -> y
minimumOf f = minimum . fmap f

-- | Find the element with minimal projection.
minimumOn :: (Ord y) => (x -> y) -> List1 x -> x
minimumOn f = minimumBy (comparing f)

-- | Find the minimum using an explicit comparison function.
minimumBy :: (x -> x -> Ordering) -> List1 x -> x
minimumBy = Fold.minimumBy

-- | Apply a function repeatedly to a starting value. The first element is the starting value.
iterate :: (x -> x) -> x -> List1 x
iterate f = fix \rec x -> x :|| rec (f x)

-- | Apply a function strictly to a starting value. The first element is the starting value.
iterated :: (x -> x) -> x -> List1 x
iterated f = fix \rec !x -> x :|| rec (f x)

-- | The infinite 'List1' consisting of a single value.
repeat :: x -> List1 x
repeat = fix (ap (:||))

-- | The 'List1' of given length consisting only of the given value.
replicate :: Int -> x -> List1 x
replicate n x = case n of
  _ | n <= 0 -> error "Data.List1.replicate: argument must be positive"
  1 -> Sole x
  _ -> x :|| replicate (pred n) x

-- | The infinite 'List1' created by repeating the elements of the given 'List1'.
cycle :: List1 x -> List1 x
cycle = fix (ap (<>))

-- | Place an element between all other elements in a 'List1'.
--
-- > intersperse 'y' ('a' :|| 'b' :|| Sole 'c') == ('a' :|| 'y' :|| 'b' :|| 'y' :|| Sole 'c')
intersperse :: x -> List1 x -> List1 x
intersperse y = fix \rec (x :? xs) -> x :? fmap ((y :||) . rec) xs

-- | Squash a 'List1' of 'List1's together with the given argument in between each 'List1'.
--
-- > intercalate (1 :|| Sole 1) (Sole 2 :|| Sole 3 :|| Sole (Sole 4)) == (2 :|| 1 :|| 1 :|| 3 :|| 1 :|| 1 :|| Sole 4)
intercalate :: List1 x -> List1 (List1 x) -> List1 x
intercalate = (join .) . intersperse

transpose :: List1 (List1 x) -> List1 (List1 x)
transpose = fix \rec ((x :| xs) :| xss) -> case List.unzip (fmap uncons xss) of
  (hs, ts) -> case mapMaybe list1 (xs :| ts) of
    Nothing -> Sole (x :| hs)
    Just ys -> (x :| hs) :|| rec ys

-- | All of the non-empty sublists of a 'List1', including those that skip elements.
subsequences :: List1 x -> List1 (List1 x)
subsequences = fix \rec (x :? xs) ->
  Sole x :? fmap (ap (:||) (Sole . (x :||)) <=< rec) xs

-- | @windows n@ lists the consecutive 'subsequences' of length @n@ of a 'List1': the subsequences of length @n@ that do not skip any elements.
windows :: Int -> List1 x -> Maybe (List1 (List1 x))
windows n xs = take (Fold.length xs Num.- n Num.+ 1) =<< mapMaybe (take n) (tails xs)

-- | All of the consecutive subsequences of a 'List1': the 'subsequences' that do not skip any elements.
consecutiveSubsequences :: List1 x -> List1 (List1 x)
consecutiveSubsequences xs = fromMaybe (Sole xs) $ Fold.foldMap (`windows` xs) [1 .. Fold.length xs]

-- | The 'List1' of all rearrangements of a 'List1'.
permutations :: List1 x -> List1 (List1 x)
permutations = fix \rec xs ->
  (xs :?) . fmap join $ flip diagonally xs \hs (t :| ts) ->
    fmap (<| ts) . insertions t =<< rec hs

-- | Apply a function on the prefix and suffix of a 'List1' at every index.
diagonally :: (List1 x -> List1 x -> y) -> List1 x -> Maybe (List1 y)
diagonally f xs =
  catMaybes $
    zipWith
      (liftM2 f)
      (Nothing :|| (Just <$> inits xs))
      ((Just <$> tails xs) ||: Nothing)

-- | The 'init' and 'tail' of the 'List1' at each positive index.
--
-- >>> diagonals (1 :| [2, 3, 4])
-- [(1 :| [],2 :| [3,4]),(1 :| [2],3 :| [4]),(1 :| [2,3],4 :| [])]
diagonals :: List1 x -> [(List1 x, List1 x)]
diagonals = unList1 . diagonally (,)

-- | Insert an element before each member of a 'List1'.
--
-- > insertions x (a :|| b :|| c :|| ...)
-- >     == (x :|| a :|| b :|| c :|| ...)
-- >    :|| (a :|| x :|| b :|| c :|| ...)
-- >    :|| (a :|| b :|| x :|| c :|| ...) ...
insertions :: x -> List1 x -> List1 (List1 x)
insertions x = fix \rec ly@(y :? ys) ->
  (x :|| ly) :? (fmap (y :||) . rec <$> ys)

data Wedge x y = Nowhere | Here x | There y deriving (Functor)

instance Bifunctor Wedge where
  bimap :: (x -> x') -> (y -> y') -> Wedge x y -> Wedge x' y'
  bimap f g = \case
    Nowhere -> Nowhere
    Here x -> Here (f x)
    There y -> There (g y)

-- -- | Zip two lists with the provided function without deleting the tail of the longer list.
-- --
-- -- >>> zipWithTruncate (,) [1, 2, 3] [10, 20, 30, 40, 50]
-- -- ([(1,10),(2,20),(3,30)],There [40,50])
-- zipWithTruncate :: (a -> b -> c) -> [a] -> [b] -> ([c], Wedge [a] [b])
-- zipWithTruncate f as bs =
--   bimap
--     (maybe [] toList)
--     (bimap toList toList)
--     (zipWithTruncate' f (list1 as) (list1 bs))

-- | The workhorse of 'zipWithTruncate' and 'zipWithTruncate1'.
zipWithTruncate' ::
  (a -> b -> c) ->
  Maybe (List1 a) ->
  Maybe (List1 b) ->
  (Maybe (List1 c), Wedge (List1 a) (List1 b))
zipWithTruncate' f = fix \rec -> \cases
  Nothing Nothing -> (Nothing, Nowhere)
  Nothing (Just tb) -> (Nothing, There tb)
  (Just ta) Nothing -> (Nothing, Here ta)
  (Just (a :| as)) (Just (b :| bs)) ->
    let (__, w) = rec (list1 as) (list1 bs)
     in (Just (f a b :? __), w)

-- | Zip two 'List1's with the provided function without deleting the tail of the longer 'List1'.
zipWithTruncate1 ::
  (a -> b -> c) ->
  List1 a ->
  List1 b ->
  (List1 c, Wedge (List1 a) (List1 b))
zipWithTruncate1 f (a :| as) (b :| bs) =
  let (__, w) = zipWithTruncate' f (list1 as) (list1 bs)
   in (f a b :? __, w)
