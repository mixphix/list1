module Data.List1 (
  -- List1 (..),
  List1,
  NonEmpty ((:|)),
  pattern Sole,
  pattern (:&),
  pattern (:&?),
  (<&),
  (&>),
  (|:),
  (&:),
  list1,
  toList,
  unList1,
  onList,
  onList1,
  withList1,
  uncons,
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
  permutations,
  diagonally,
  diagonals,
  insertions,
  compareLength,
) where

import Control.Applicative ((<|>))
import Control.Monad (ap, guard, join, liftM2, (<=<), (=<<), (>>), (>>=))
import Control.Monad.Fix (fix)
-- import Control.Monad.Fix (MonadFix (..), fix)
-- import Control.Monad.Zip (MonadZip (..))
import Data.Bool (Bool (..), not, otherwise)
-- import Data.Data (Data, Typeable)
import Data.Eq (Eq (..))
-- import Data.Foldable (Foldable)
-- import Data.Foldable1 (Foldable1 (..))
import Data.Foldable qualified as Fold
import Data.Function (flip, id, on, ($), (.))
import Data.Functor (fmap, void, ($>), (<$>), (<&>))
import Data.Int (Int)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), unfoldr)
import Data.Maybe (Maybe (..), fromJust, isJust, maybe)
import Data.Maybe qualified as Maybe
import Data.Ord (Ord (..), Ordering (..), comparing)
import Data.Semigroup (Semigroup ((<>)))
import Data.Traversable (for, traverse)
-- import GHC.Generics (Generic, Generic1)
-- import GHC.IsList qualified (IsList (..))
-- import GHC.IsList qualified as GHC (IsList)
import Prelude (Enum (..), Integral)

infixr 5 {- :|, -} :&, :&?, |:, &:

infixl 4 <&, &>

type List1 = NonEmpty

-- data List1 x = x :| [x]
--   deriving
--     ( Eq
--     , Ord
--     , Show
--     , Read
--     , Typeable
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
pattern (:&) :: x -> List1 x -> List1 x
pattern x :& y <- (x :| (list1 -> Just y))
  where
    x :& ~(y :| ys) = x :| (y : ys)

{-# COMPLETE Sole, (:&) #-}

-- | Isomorphic to '(:|)', but instead with a 'Maybe' 'List1'.
pattern (:&?) :: x -> Maybe (List1 x) -> List1 x
pattern x :&? y <- (x :| ~(list1 -> y))
  where
    x :&? y = maybe (Sole x) (x :&) y

{-# COMPLETE (:&?) #-}

-- | Prepend a 'List1' to a list.
(<&) :: List1 x -> [x] -> List1 x
x :| xs <& ys = x :| (xs <> ys)

-- | Append a 'List1' to a list.
(&>) :: [x] -> List1 x -> List1 x
xs &> ys = case xs of
  [] -> ys
  x : zs -> x :& (zs &> ys)

-- | Append an element to a list. C.f. '(:|)'.
(|:) :: [x] -> x -> List1 x
ys |: x = ys &> Sole x

-- | Append an element to a 'List1'. C.f. '(:&)'.
(&:) :: List1 x -> x -> List1 x
(y :| ys) &: x = y :| (ys <> [x])

-- | Together with 'unList1', witness the isomorphism @[x] ~ Maybe (List1 x)@.
list1 :: [x] -> Maybe (List1 x)
list1 = \case
  [] -> Nothing
  x : xs -> Just (x :| xs)

-- | Forget the nonemptiness information.
toList :: List1 x -> [x]
toList (x :| xs) = x : xs

-- | Together with 'list1', witness the isomorphism @[x] ~ Maybe (List1 x)@.
unList1 :: Maybe (List1 x) -> [x]
unList1 = maybe [] toList

-- | Apply a 'List1' endomorphism to a regular list.
onList :: (List1 x -> List1 x) -> [x] -> [x]
onList f = maybe [] (toList . f) . list1

-- | Check nonemptiness and apply a 'List1' function in the same step.
onList1 :: (List1 x -> y) -> [x] -> Maybe y
onList1 f = fmap f . list1

-- |
-- Case split on a list with a default value and a 'List1' function.
-- Flipped variant of what some call @withNonEmpty@ or @withNotNull@.
withList1 :: [x] -> y -> (List1 x -> y) -> y
withList1 lx y xy = case lx of [] -> y; x : xs -> xy (x :| xs)

-- instance GHC.IsList (List1 x) where
--   type Item (List1 x) = x

--   fromList :: [GHC.IsList.Item (List1 x)] -> List1 x
--   fromList = fromMaybe (error "Data.List.List1.fromList []") . list1

--   toList :: List1 x -> [GHC.IsList.Item (List1 x)]
--   toList = toList

-- instance Semigroup (List1 x) where
--   (<>) :: List1 x -> List1 x -> List1 x
--   (x :| xs) <> ys = x :| (xs <> Fold.toList ys)

-- | Type-restricted concatenation.
(++) :: List1 x -> List1 x -> List1 x
(++) = (<>)

-- | 'List1' the elements backwards.
reverse :: List1 x -> List1 x
reverse (x :| xs) = withList1 xs (Sole x) ((&: x) . reverse)

-- instance Foldable1 List1 where
foldMap1 :: (Semigroup s) => (x -> s) -> List1 x -> s
foldMap1 f = \case
  Sole x -> f x
  x :& y -> f x <> foldMap1 f y

-- instance Applicative List1 where
--   pure :: x -> List1 x
--   pure = List1

--   (<*>) :: List1 (x -> y) -> List1 x -> List1 y
--   (<*>) = ap

-- instance Monad List1 where
--   (>>=) :: List1 x -> (x -> List1 y) -> List1 y
--   (>>=) = flip foldMap1

-- | Extract the first element of a 'List1'.
head :: List1 x -> x
head (x :| _) = x

-- | Extract all but the first element of a 'List1'.
tail :: List1 x -> [x]
tail (_ :| xs) = xs

-- | Extract all but the last element of a 'List1'.
init :: List1 x -> [x]
init = \case
  Sole _ -> []
  x :& xs -> x : init xs

-- | Extract the last element of a 'List1'.
last :: List1 x -> x
last = \case
  Sole x -> x
  _ :& xs -> last xs

-- | Convenience function for decomposing 'List1' into its 'head' and 'tail'.
uncons :: List1 x -> (x, [x])
uncons (x :| xs) = (x, xs)

-- | The analogue of 'build' for regular lists.
build1 :: forall x. (forall y. (x -> Maybe y -> y) -> Maybe y -> y) -> List1 x
build1 f = f (:&?) Nothing

-- | The sequence of prefixes of a 'List1', from longest to shortest.
inits :: List1 x -> List1 (List1 x)
inits = fromJust . list1 . Maybe.mapMaybe list1 . List.tail . List.inits . toList

-- | The sequence of suffixes of a 'List1', from longest to shortest.
tails :: List1 x -> List1 (List1 x)
tails xs = build1 \(.&?) end ->
  fix (\go x@(_ :&? y) -> x .&? maybe end (Just . go) y) xs

-- | Pointwise product of two 'List1's.
zip :: List1 x -> List1 y -> List1 (x, y)
zip = zipWith (,)

-- | Pointwise application of two 'List1's.
zipWith :: (x -> y -> z) -> List1 x -> List1 y -> List1 z
zipWith (+) (x :| xs) (y :| ys) = x + y :| List.zipWith (+) xs ys

-- | Decompose a 'List1' of pairs into a pair of 'List1's.
unzip :: List1 (x, y) -> (List1 x, List1 y)
unzip = \case
  Sole (x, y) -> (Sole x, Sole y)
  (x, y) :& xys -> case unzip xys of (xs, ys) -> (x :& xs, y :& ys)

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

accuml :: (a -> x -> (a, y)) -> a -> List1 x -> (a, List1 y)
accuml (+) a0 (x :&? xs) = case a0 + x of
  (a, y) -> maybe (a, Sole y) (fmap (y :&) . accuml (+) a) xs

accumr :: (a -> x -> (a, y)) -> a -> List1 x -> (a, List1 y)
accumr (+) a0 = \case
  Sole x -> Sole <$> (a0 + x)
  x :& xs -> case accumr (+) a0 xs of (a, ys) -> (a + x) <&> (:& ys)

scanl :: (y -> x -> y) -> y -> [x] -> List1 y
scanl = fix \go f y -> \case
  [] -> Sole y
  x : xs -> go f (f y x) xs

scanl' :: (y -> x -> y) -> y -> [x] -> List1 y
scanl' = fix \go f !y -> \case
  [] -> Sole y
  x : xs -> go f (f y x) xs

scanl1 :: (x -> x -> x) -> List1 x -> List1 x
scanl1 f (x :| xs) = scanl f x xs

scanl1' :: (x -> x -> x) -> List1 x -> List1 x
scanl1' f (x :| xs) = scanl' f x xs

scanr :: (x -> y -> y) -> y -> [x] -> List1 y
scanr = fix \go f y -> \case
  [] -> Sole y
  x : xs -> go f (f x y) xs

scanr1 :: (x -> x -> x) -> List1 x -> List1 x
scanr1 f (x :| xs) = scanr f x xs

mapMaybe :: (x -> Maybe y) -> List1 x -> Maybe (List1 y)
mapMaybe = fix \go f (x :&? xs) ->
  maybe id ((Just .) . (:&?)) (f x) (go f =<< xs)

catMaybes :: List1 (Maybe x) -> Maybe (List1 x)
catMaybes = mapMaybe id

take :: Int -> List1 x -> Maybe (List1 x)
take n (x :| xs) = guard (n > 0) $> (x :| List.take (pred n) xs)

drop :: Int -> List1 x -> Maybe (List1 x)
drop n (x :&? xs) = if n <= 0 then Just (x :&? xs) else drop (pred n) =<< xs

takeWhile :: (x -> Bool) -> List1 x -> Maybe (List1 x)
takeWhile p (x :&? xs) = guard (p x) >> (fmap (x :&) . takeWhile p =<< xs)

dropWhile :: (x -> Bool) -> List1 x -> Maybe (List1 x)
dropWhile p (x :&? xs) = if p x then dropWhile p =<< xs else Just (x :&? xs)

delete :: (Eq x) => x -> List1 x -> Maybe (List1 x)
delete = deleteBy (==)

deleteBy :: (x -> x -> Bool) -> x -> List1 x -> Maybe (List1 x)
deleteBy eq y (x :&? xs) = (guard (eq x y) >> xs) <|> (deleteBy eq y =<< xs)

(\\) :: (Eq x) => List1 x -> List1 x -> Maybe (List1 x)
xs \\ os = filter (not . (`elem` os)) xs

filter :: (x -> Bool) -> List1 x -> Maybe (List1 x)
filter p (x :&? xs) = (if p x then Just . (x :&?) else id) (filter p =<< xs)

span :: (x -> Bool) -> List1 x -> ([x], [x])
span p = List.span p . toList

break :: (x -> Bool) -> List1 x -> ([x], [x])
break p = List.break p . toList

partition :: (x -> Bool) -> List1 x -> ([x], [x])
partition p = List.partition p . toList

splitAt :: Int -> List1 x -> ([x], [x])
splitAt n xs = (unList1 (take n xs), unList1 (drop n xs))

index :: (Integral n) => List1 x -> List1 (n, x)
index = zip (iterated succ 0)

notElem :: (Eq x) => x -> List1 x -> Bool
notElem = (not .) . elem

elem :: (Eq x) => x -> List1 x -> Bool
elem = (isJust .) . elemIndex

elemIndex :: (Eq x) => x -> List1 x -> Maybe Int
elemIndex = findIndex . (==)

elemIndices :: (Eq x) => x -> List1 x -> Maybe (List1 Int)
elemIndices = findIndices . (==)

find :: (x -> Bool) -> List1 x -> Maybe x
find p = fmap head . filter p

findIndex :: (x -> Bool) -> List1 x -> Maybe Int
findIndex p = fmap head . findIndices p

findIndices :: (x -> Bool) -> List1 x -> Maybe (List1 Int)
findIndices p xs = catMaybes $ index xs <&> \(i, x) -> guard (p x) $> i

(!?) :: List1 x -> Int -> Maybe x
(x :&? xs) !? n
  | n < 0 = Nothing
  | n == 0 = Just x
  | otherwise = xs >>= (!? pred n)

lookup :: Int -> List1 x -> Maybe x
lookup = flip (!?)

sort :: (Ord x) => List1 x -> List1 x
sort = fromJust . list1 . List.sort . toList

sortOn :: (Ord y) => (x -> y) -> List1 x -> List1 x
sortOn f = fromJust . list1 . List.sortOn f . toList

sortBy :: (x -> x -> Ordering) -> List1 x -> List1 x
sortBy f = fromJust . list1 . List.sortBy f . toList

group :: (Eq x) => List1 x -> List1 (List1 x)
group = groupBy (==)

groupOn :: (Eq y) => (x -> y) -> List1 x -> List1 (List1 x)
groupOn f = groupBy (on (==) f)

groupBy :: (x -> x -> Bool) -> List1 x -> List1 (List1 x)
groupBy eq (x :| lx) = case List.span (eq x) lx of
  (xs, ys) -> (x :| xs) :&? onList1 (groupBy eq) ys

intersect :: (Eq x) => List1 x -> List1 x -> Maybe (List1 x)
intersect = intersectBy (==)

intersectOn :: (Eq y) => (x -> y) -> List1 x -> List1 x -> Maybe (List1 x)
intersectOn f = intersectBy (on (==) f)

intersectBy :: (x -> y -> Bool) -> List1 x -> List1 y -> Maybe (List1 x)
intersectBy eq xs ys = for xs \x -> guard (Fold.any (eq x) ys) $> x

union :: (Eq x) => List1 x -> List1 x -> List1 x
union = unionBy (==)

unionOn :: (Eq y) => (x -> y) -> List1 x -> List1 x -> List1 x
unionOn f = unionBy (on (==) f)

unionBy :: (x -> x -> Bool) -> List1 x -> List1 x -> List1 x
unionBy eq xs ys =
  xs <> Fold.foldr ((fromJust .) . deleteBy eq) (nubBy eq ys) (toList xs)

nub :: (Eq x) => List1 x -> List1 x
nub = nubBy (==)

nubOn :: (Eq y) => (x -> y) -> List1 x -> List1 x
nubOn f = nubBy (on (==) f)

nubBy :: (x -> x -> Bool) -> List1 x -> List1 x
nubBy eq (x :| xs) = x :| List.nubBy eq (List.filter (not . eq x) xs)

maximum :: (Ord x) => List1 x -> x
maximum = Fold.maximum

maximumOf :: (Ord y) => (x -> y) -> List1 x -> y
maximumOf f = maximum . fmap f

maximumOn :: (Ord x, Ord y) => (x -> y) -> List1 x -> x
maximumOn f = maximumBy (comparing f)

maximumBy :: (x -> x -> Ordering) -> List1 x -> x
maximumBy = Fold.maximumBy

minimum :: (Ord x) => List1 x -> x
minimum = Fold.minimum

minimumOf :: (Ord y) => (x -> y) -> List1 x -> y
minimumOf f = minimum . fmap f

minimumOn :: (Ord x, Ord y) => (x -> y) -> List1 x -> x
minimumOn f = minimumBy (comparing f)

minimumBy :: (x -> x -> Ordering) -> List1 x -> x
minimumBy = Fold.minimumBy

iterate :: (x -> x) -> x -> List1 x
iterate f x = x :& iterate f (f x)

iterated :: (x -> x) -> x -> List1 x
iterated f !x = x :& iterated f (f x)

repeat :: x -> List1 x
repeat = fix (ap (:&))

replicate :: Int -> x -> Maybe (List1 x)
replicate n x = take n (repeat x)

cycle :: List1 x -> List1 x
cycle = fix (ap (<>))

intersperse :: x -> List1 x -> List1 x
intersperse y (x :&? xs) = x :&? fmap ((y :&) . intersperse y) xs

intercalate :: List1 x -> List1 (List1 x) -> List1 x
intercalate = (join .) . intersperse

transpose :: List1 (List1 x) -> List1 (List1 x)
transpose = \case
  Sole x -> traverse Sole x
  (x :&? xs) :& xss -> case unzip (xss <&> \(h :&? t) -> (h, t)) of
    (hs, ts) ->
      (x :& hs) :&? maybe id (fmap . (transpose .) . (:&)) xs (catMaybes ts)

subsequences :: List1 x -> List1 (List1 x)
subsequences (x :&? xs) =
  Sole x :&? fmap (ap (:&) (Sole . (x :&)) <=< subsequences) xs

permutations :: List1 x -> List1 (List1 x)
permutations xs =
  (xs :&?) . fmap join $ flip diagonally xs \(t :| ts) hs ->
    fmap (<& ts) . insertions t =<< permutations hs

diagonally :: (List1 x -> List1 x -> y) -> List1 x -> Maybe (List1 y)
diagonally f xs =
  catMaybes $
    zipWith
      (liftM2 f)
      ((Just <$> tails xs) &: Nothing)
      (Nothing :& (Just <$> inits xs))

diagonals :: List1 x -> Maybe (List1 (List1 x, List1 x))
diagonals = diagonally (,)

-- > insertions x (a : b : cs)
-- >   == (x : a : b : cs)
-- >    : (a : x : b : cs)
-- >    : (a : b : x : cs) ...
insertions :: x -> List1 x -> List1 (List1 x)
insertions x ly@(y :&? ys) = (x :& ly) :&? (fmap (y :&) . insertions x <$> ys)

compareLength :: List1 x -> List1 y -> Ordering
compareLength xs ys = compare (void xs) (void ys)
