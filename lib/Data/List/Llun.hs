module Data.List.Llun (
  -- Llun (..),
  Llun,
  NonEmpty ((:|)),
  pattern Llun,
  pattern (:&),
  pattern (:&?),
  (<&),
  (&>),
  (|:),
  (&:),
  llun,
  toList,
  unLlun,
  onList,
  onLlun,
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

import Control.Applicative ((<*>), (<|>))
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
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (Maybe (..), fromJust, isJust, maybe)
import Data.Maybe qualified as Maybe
import Data.Ord (Ord (..), Ordering (..), comparing)
import Data.Semigroup (Semigroup ((<>)))
import Data.Traversable (for, sequence, traverse)
-- import GHC.Generics (Generic, Generic1)
-- import GHC.IsList qualified (IsList (..))
-- import GHC.IsList qualified as GHC (IsList)
import Prelude (Enum (..), Integral)

infixr 5 {- :|, -} :&, :&?, |:, &:

infixl 4 <&, &>

type Llun = NonEmpty

-- data Llun x = x :| [x]
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

pattern Llun :: x -> Llun x
pattern Llun x = x :| []

pattern (:&) :: x -> Llun x -> Llun x
pattern x :& y <- (x :| ~(llun -> Just y))
  where
    x :& ~(y :| ys) = x :| (y : ys)

{-# COMPLETE Llun, (:&) #-}

pattern (:&?) :: x -> Maybe (Llun x) -> Llun x
pattern x :&? y <- (x :| ~(llun -> y))
  where
    x :&? y = maybe (Llun x) (x :&) y

{-# COMPLETE (:&?) #-}

(<&) :: Llun x -> [x] -> Llun x
x :| xs <& ys = x :| (xs <> ys)

(&>) :: [x] -> Llun x -> Llun x
xs &> ys = case xs of
  [] -> ys
  x : zs -> x :& (zs &> ys)

(|:) :: [x] -> x -> Llun x
ys |: x = ys &> Llun x

(&:) :: Llun x -> x -> Llun x
(y :| ys) &: x = y :| (ys <> [x])

llun :: [x] -> Maybe (Llun x)
llun = \case
  [] -> Nothing
  x : xs -> Just (x :| xs)

toList :: Llun x -> [x]
toList (x :| xs) = x : xs

unLlun :: Maybe (Llun x) -> [x]
unLlun = maybe [] toList

onList :: (Llun x -> Llun x) -> [x] -> [x]
onList f = maybe [] (toList . f) . llun

onLlun :: (Llun x -> y) -> [x] -> Maybe y
onLlun f = fmap f . llun

-- instance GHC.IsList (Llun x) where
--   type Item (Llun x) = x

--   fromList :: [GHC.IsList.Item (Llun x)] -> Llun x
--   fromList = fromMaybe (error "Data.List.Llun.fromList []") . llun

--   toList :: Llun x -> [GHC.IsList.Item (Llun x)]
--   toList = toList

-- instance Semigroup (Llun x) where
--   (<>) :: Llun x -> Llun x -> Llun x
--   (x :| xs) <> ys = x :| (xs <> Fold.toList ys)

(++) :: Llun x -> Llun x -> Llun x
(++) = (<>)

reverse :: Llun x -> Llun x
reverse (x :| xs) = maybe (Llun x) ((&: x) . reverse) (llun xs)

-- instance Foldable1 Llun where
foldMap1 :: (Semigroup s) => (x -> s) -> Llun x -> s
foldMap1 f = \case
  Llun x -> f x
  x :& y -> f x <> foldMap1 f y

-- instance Applicative Llun where
--   pure :: x -> Llun x
--   pure = Llun

--   (<*>) :: Llun (x -> y) -> Llun x -> Llun y
--   (<*>) = ap

-- instance Monad Llun where
--   (>>=) :: Llun x -> (x -> Llun y) -> Llun y
--   (>>=) = flip foldMap1

head :: Llun x -> x
head (x :| _) = x

tail :: Llun x -> [x]
tail (_ :| xs) = xs

init :: Llun x -> [x]
init = \case
  Llun _ -> []
  x :& xs -> x : init xs

last :: Llun x -> x
last = \case
  Llun x -> x
  _ :& xs -> last xs

build1 :: forall x. (forall y. (x -> Maybe y -> y) -> Maybe y -> y) -> Llun x
build1 f = f (:&?) Nothing

inits :: Llun x -> Llun (Llun x)
inits = fromJust . llun . Maybe.mapMaybe llun . List.tail . List.inits . toList

tails :: Llun x -> Llun (Llun x)
tails xs = build1 \(.&?) end ->
  fix (\go x@(_ :&? y) -> x .&? maybe end (Just . go) y) xs

zip :: Llun x -> Llun y -> Llun (x, y)
zip = zipWith (,)

zipWith :: (x -> y -> z) -> Llun x -> Llun y -> Llun z
zipWith (+) (x :| xs) (y :| ys) = x + y :| List.zipWith (+) xs ys

unzip :: Llun (x, y) -> (Llun x, Llun y)
unzip = \case
  Llun (x, y) -> (Llun x, Llun y)
  (x, y) :& xys -> case unzip xys of (xs, ys) -> (x :& xs, y :& ys)

-- instance MonadZip Llun where
--   mzip :: Llun x -> Llun y -> Llun (x, y)
--   mzip = zip
--   mzipWith :: (x -> y -> z) -> Llun x -> Llun y -> Llun z
--   mzipWith = zipWith
--   munzip :: Llun (x, y) -> (Llun x, Llun y)
--   munzip = unzip

-- instance MonadFix Llun where
--   mfix :: (x -> Llun x) -> Llun x
--   mfix f = case fix (f . head) of (x :| _) -> x :| mfix (tail . f)

accuml :: (a -> x -> (a, y)) -> a -> Llun x -> (a, Llun y)
accuml (+) a0 (x :&? xs) = case a0 + x of
  (a, y) -> maybe (a, Llun y) (fmap (y :&) . accuml (+) a) xs

accumr :: (a -> x -> (a, y)) -> a -> Llun x -> (a, Llun y)
accumr (+) a0 = \case
  Llun x -> Llun <$> (a0 + x)
  x :& xs -> case accumr (+) a0 xs of (a, ys) -> (a + x) <&> (:& ys)

scanl :: (y -> x -> y) -> y -> [x] -> Llun y
scanl = fix \go f y -> \case
  [] -> Llun y
  x : xs -> go f (f y x) xs

scanl' :: (y -> x -> y) -> y -> [x] -> Llun y
scanl' = fix \go f !y -> \case
  [] -> Llun y
  x : xs -> go f (f y x) xs

scanl1 :: (x -> x -> x) -> Llun x -> Llun x
scanl1 f (x :| xs) = scanl f x xs

scanl1' :: (x -> x -> x) -> Llun x -> Llun x
scanl1' f (x :| xs) = scanl' f x xs

scanr :: (x -> y -> y) -> y -> [x] -> Llun y
scanr = fix \go f y -> \case
  [] -> Llun y
  x : xs -> go f (f x y) xs

scanr1 :: (x -> x -> x) -> Llun x -> Llun x
scanr1 f (x :| xs) = scanr f x xs

mapMaybe :: (x -> Maybe y) -> Llun x -> Maybe (Llun y)
mapMaybe = fix \go f (x :&? xs) ->
  maybe id ((Just .) . (:&?)) (f x) (go f =<< xs)

catMaybes :: Llun (Maybe x) -> Maybe (Llun x)
catMaybes = mapMaybe id

take :: Int -> Llun x -> Maybe (Llun x)
take n (x :| xs) = guard (n > 0) $> (x :| List.take (pred n) xs)

drop :: Int -> Llun x -> Maybe (Llun x)
drop n (x :&? xs) = if n <= 0 then Just (x :&? xs) else drop (pred n) =<< xs

takeWhile :: (x -> Bool) -> Llun x -> Maybe (Llun x)
takeWhile p (x :&? xs) = guard (p x) >> (fmap (x :&) . takeWhile p =<< xs)

dropWhile :: (x -> Bool) -> Llun x -> Maybe (Llun x)
dropWhile p (x :&? xs) = if p x then dropWhile p =<< xs else Just (x :&? xs)

delete :: (Eq x) => x -> Llun x -> Maybe (Llun x)
delete = deleteBy (==)

deleteBy :: (x -> x -> Bool) -> x -> Llun x -> Maybe (Llun x)
deleteBy eq y (x :&? xs) = (guard (eq x y) >> xs) <|> (deleteBy eq y =<< xs)

(\\) :: (Eq x) => Llun x -> Llun x -> Maybe (Llun x)
xs \\ os = filter (not . (`elem` os)) xs

filter :: (x -> Bool) -> Llun x -> Maybe (Llun x)
filter p (x :&? xs) = (if p x then Just . (x :&?) else id) (filter p =<< xs)

span :: (x -> Bool) -> Llun x -> ([x], [x])
span p = List.span p . toList

break :: (x -> Bool) -> Llun x -> ([x], [x])
break p = List.break p . toList

partition :: (x -> Bool) -> Llun x -> ([x], [x])
partition p = List.partition p . toList

splitAt :: Int -> Llun x -> ([x], [x])
splitAt n xs = (unLlun (take n xs), unLlun (drop n xs))

index :: (Integral n) => Llun x -> Llun (n, x)
index = zip (iterated succ 0)

notElem :: (Eq x) => x -> Llun x -> Bool
notElem = (not .) . elem

elem :: (Eq x) => x -> Llun x -> Bool
elem = (isJust .) . elemIndex

elemIndex :: (Eq x) => x -> Llun x -> Maybe Int
elemIndex = findIndex . (==)

elemIndices :: (Eq x) => x -> Llun x -> Maybe (Llun Int)
elemIndices = findIndices . (==)

find :: (x -> Bool) -> Llun x -> Maybe x
find p = fmap head . filter p

findIndex :: (x -> Bool) -> Llun x -> Maybe Int
findIndex p = fmap head . findIndices p

findIndices :: (x -> Bool) -> Llun x -> Maybe (Llun Int)
findIndices p xs = sequence $ index xs <&> \(i, x) -> guard (p x) $> i

(!?) :: Llun x -> Int -> Maybe x
(x :&? xs) !? n
  | n < 0 = Nothing
  | n == 0 = Just x
  | otherwise = xs >>= (!? pred n)

lookup :: Int -> Llun x -> Maybe x
lookup = flip (!?)

sort :: (Ord x) => Llun x -> Llun x
sort = fromJust . llun . List.sort . toList

sortOn :: (Ord y) => (x -> y) -> Llun x -> Llun x
sortOn f = fromJust . llun . List.sortOn f . toList

sortBy :: (x -> x -> Ordering) -> Llun x -> Llun x
sortBy f = fromJust . llun . List.sortBy f . toList

group :: (Eq x) => Llun x -> Llun (Llun x)
group = groupBy (==)

groupOn :: (Eq y) => (x -> y) -> Llun x -> Llun (Llun x)
groupOn f = groupBy (on (==) f)

groupBy :: (x -> x -> Bool) -> Llun x -> Llun (Llun x)
groupBy eq (x :| lx) = case List.span (eq x) lx of
  (xs, ys) -> (x :| xs) :&? onLlun (groupBy eq) ys

intersect :: (Eq x) => Llun x -> Llun x -> Maybe (Llun x)
intersect = intersectBy (==)

intersectOn :: (Eq y) => (x -> y) -> Llun x -> Llun x -> Maybe (Llun x)
intersectOn f = intersectBy (on (==) f)

intersectBy :: (x -> y -> Bool) -> Llun x -> Llun y -> Maybe (Llun x)
intersectBy eq xs ys = for xs \x -> guard (Fold.any (eq x) ys) $> x

union :: (Eq x) => Llun x -> Llun x -> Llun x
union = unionBy (==)

unionOn :: (Eq y) => (x -> y) -> Llun x -> Llun x -> Llun x
unionOn f = unionBy (on (==) f)

unionBy :: (x -> x -> Bool) -> Llun x -> Llun x -> Llun x
unionBy eq xs ys =
  xs <> Fold.foldr ((fromJust .) . deleteBy eq) (nubBy eq ys) (toList xs)

nub :: (Eq x) => Llun x -> Llun x
nub = nubBy (==)

nubOn :: (Eq y) => (x -> y) -> Llun x -> Llun x
nubOn f = nubBy (on (==) f)

nubBy :: (x -> x -> Bool) -> Llun x -> Llun x
nubBy eq (x :| xs) = x :| List.nubBy eq (List.filter (not . eq x) xs)

maximum :: (Ord x) => Llun x -> x
maximum = Fold.maximum

maximumOf :: (Ord y) => (x -> y) -> Llun x -> y
maximumOf f = maximum . fmap f

maximumOn :: (Ord x, Ord y) => (x -> y) -> Llun x -> x
maximumOn f = maximumBy (comparing f)

maximumBy :: (x -> x -> Ordering) -> Llun x -> x
maximumBy = Fold.maximumBy

minimum :: (Ord x) => Llun x -> x
minimum = Fold.minimum

minimumOf :: (Ord y) => (x -> y) -> Llun x -> y
minimumOf f = minimum . fmap f

minimumOn :: (Ord x, Ord y) => (x -> y) -> Llun x -> x
minimumOn f = minimumBy (comparing f)

minimumBy :: (x -> x -> Ordering) -> Llun x -> x
minimumBy = Fold.minimumBy

iterate :: (x -> x) -> x -> Llun x
iterate f x = x :& iterate f (f x)

iterated :: (x -> x) -> x -> Llun x
iterated f !x = x :& iterated f (f x)

repeat :: x -> Llun x
repeat = fix (ap (:&))

replicate :: Int -> x -> Maybe (Llun x)
replicate n x = take n (repeat x)

cycle :: Llun x -> Llun x
cycle = fix (ap (<>))

intersperse :: x -> Llun x -> Llun x
intersperse y (x :&? xs) = x :&? fmap ((y :&) . intersperse y) xs

intercalate :: Llun x -> Llun (Llun x) -> Llun x
intercalate = (join .) . intersperse

transpose :: Llun (Llun x) -> Llun (Llun x)
transpose = \case
  Llun x -> traverse Llun x
  (x :&? xs) :& xss -> case unzip (xss <&> \(h :&? t) -> (h, t)) of
    (hs, ts) -> (x :& hs) :&? maybe id ((fmap transpose .) . fmap . (:&)) xs (catMaybes ts)

subsequences :: Llun x -> Llun (Llun x)
subsequences (x :&? xs) =
  Llun x :&? fmap (ap (:&) (Llun . (x :&)) <=< subsequences) xs

permutations :: Llun x -> Llun (Llun x)
permutations = (:&?) <*> fmap join . diagonally \(t :| ts) -> fmap (<& ts) . insertions t <=< permutations

diagonally :: (Llun x -> Llun x -> y) -> Llun x -> Maybe (Llun y)
diagonally f xs =
  catMaybes $
    zipWith
      (liftM2 f)
      ((Just <$> tails xs) &: Nothing)
      (Nothing :& (Just <$> inits xs))

diagonals :: Llun x -> Maybe (Llun (Llun x, Llun x))
diagonals = diagonally (,)

-- > insertions x (a : b : cs)
-- >   == (x : a : b : cs)
-- >    : (a : x : b : cs)
-- >    : (a : b : x : cs) ...
insertions :: x -> Llun x -> Llun (Llun x)
insertions x ly@(y :&? ys) = (x :& ly) :&? (fmap (y :&) . insertions x <$> ys)

compareLength :: Llun x -> Llun y -> Ordering
compareLength xs ys = compare (void xs) (void ys)
