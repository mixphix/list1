# Revision history for list1

# Unreleased

* `windows :: Int -> List1 x -> Maybe (List1 (List1 x))`
* `consecutiveSubsequences :: List1 x -> List1 (List1 x)`
* fix `accuml`
* relax constraints of `mininumOn` and `maximumOn` to only the target requiring `Ord`
* generalize `ifList1` to an arbitrary `Alternative`
* generalize `whenList1` to `Applicative` instead of `Monad`

## 0.0.3 -- 2024-12-27

* change type of `lookup` to match `Data.List`
* localize definition of `unfoldr`
* reverse `inits` and `tails` in `diagonally`
* export `asList :: ([x] -> [x]) -> List1 x -> List1 x`
* fix `delete` and `deleteBy`
* fix `takeWhile`
* add `unsnoc`
* new function `has1Plus` extending `has01`
* rename `truncate` family to `zipWithTruncate`
* change type of `diagonals`
* change `(<&)` and `(&>)` to `(<|)` and `(|>)` respectively

## 0.0.2 -- 2024-08-10

* function `(?:) :: Maybe (List1 x) -> x -> List1 x` reminiscent of `pattern (:?)`
* renamed `onList1 -> ifList1`, `nE -> has01`
* new function `whenList1 :: (Monad m) => [x] -> (List1 x -> m ()) -> m ()`
* fix `scanl`, `scanl'`, `scanl1`, `scanl1'`, `scanr`, `scanr1`
* fix `intersect`, `intersectOn`, `intersectBy`
* `inits` implementation localized
* more explicit recursion

## 0.0.1

* rename package to `list1`
* rename `pattern Llun` to `pattern Sole`

## 0.0.0 -- 2023-03-26

* `type Llun = NonEmpty`
