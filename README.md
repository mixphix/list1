# `list1`

Striving toward a better interface for `NonEmpty`.

If you know it's not null, you have at least one `Sole` thing, or something & some other things!

```hs
type List1 = NonEmpty

pattern Sole :: x -> List1 x
pattern Sole x = x :| []

infixr 5 :&, :&?

-- note the lazy matches!
pattern (:&) :: x -> List1 x -> List1 x
pattern x :& y <- (x :| ~(list1 -> Just y))
  where
    x :& ~(y :| ys) = x :| (y : ys)

{-# COMPLETE Sole, (:&) #-}

pattern (:&?) :: x -> Maybe (List1 x) -> List1 x
pattern x :&? y <- (x :| ~(list1 -> y))
  where
    x :&? y = maybe (Sole x) (x :&) y

{-# COMPLETE (:&?) #-}

list1 :: [x] -> Maybe (List1 x)
list1 = \case
  [] -> Nothing
  x : xs -> Just (x :| xs)
```

These patterns made writing a lot of the definitions in this module easier, and generally makes working with the cases of a `NonEmpty` list much more ergonomic.

This module also offers some implementations of `NonEmpty` functions implemented using these patterns. Exact feature parity with `Data.List` is not the goal of this package. The intent is to explore the design space: many of the functions that may delete items return a `Maybe (List1 x)`, which is in turn made less unwieldy by having the `(:&?)` bidirectional pattern.
