# `llun`

Striving toward a better interface for `NonEmpty`.

If you know it's "not null", you have at least one thing; something that's "all alone" or something & some other things!

```hs
type Llun = NonEmpty

pattern Llun :: x -> Llun x
pattern Llun x = x :| []

infixr 5 :&, :&?

-- note the lazy matches!
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

llun :: [x] -> Maybe (Llun x)
llun = \case
  [] -> Nothing
  x : xs -> Just (x :| xs)
```

These patterns made writing a lot of the definitions in this module easier, and generally makes working with the cases of a `NonEmpty` list much more ergonomic.

This module also offers some implementations of `NonEmpty` functions implemented using these patterns. Exact feature parity with `Data.List` is not the goal of this package. The intent is to explore the design space: many of the functions that may delete items return a `Maybe (Llun x)`, which is in turn made less unwieldy by having the `(:&?)` bidirectional pattern.
