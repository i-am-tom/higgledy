# Higgledy 📚

Higher-kinded data via generics: all\* the benefits, but none\* of the
boilerplate.

## Introduction

When we work with [higher-kinded
data](https://reasonablypolymorphic.com/blog/higher-kinded-data), we find
ourselves writing types like:

```{haskell, ignore}
data User f
  = User
      { name :: f String
      , age  :: f Int
      , ...
      }
```

This is good - we can use `f ~ Maybe` for partial data, `f ~ Identity` for
complete data, etc - but it introduces a fair amount of noise, and we have a
lot of boilerplate deriving to do. Wouldn't it be nice if we could get back to
writing simple types as we know and love them, and get all this stuff for
_free_?

```{haskell, ignore}
data User
  = User
      { name :: String
      , age  :: Int
      , ...
      }
  deriving Generic

-- HKD for free!
type UserF f = HKD User f
```

As an added little bonus, any `HKD`-wrapped object is automatically an instance
of all the [Barbie](https://hackage.haskell.org/package/barbies) classes, so no
need to derive anything more than `Generic`!

## API

All examples below were compiled with the following extensions, modules, and
example data types:

```haskell
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
module Main where

import Control.Applicative (Alternative (empty))
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Data.Barbie (ProductB (buniq))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Last (..))
import GHC.Generics (Generic)
import Named ((:!), (!))

-- An example of a record (with named fields):
data User
  = User
      { name      :: String
      , age       :: Int
      , likesDogs :: Bool
      }
  deriving (Generic, Show)

user :: User
user = User "Tom" 26 True

-- An example of a product (without named fields):
data Triple
  = Triple Int () String
  deriving (Generic, Show)

triple :: Triple
triple = Triple 123 () "ABC"
```

### The HKD type constructor

The `HKD` type takes two parameters: your model type, and the functor in which
we want to wrap all our inputs. By picking different functors for the second
parameter, we can recover various behaviours:

```haskell
type Partial a = HKD a  Last          -- Fields may be missing.
type Bare    a = HKD a  Identity      -- All must be present.
type Labels  a = HKD a (Const String) -- Every field holds a string.
```

_NB: as of GHC 8.8, the `Last` monoid will be removed in favour of `Compose
Maybe Last` (using the `Last` in `Data.Semigroup`). Until then, I'll use `Last`
for brevity, but you may wish to use this suggestion for future-proofing._

### Fresh objects

When we want to start working with the `HKD` interface, we have a couple of
options, depending on the functor in question. The first option is to use
`mempty`:

```haskell
eg0 :: Partial User
eg0 = mempty
-- User
--   { name      = Last {getLast = Nothing}
--   , age       = Last {getLast = Nothing}
--   , likesDogs = Last {getLast = Nothing}
--   }
```

Other 'Alternative'-style functors lead to very different results:

```haskell
eg1 :: Labels Triple
eg1 = mempty
-- Triple
--   Const ""
--   Const ""
--   Const ""
```

Of course, this method requires every field to be monoidal. If we try with
`Identity`, for example, we're in trouble if all our fields aren't themselves
monoids:

```{haskell, ignore}
eg2 :: Bare Triple
eg2 = mempty
-- error:
-- • No instance for (Monoid Int) arising from a use of ‘mempty’
```

The other option is to `deconstruct` a complete object. This effectively lifts
a type into the `HKD` structure with `pure` applied to each field:

```haskell
eg3 :: Bare User
eg3 = deconstruct user
-- User
--   { name      = Identity "Tom"
--   , age       = Identity 26
--   , likesDogs = Identity True
--   }
```

This approach works with any applicative we like, so we can recover the other
behaviours:

```haskell
eg4 :: Partial Triple
eg4 = deconstruct @Last triple
-- Triple
--   Last {getLast = Just 123}
--   Last {getLast = Just ()}
--   Last {getLast = Just "ABC"}
```

There's also `construct` for when we want to escape our `HKD` wrapper, and
attempt to _construct_ our original type:

```haskell
eg5 :: Last Triple
eg5 = construct eg4
-- Last {getLast = Just (Triple 123 () "ABC")}
```

If none of the above suit your needs, maybe you want to try `build` on for
size. This function constructs an `HKD`-wrapped version of the type supplied to
it by taking all its parameters. In other words:

```haskell
eg6 :: f Int -> f () -> f String -> HKD Triple f
eg6 = build @Triple

eg7 :: HKD Triple []
eg7 = eg6 [1] [] ["Tom", "Tim"]
-- Triple [1] [] ["Tom","Tim"]
```

Should we need to work with records, we can exploit the label trickery of the
[`named`](https://hackage.haskell.org/package/named) package. The `record`
function behaves exactly as `build` does, but produces a function compatible
with the `named` interface. After that, we can use the function with labels
(and with no regard for the internal order):

```haskell
eg8 :: "name"      :! f [Char]
    -> "age"       :! f Int
    -> "likesDogs" :! f Bool
    -> HKD User f
eg8 = record @User

eg9 :: HKD User Maybe
eg9 = eg8 ! #name (Just "Tom")
          ! #likesDogs (Just True)
          ! #age (Just 26)
```

If you're _still_ not satisfied, check out the
[`buniq`](https://hackage.haskell.org/package/barbies-1.1.2.1/docs/Data-Barbie.html#v:buniq)
method hiding in `barbies`:

```haskell
eg10 :: HKD Triple []
eg10 = buniq empty
-- Triple [] [] []
```

### Field Access

The `field` lens, when given a type-applied field name, allows us to focus on
fields within a record:

```haskell
eg11 :: Last Int
eg11 = eg0 ^. field @"age"
-- Last {getLast = Nothing}
```

As this is a true `Lens`, it also means that we can _set_ values within our
record (note that these set values will _also_ need to be in our functor of
choice):

```haskell
eg12 :: Partial User
eg12 = eg0 & field @"name"      .~ pure "Evil Tom"
           & field @"likesDogs" .~ pure False
-- User
--   { name      = Last {getLast = Just "Evil Tom"}
--   , age       = Last {getLast = Nothing}
--   , likesDogs = Last {getLast = Just False}
--   }
```

This also means, for example, we can check whether a particular value has been
completed for a given partial type:

```haskell
eg13 :: Bool
eg13 = anyOf (field @"name") (isJust . getLast) eg0
-- False
```

Finally, thanks to the fact that this library exploits some of the internals of
`generic-lens`, we'll also get a nice type error when we mention a field that
doesn't exist in our type:

```{haskell, ignore}
eg14 :: Identity ()
eg14 = eg3 ^. field @"oops"
-- error:
-- • The type User does not contain a field named 'oops'.
```

### Position Access

Just as with field names, we can use positions when working with non-record
product types:

```haskell
eg15 :: Labels Triple
eg15 = mempty & position @1 .~ Const "hello"
              & position @2 .~ Const "world"
-- Triple
--   Const "hello"
--   Const "world"
--   Const ""
```

Again, this is a `Lens`, so we can just as easily _set_ values:

```haskell
eg16 :: Partial User
eg16 = eg12 & position @2 .~ pure 26
-- User
--   { name      = Last {getLast = Just "Evil Tom"}
--   , age       = Last {getLast = Just 26}
--   , likesDogs = Last {getLast = Just False}
--   }
```

Similarly, the internals here come to us courtesy of `generic-lens`, so the
type errors are a delight:

```{haskell, ignore}
eg17 :: Identity ()
eg17 = deconstruct @Identity triple ^. position @4
-- error:
-- • The type Triple does not contain a field at position 4
```

### Labels

One neat trick we can do - thanks to the generic representation - is get the
names of the fields into the functor we're using. The `label` value gives us
this interface:

```haskell
eg18 :: Labels User
eg18 = label
-- User
--   { name = Const "name"
--   , age = Const "age"
--   , likesDogs = Const "likesDogs"
--   }
```

By combining this with some of the
[Barbies](https://hackage.haskell.org/package/barbies) interface (the entirety
of which is available to any `HKD`-wrapped type) such as `bprod` and `bmap`, we
can implement functions such as `labelsWhere`, which returns the names of all
fields whose values satisfy some predicate:

```haskell
eg19 :: [String]
eg19 = labelsWhere (isNothing . getLast) eg12
-- ["age"]
```

### Documentation

All the docs in this library are tested on `cabal new-test`. Furthermore, this
README is tested by `markdown-unlit`. To keep _that_ happy, we do need a `main`
in this file, so just ignore the following :)

```haskell
main :: IO ()
main = pure ()
```
