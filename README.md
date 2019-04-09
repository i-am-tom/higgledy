# Higgledy 📚

Get partial versions of any `Generic`-deriving type... for free!

_All the examples below were written using the following extensions, modules,
and data types:_

```haskell
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
module Example where

import Control.Lens ((^?), (?~), (^.), re)
import Data.Function ((&))
import Data.Partial
import GHC.Generics (Generic)

-- A record (named fields)
data User
  = User
      { name      :: String
      , age       :: Int
      , likesDogs :: Bool
      }
  deriving (Generic, Show)

-- A product (unnamed fields)
data Triple
  = Triple Int String Bool
  deriving (Generic, Show)
```

## Creating empty partial structures

``` haskell
eg0 :: Partial User
eg0 = mempty
-- User {name = ???, age = ???, likesDogs = ???}
```

```haskell
eg1 :: Partial Triple
eg1 = mempty
-- Triple ??? ??? ???
```

## Populating single fields

```haskell
eg2 :: Partial User
eg2 = mempty & field @"name" ?~ "Tom"
-- User {name = "Tom", age = ???, likesDogs = ???}

eg3 :: Partial User
eg3 = mempty & field @"age" ?~ 25
-- User {name = ???, age = 25, likesDogs = ???}

eg4 :: Partial User
eg4 = mempty & field @"likesDogs" ?~ "Oops!"
-- error
-- • Couldn't match type ‘Bool’ with ‘[Char]’

eg5 :: Partial User
eg5 = mempty & field @"1x" ?~ True
-- error:
-- • User has no field called "1x"!
```

```haskell
eg6 :: Partial Triple
eg6 = mempty & position @1 ?~ 3
-- Triple 3 ??? ???

eg7 :: Partial Triple
eg7 = mempty & position @2 ?~ "Hello!"
-- Triple ??? "Hello!" ???

eg8 :: Partial Triple
eg8 = mempty & position @1 ?~ ()
-- error:
-- • Couldn't match type ‘Int’ with ‘()’

eg9 :: Partial Triple
eg9 = mempty & position @4 ?~ ()
-- error:
-- • Triple has no position #4!
```

## Populating multiple fields

```haskell
eg10 :: Partial User
eg10 = eg2 <> eg3
-- User {name = "Tom", age = 25, likesDogs = ???}

eg11 :: Partial User
eg11 = mempty & (field @"name"      ?~ "Evil Tom")
              . (field @"likesDogs" ?~      False)
-- User {name = "Evil Tom", age = ???, likesDogs = False}

eg12 :: Partial Triple
eg12 = eg6 <> eg7
-- Triple 3 "Hello!" ???

eg13 :: Partial Triple
eg13 = mempty & (position @1 ?~    3)
              . (position @3 ?~ True)
-- Triple 3 ??? True
```

## Checking whether fields are populated

```haskell
eg14 :: Maybe String
eg14 = eg11 ^. field @"name"
-- Just "Evil Tom"
```

```haskell
eg15 :: Maybe String
eg15 = eg12 ^. position @2
-- Just "Hello!"
```

## Populating all the fields

```haskell
user :: User
user = User "Good Tom" 25 True

eg16 :: Partial User
eg16 = toPartial user
-- User {name = "Good Tom", age = 25, likesDogs = True}

eg17 :: Partial User
eg17 = user ^. re impartial
-- User {name = "Good Tom", age = 25, likesDogs = True}

eg18 :: Partial User
eg18 = eg11 & field @"age" ?~ 24
-- User {name = "Evil Tom", age = 24, likesDogs = False}
```

```haskell
triple :: Triple
triple = Triple 123 "ABC" True

eg19 :: Partial Triple
eg19 = toPartial triple
-- Triple 123 "ABC" True

eg20 :: Partial Triple
eg20 = triple ^. re impartial
-- Triple 123 "ABC" True

eg21 :: Partial Triple
eg21 = eg13 & position @2 ?~ "XYZ"
-- Triple 3 "XYZ" True
```

## Extracting from partials

```haskell
eg22 :: User
eg22 = withDefaults user (mempty & field @"name" ?~ "Tim")
-- User {name = "Tim", age = 25, likesDogs = True}

eg23 :: Maybe User
eg23 = fromPartial eg14
-- Just (User {name = "Good Tom", age = 25, likesDogs = True})

eg24 :: Maybe User
eg24 = eg15 ^? impartial
-- Just (User {name = "Good Tom", age = 25, likesDogs = True})
```

```haskell
eg25 :: Triple
eg25 = withDefaults triple (mempty & position @1 ?~ 789)
-- Triple 789 "ABC" True

eg26 :: Maybe Triple
eg26 = fromPartial eg17
-- Just (Triple 123 "ABC" True)

eg27 :: Maybe Triple
eg27 = eg18 ^? impartial
-- Just (Triple 123 "ABC" True)
```
