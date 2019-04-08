# Higgledy 📚

## Introduction

Every now and then, we need to write a **form**, or a **parser**, and we face a
familiar problem: what do I do when I don't have _all_ my data yet? How do I
store a "partially-complete" version of my type?

```haskell
data User
  = User
      { name      :: String
      , age       :: Int
      , likesDogs :: Bool
      }
  deriving (Generic, Show)
```

A very common approach to this is known as [higher-kinded
data](http://reasonablypolymorphic.com/blog/higher-kinded-data), which involves
us parameterising our data type:

```haskell
data UserHKD f
  = User
      { name      :: f String
      , age       :: f Int
      , likesDogs :: f Bool
      }
```

Now, I can represent the partial version with `UserHKD Maybe`, and the total
version with `UserHKD Identity`. Huzzah! This is totally fine, but it has two
shortcomings:

1. Some of my instances are now harder to derive, as they require some sort of
   quantified constraint in order to interact properly with the `f` parameter
   (e.g. I now need to know that any `Show`-friendly type is /still/
   `Show`-friendly when wrapped in `f`).

2. My abstraction is leaking: my pristine domain types now contain this `f`
   parameter, which doesn't really belong here.

Of course, these are two rather minor points, and we can definitely live with
them, but let's imagine we /couldn't/...

What if the type of a partial `User` were... `Partial User`?

## The `Partial` constructor

## Building
## Manipulation by field
## Manipulation by position
## Defaults
