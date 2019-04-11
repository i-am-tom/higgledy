# Higgledy 📚

Higher-kinded data via generics: all\* the benefits, but none\* of the
boilerplate.

## Introduction

When we work with [higher-kinded
data](https://reasonablypolymorphic.com/blog/higher-kinded-data), we find
ourselves writing types like:

```haskell
data User f
  = User
      { name :: f String
      , age  :: f Int
        ...
      }
  deriving (FunctorB, ProductB, ...)
```

This is good - we can use `f ~ Maybe` for partial data, `f ~ Identity` for
complete data, etc - but it introduces a fair amount of noise, and we have a
lot of boilerplate deriving to do. Wouldn't it be nice if we could get back to
writing simple types as we know and love them, and get all this stuff for free?

```haskell
data User
  = User
      { name :: String
      , age  :: Int
      , ...
      }
  deriving Generic

-- We can recover something isomorphic to the first example with:
type UserF f = HKD UserF f
```

As an added little bonus, any `HKD`-wrapped object is automatically an instance
of all the [Barbie](http://hackage.haskell.org/package/barbies) classes, so no
need to derive anything more than `Generic`!

