{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Data.Generic.HKD.Types
Description : Type declarations for the HKD structure.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental
-}
module Data.Generic.HKD.Types
  ( HKD (..)

  , HKD_
  , GHKD_

  , Nested (..)
  , Tuple (..)
  ) where

import Data.Barbie (ConstraintsB (..), FunctorB (..), ProductB (..), ProductBC (..), TraversableB (..))
import Data.Barbie.Constraints (Dict (..))
import Data.Function (on)
import Data.Functor.Contravariant (Contravariant (..), phantom)
import Data.Functor.Product (Product (..))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..))
import Test.QuickCheck.Function (Function (..), functionMap)

-- | Higher-kinded data (HKD) is the design pattern in which every field in our
-- type is wrapped in some functor @f@:
--
-- @
--   data User f
--    = User
--        { name :: f String
--        , age  :: f Int
--        }
-- @
--
-- Depending on the functor, we can get different behaviours: with 'Maybe', we
-- get a partial structure; with 'Validation', we get a piecemeal validator;
-- and so on. The @HKD@ newtype allows us to lift any type into an HKD-style
-- API via its generic representation.
--
-- >>> :set -XDeriveGeneric -XTypeApplications
-- >>> :{
-- data User
--   = User { name :: String, age :: Int }
--   deriving Generic
-- :}
--
-- The @HKD@ type is indexed by our choice of functor and the structure we're
-- lifting. In other words, we can define a synonym for our behaviour:
--
-- >>> import Data.Monoid (Last (..))
-- >>> type Partial a = HKD a Last
--
-- ... and then we're ready to go!
--
-- >>> mempty @(Partial User)
-- User {name = Last {getLast = Nothing}, age = Last {getLast = Nothing}}
--
-- >>> mempty @(HKD (Int, Bool) [])
-- (,) [] []
newtype HKD (structure :: Type) (f :: Type -> Type)
  = HKD { runHKD :: HKD_ f structure Void }

instance (Contravariant (HKD_ f structure), Functor (HKD_ f structure))
    => Generic (HKD structure f) where
  type Rep (HKD structure f) = HKD_ f structure

  from = phantom . runHKD
  to   = HKD . phantom

-------------------------------------------------------------------------------

newtype Nested (x :: Type)
  = Nested { unnest :: x }
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------

-- | Calculate the "partial representation" of a type.
type HKD_ (f :: Type -> Type) (structure :: Type)
  = GHKD_ f (Rep structure)

-- | Calculate the "partial representation" of a generic rep.
type family GHKD_ (f :: Type -> Type) (rep :: Type -> Type)
    = (output :: Type -> Type) | output -> f rep where
  GHKD_ f (M1 index meta inner)     = M1 index meta (GHKD_ f inner)
  GHKD_ f (left :*: right)          = GHKD_ f left :*: GHKD_ f right
  GHKD_ f (K1 index (Nested inner)) = K1 index (HKD inner f)
  GHKD_ f (K1 index value)          = K1 index (f value)
  GHKD_ f (left :+: right)          = GHKD_ f left :+: GHKD_ f right

-------------------------------------------------------------------------------

instance (Eq tuple, Generic xs, Tuple f xs tuple)
    => Eq (HKD xs f) where
  (==) = (==) `on` toTuple

instance (Ord tuple, Generic xs, Tuple f xs tuple)
    => Ord (HKD xs f) where
  compare = compare `on` toTuple

instance (Semigroup tuple, Generic xs, Tuple f xs tuple)
    => Semigroup (HKD xs f) where
  x <> y = fromTuple (toTuple x <> toTuple y)

instance (Monoid tuple, Generic xs, Tuple f xs tuple)
    => Monoid (HKD xs f) where
  mempty = fromTuple mempty

-------------------------------------------------------------------------------

instance (Arbitrary tuple, GToTuple (HKD_ f structure) tuple)
    => Arbitrary (HKD structure f) where
  arbitrary = fmap (HKD . gfromTuple) arbitrary

instance (CoArbitrary tuple, GToTuple (HKD_ f structure) tuple)
    => CoArbitrary (HKD structure f) where
  coarbitrary (HKD x) = coarbitrary (gtoTuple x)

instance (Generic structure, Function tuple, Tuple f structure tuple)
    => Function (HKD structure f) where
  function = functionMap toTuple fromTuple

-------------------------------------------------------------------------------

class GShow (named :: Bool) (rep :: Type -> Type) where
  gshow :: rep p -> String

instance GShow named inner => GShow named (D1 meta inner) where
  gshow = gshow @named . unM1

instance (GShow 'True inner, KnownSymbol name)
    => GShow any (C1 ('MetaCons name fixity 'True) inner) where
  gshow (M1 x) = symbolVal (Proxy @name) <> " {" <> gshow @'True x <> "}"

instance (GShow 'False inner, KnownSymbol name)
    => GShow any (C1 ('MetaCons name fixity 'False) inner) where
  gshow (M1 x) = symbolVal (Proxy @name) <> " " <> gshow @'False x

instance (GShow 'True left, GShow 'True right)
    => GShow 'True (left :*: right) where
  gshow (left :*: right) = gshow @'True left <> ", " <> gshow @'True right

instance (GShow 'False left, GShow 'False right)
    => GShow 'False (left :*: right) where
  gshow (left :*: right) = gshow @'False left <> " " <> gshow @'False right

instance (GShow 'True inner, KnownSymbol field)
    => GShow 'True (S1 ('MetaSel ('Just field) i d c) inner) where
  gshow (M1 inner) = symbolVal (Proxy @field) <> " = " <> gshow @'True inner

instance GShow 'False inner => GShow 'False (S1 meta inner) where
  gshow (M1 inner) = gshow @'False inner

instance (Show (f inner)) => GShow named (K1 R (f inner)) where
  gshow (K1 x) = show x

instance (Generic structure, GShow 'True (HKD_ f structure))
    => Show (HKD structure f) where
  show (HKD x) = gshow @'True x

-------------------------------------------------------------------------------

class Tuple (f :: Type -> Type) (structure :: Type) (tuple :: Type)
    | f structure -> tuple where
  toTuple   :: HKD structure f -> tuple
  fromTuple :: tuple -> HKD structure f

class GToTuple (rep :: Type -> Type) (tuple :: Type)
    | rep -> tuple where
  gfromTuple :: tuple -> rep p
  gtoTuple   :: rep p -> tuple

instance GToTuple inner tuple
    => GToTuple (M1 index meta inner) tuple where
  gfromTuple = M1 . gfromTuple
  gtoTuple   = gtoTuple . unM1

instance (GToTuple left left', GToTuple right right')
    => GToTuple (left :*: right) (left', right') where
  gfromTuple (x, y) = gfromTuple x :*: gfromTuple y
  gtoTuple (x :*: y) = (gtoTuple x, gtoTuple y)

instance GToTuple (K1 index inner) inner where
  gfromTuple = K1
  gtoTuple = unK1
