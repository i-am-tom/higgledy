{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
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
  ) where

import Data.Function (on)
import Data.Kind (Type)
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
--        { name      :: f String
--        , age       :: f Int
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
-- >>> type Partial = HKD Last
--
-- ... and then we're ready to go!
--
-- >>> mempty @(Partial User)
-- User {name = Last {getLast = Nothing}, age = Last {getLast = Nothing}}
--
-- >>> mempty @(HKD [] (Int, Bool))
-- (,) [] []
newtype HKD (f :: Type -> Type) (structure :: Type)
  = HKD { runHKD :: HKD_ f structure Void }

-------------------------------------------------------------------------------

-- | Calculate the "partial representation" of a type.
type HKD_ (f :: Type -> Type) (structure :: Type)
  = GHKD_ f (Rep structure)

-- | Calculate the "partial representation" of a generic rep.
type family GHKD_ (f :: Type -> Type) (rep :: Type -> Type) :: Type -> Type where
  GHKD_ f (M1 index meta inner) = M1 index meta (GHKD_ f inner)
  GHKD_ f (left :*: right)      = GHKD_ f left :*: GHKD_ f right
  GHKD_ f (K1 index value)      = K1 index (f value)
  GHKD_ f (left :+: right)      = GHKD_ f left :+: GHKD_ f right
  GHKD_ f  U1                   = U1
  GHKD_ f  V1                   = V1

-------------------------------------------------------------------------------

instance (Eq tuple, Generic xs, Tuple f xs tuple)
    => Eq (HKD f xs) where
  (==) = (==) `on` toTuple

instance (Ord tuple, Generic xs, Tuple f xs tuple)
    => Ord (HKD f xs) where
  compare = compare `on` toTuple

instance (Semigroup tuple, Generic xs, Tuple f xs tuple)
    => Semigroup (HKD f xs) where
  x <> y = fromTuple (toTuple x <> toTuple y)

instance (Monoid tuple, Generic xs, Tuple f xs tuple)
    => Monoid (HKD f xs) where
  mempty = fromTuple mempty

-------------------------------------------------------------------------------

instance (Arbitrary tuple, GToTuple (HKD_ f structure) tuple)
    => Arbitrary (HKD f structure) where
  arbitrary = fmap (HKD . gfromTuple) arbitrary

instance (CoArbitrary tuple, GToTuple (HKD_ f structure) tuple)
    => CoArbitrary (HKD f structure) where
  coarbitrary (HKD x) = coarbitrary (gtoTuple x)

instance (Generic structure, Function tuple, Tuple f structure tuple)
    => Function (HKD f structure) where
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
    => Show (HKD f structure) where
  show (HKD x) = gshow @'True x

-------------------------------------------------------------------------------

class Tuple (f :: Type -> Type) (structure :: Type) (tuple :: Type)
    | f structure -> tuple where
  toTuple   :: HKD f structure -> tuple
  fromTuple :: tuple -> HKD f structure

class Function tuple => GToTuple (rep :: Type -> Type) (tuple :: Type)
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

instance Function inner => GToTuple (K1 index inner) inner where
  gfromTuple = K1
  gtoTuple = unK1

instance (Generic structure, GToTuple (HKD_ f structure) tuple)
    => Tuple f structure tuple where
  toTuple = gtoTuple . runHKD
  fromTuple = HKD . gfromTuple
