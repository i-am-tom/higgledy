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
Module      : Data.Partial.Position
Description : Partial structure type declarations.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental
-}
module Data.Partial.Types
  ( Partial (..)

  , Partial_
  , GPartial_
  ) where

import Data.Monoid (Last (..))
import Data.Function (on)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..))
import Test.QuickCheck.Function (Function (..), functionMap)

-- | A partial structure is a version of a structure in which every parameter
-- is optional. We can interact with a partial structure using the API
-- provided, and eventually use the @impartial@ lens to attempt to build a
-- complete structure from our partial data set.
--
-- >>> import Control.Lens
-- >>> import Data.Partial.Build
--
-- We can attempt a construction and fail:
--
-- >>> mempty @(Partial (Int, String, Bool)) ^? impartial
-- Nothing
--
-- ... or succeed!
--
-- >>> toPartial ("Hello", True) ^? impartial
-- Just ("Hello",True)
newtype Partial (structure :: Type)
  = Partial { runPartial :: Partial_ structure Void }

-------------------------------------------------------------------------------

-- | Calculate the "partial representation" of a type.
type Partial_ (structure :: Type)
  = GPartial_ (Rep structure)

-- | Calculate the "partial representation" of a generic rep.
type family GPartial_ (rep :: Type -> Type) :: Type -> Type where

  GPartial_ (M1 index meta inner)
    = M1 index meta (GPartial_ inner)

  GPartial_ (left :*: right)
    = GPartial_ left :*: GPartial_ right

  GPartial_ (K1 index value)
    = K1 index (Last value)

  GPartial_ (left :+: right)
    = GPartial_ left :+: GPartial_ right

  GPartial_ U1 = U1
  GPartial_ V1 = V1

-------------------------------------------------------------------------------

instance (Eq tuple, Generic xs, Tuple xs tuple) => Eq (Partial xs) where
  (==) = (==) `on` toTuple

instance (Ord tuple, Generic xs, Tuple xs tuple) => Ord (Partial xs) where
  compare = compare `on` toTuple

instance (Semigroup tuple, Generic xs, Tuple xs tuple)
    => Semigroup (Partial xs) where
  x <> y = fromTuple (toTuple x <> toTuple y)

instance (Monoid tuple, Generic xs, Tuple xs tuple)
    => Monoid (Partial xs) where
  mempty = fromTuple mempty

-------------------------------------------------------------------------------

instance (Arbitrary tuple, GToTuple (Partial_ structure) tuple)
    => Arbitrary (Partial structure) where
  arbitrary = fmap (Partial . gfromTuple) arbitrary

instance (CoArbitrary tuple, GToTuple (Partial_ structure) tuple)
    => CoArbitrary (Partial structure) where
  coarbitrary (Partial x) = coarbitrary (gtoTuple x)

instance (Generic structure, Function tuple, Tuple structure tuple)
    => Function (Partial structure) where
  function = functionMap toTuple fromTuple

-- | We can 'show' a partial structure, and simply replace its missing fields
-- with "???".
--
-- >>> mempty @(Partial (Int, String, Bool))
-- (,,) ??? ??? ???
--
-- >>> import Data.Partial.Build
-- >>> toPartial ("Hello", True)
-- (,) "Hello" True
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

instance Show inner => GShow named (K1 R (Last inner)) where
  gshow (K1 x) = maybe "???" show (getLast x)

instance (Generic structure, GShow 'True (Partial_ structure))
    => Show (Partial structure) where
  show (Partial x) = gshow @'True x

-------------------------------------------------------------------------------

class Tuple (structure :: Type) (tuple :: Type)
    | structure -> tuple where
  toTuple   :: Partial structure -> tuple
  fromTuple :: tuple -> Partial structure

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

instance (Generic structure, GToTuple (Partial_ structure) tuple)
    => Tuple structure tuple where
  toTuple = gtoTuple . runPartial
  fromTuple = Partial . gfromTuple
