{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Partial.Types where

import Data.Monoid (Last (..))
import Data.Function (on)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..))
import Test.QuickCheck.Function (Function (..), functionMap)

newtype Partial (structure :: Type)
  = Partial
      { runPartial :: Partial_ structure Void
      }

type Partial_ (structure :: Type)
  = GPartial_ (Rep structure)

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

instance (Arbitrary tuple, GToTuple (Partial_ structure) tuple)
    => Arbitrary (Partial structure) where
  arbitrary = fmap (Partial . gfromTuple) arbitrary

instance (CoArbitrary tuple, GToTuple (Partial_ structure) tuple)
    => CoArbitrary (Partial structure) where
  coarbitrary (Partial x) = coarbitrary (gtoTuple x)

-- | For complete partial structures, the 'Show' instances should match (though
-- there are some edge-cases around, say, rendering of negative numbers).
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

class Tuple (structure :: Type) (tuple :: Type)
    | structure -> tuple where
  toTuple   :: Partial structure -> tuple 
  fromTuple :: tuple -> Partial structure

instance (Generic structure, GToTuple (Partial_ structure) tuple)
    => Tuple structure tuple where
  toTuple = gtoTuple . runPartial
  fromTuple = Partial . gfromTuple

instance (Generic structure, Function tuple, Tuple structure tuple)
    => Function (Partial structure) where
  function = functionMap toTuple fromTuple
