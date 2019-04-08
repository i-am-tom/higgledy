{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Partial.Types where

import Control.Applicative ((<|>), empty)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Test.QuickCheck (Gen)
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
    = K1 index (Maybe value)

  GPartial_ (left :+: right)
    = GPartial_ left :+: GPartial_ right

  GPartial_ U1 = U1
  GPartial_ V1 = V1

-- | Partial structures have an 'Eq' instance providing that all the
-- parameters' types have an 'Eq' instance as well.
class GEq (rep :: Type -> Type) where
  geq :: rep p -> rep q -> Bool

instance GEq inner => GEq (M1 index meta inner) where
  geq (M1 x) (M1 y) = geq x y

instance (GEq left, GEq right) => GEq (left :*: right) where
  geq (leftX :*: rightX) (leftY :*: rightY)
    = geq leftX leftY && geq rightX rightY

instance Eq parameter => GEq (K1 index (Maybe parameter)) where
  geq (K1 x) (K1 y) = x == y

instance (Generic structure, GEq (Partial_ structure))
    => Eq (Partial structure) where
  Partial x == Partial y = geq x y

-- | Similarly, partial structures have an ordering if all parameters are also
-- instances of 'Ord'. Note that an "empty" parameter is /less than/ a
-- completed parameter due to the 'Ord' instance of 'Maybe'.
class GEq rep => GOrd (rep :: Type -> Type) where
  gcompare :: rep p -> rep q -> Ordering

instance GOrd inner => GOrd (M1 index meta inner) where
  gcompare (M1 x) (M1 y) = gcompare x y

instance (GOrd left, GOrd right) => GOrd (left :*: right) where
  gcompare (leftX :*: rightX) (leftY :*: rightY)
    = gcompare leftX leftY <> gcompare rightX rightY

instance Ord parameter => GOrd (K1 index (Maybe parameter)) where
  gcompare (K1 x) (K1 y) = compare x y

instance (Generic structure, GOrd (Partial_ structure))
    => Ord (Partial structure) where
  compare (Partial x) (Partial y) = gcompare x y

-- | Partial structures form a semigroup by taking the (right-biased) union of
-- partial data.
class GSemigroup (rep :: Type -> Type) where
  gmappend :: rep p -> rep q -> rep r

instance GSemigroup inner => GSemigroup (M1 index meta inner) where
  gmappend (M1 x) (M1 y) = M1 (gmappend x y)

instance (GSemigroup left, GSemigroup right)
    => GSemigroup (left :*: right) where
  gmappend (leftX :*: rightX) (leftY :*: rightY)
    = gmappend leftX leftY :*: gmappend rightX rightY

instance GSemigroup (K1 index (Maybe anything)) where
  gmappend (K1 x) (K1 y) = K1 (y <|> x)

instance GSemigroup U1 where
  gmappend U1 U1 = U1

instance (Generic structure, GSemigroup (Partial_ structure))
    => Semigroup (Partial structure) where
  Partial x <> Partial y
    = Partial (gmappend x y)

-- | Partial structures form a monoid if the unit element is a partial
-- structure with no completed fields.
class GSemigroup rep => GMonoid (rep :: Type -> Type) where
  gmempty :: rep p

instance (GMonoid inner, GSemigroup (M1 index meta inner))
    => GMonoid (M1 index meta inner) where
  gmempty = M1 gmempty

instance (GMonoid left, GMonoid right, GSemigroup (left :*: right))
    => GMonoid (left :*: right) where
  gmempty = gmempty :*: gmempty

instance GSemigroup (K1 index (Maybe anything))
    => GMonoid (K1 index (Maybe anything)) where
  gmempty = K1 empty

instance GSemigroup U1 => GMonoid U1 where
  gmempty = U1

instance (Generic structure, GMonoid (Partial_ structure))
    => Monoid (Partial structure) where
  mempty = Partial gmempty

-- | If all the components of a structure have an 'Arbitrary' instance, we can
-- trivially create arbitrary partial structures.
class GArbitrary (rep :: Type -> Type) where
  garbitrary :: Gen (rep p)

instance GArbitrary inner => GArbitrary (M1 index meta inner) where
  garbitrary = fmap M1 garbitrary

instance (GArbitrary left, GArbitrary right)
    => GArbitrary (left :*: right) where
  garbitrary = (:*:) <$> garbitrary <*> garbitrary

instance Arbitrary inner => GArbitrary (K1 index inner) where
  garbitrary = fmap K1 arbitrary

instance (Generic structure, GArbitrary (Partial_ structure))
    => Arbitrary (Partial structure) where
  arbitrary = fmap Partial garbitrary

class GCoArbitrary (rep :: Type -> Type) where
  gcoarbitrary :: rep p -> Gen b -> Gen b

instance GCoArbitrary inner => GCoArbitrary (M1 index meta inner) where
  gcoarbitrary (M1 x) = gcoarbitrary x

instance (GCoArbitrary left, GCoArbitrary right)
    => GCoArbitrary (left :*: right) where
  gcoarbitrary (left :*: right) = gcoarbitrary left . gcoarbitrary right

instance CoArbitrary inner => GCoArbitrary (K1 index (Maybe inner)) where
  gcoarbitrary (K1 inner) = coarbitrary inner

instance (Generic structure, GCoArbitrary (Partial_ structure))
    => CoArbitrary (Partial structure) where
  coarbitrary (Partial x) = gcoarbitrary x

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

instance Show inner => GShow named (K1 R (Maybe inner)) where
  gshow (K1 x) = maybe "???" show x

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

instance (Generic structure, GToTuple (Partial_ structure) tuple)
    => Function (Partial structure) where
  function = functionMap (gtoTuple . runPartial) (Partial . gfromTuple)
