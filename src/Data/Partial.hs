{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Partial where

import Control.Applicative ((<|>), empty)
import Control.Lens (Lens')
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen)

-- $setup
-- >>> :set -XDataKinds -XDeriveGeneric -XFlexibleContexts -XTypeApplications
--
-- >>> :{
-- data A
--   = Person
--       { name      :: String
--       , age       :: Int
--       , likesDogs :: Bool
--       }
--   deriving (Eq, Generic, Ord, Show)
-- :}
--
-- >>> :{
-- data B = Triple () String Bool
--   deriving (Eq, Generic, Ord, Show)
-- :}
--
-- >>> import Control.Lens
-- >>> import Data.Function ((&))
-- >>> import Test.QuickCheck.Arbitrary
--
-- >>> :{
-- instance Arbitrary A where
--   arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary
-- :}
--
-- >>> :{
-- instance Arbitrary B where
--   arbitrary = Triple <$> arbitrary <*> arbitrary <*> arbitrary
-- :}


type family Partial_ (structure :: Type) :: Type -> Type where
  Partial_ structure = GPartial_ (Rep structure)

type family GPartial_ (rep :: Type -> Type) :: Type -> Type where
  GPartial_ (M1 index meta inner) = M1 index meta (GPartial_ inner)
  GPartial_ (left :*: right)      = GPartial_ left :*: GPartial_ right
  GPartial_ (K1 index value)      = K1 index (Maybe value)
  GPartial_  U1                   = U1
  GPartial_  V1                   = V1

newtype Partial (structure :: Type)
  = Partial { runPartial :: Partial_ structure Void }

---

-- |
-- >>> mempty @(Partial A) == mempty
-- True
--
-- >>> mempty @(Partial B) == mempty
-- True
--
-- prop> x == (x :: A)
-- prop> x == (x :: B)
--
-- prop> (x == y) == (toPartial x == toPartial (y :: A))
-- prop> (x == y) == (toPartial x == toPartial (y :: B))
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

-- |
-- >>> compare mempty (mempty @(Partial A))
-- EQ
--
-- >>> compare mempty (mempty @(Partial B))
-- EQ
--
-- prop> compare x y == compare (toPartial x) (toPartial (y :: A))
-- prop> compare x y == compare (toPartial x) (toPartial (y :: B))
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

-- |
-- prop> x <> (y <> z) == (x <> y) <> (z :: Partial A)
-- prop> x <> (y <> z) == (x <> y) <> (z :: Partial B)
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

-- |
-- prop> mempty <> x == (x :: Partial A)
-- prop> mempty <> x == (x :: Partial B)
--
-- prop> x <> mempty == (x :: Partial A)
-- prop> x <> mempty == (x :: Partial B)
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

---

type family Field (field :: Symbol) (rep :: Type -> Type) :: Maybe Type where
  Field field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 (Maybe focus)))
    = 'Just focus

  Field _ (S1 _ _)
    = 'Nothing

  Field field (M1 _ _ inner)
    = Field field inner

  Field field (left :*: right)
    = Field field left <|> Field field right

  Field _ _
    = 'Nothing

type family (x :: Maybe k) <|> (y :: Maybe k) :: Maybe k where
  'Just x <|> y = 'Just x
  _       <|> y =  y


-- |
-- prop> ((p :: Partial A) & field @"name" .~ x) ^. field @"name" == x
-- prop> (p & field @"name" .~ (p ^. field @"name")) == (p :: Partial A)
-- prop> ((p & field @"name" .~ x) & field @"name" .~ y) == ((p :: Partial A) & field @"name" .~ y)
class HasField' (field :: Symbol) (structure :: Type) (focus :: Type)
    | field structure -> focus where
  field :: Lens' (Partial structure) (Maybe focus)

class GHasField' (field :: Symbol) (rep :: Type -> Type) (focus :: Type)
    | field rep -> focus where
  gfield :: Lens' (rep p) (Maybe focus)

instance GHasField' field inner focus
    => GHasField' field (M1 index meta inner) focus where
  gfield = go . gfield @field
    where go f = fmap M1 . f . unM1

instance GHasField' field (K1 index (Maybe focus)) focus where
  gfield f = fmap K1 . f . unK1

class GHasFieldProduct
    (goal  ::   Maybe Type)
    (field ::       Symbol)
    (rep   :: Type -> Type)
    (focus ::         Type)
    | field rep -> focus where
  gfieldProduct :: Lens' (rep p) (Maybe focus)

instance (GHasField' field left focus, any ~ focus)
    => GHasFieldProduct ('Just any) field (left :*: right) focus where
  gfieldProduct = go . gfield @field
    where go f (left :*: right) = fmap (:*: right) (f left)

instance (GHasField' field right focus, any ~ focus)
    => GHasFieldProduct 'Nothing field (left :*: right) focus where
  gfieldProduct = go . gfield @field
    where go f (left :*: right) = fmap (left :*:) (f right)

instance GHasFieldProduct (Field field left) field (left :*: right) focus
    => GHasField' field (left :*: right) focus where
  gfield = gfieldProduct @(Field field left) @field

instance (Generic structure, GHasField' field (Partial_ structure) focus)
    => HasField' field structure focus where
  field = go . gfield @field
    where go f (Partial inner) = fmap Partial (f inner)

---

-- |
-- prop> fromPartial (toPartial x) == Just (x :: A)
-- prop> fromPartial (toPartial x) == Just (x :: B)
class HasPartial (structure :: Type) where
  toPartial   ::         structure -> Partial structure
  fromPartial :: Partial structure -> Maybe   structure

class GHasPartial (rep :: Type -> Type) where
  gtoPartial   ::           rep p -> GPartial_ rep p
  gfromPartial :: GPartial_ rep p -> Maybe    (rep p)

instance GHasPartial inner => GHasPartial (M1 index meta inner) where
  gtoPartial   =      M1 .  gtoPartial  . unM1
  gfromPartial = fmap M1 . gfromPartial . unM1

instance (GHasPartial left, GHasPartial right)
    => GHasPartial (left :*: right) where
  gtoPartial   (left :*: right) = (:*:)     (gtoPartial left)     (gtoPartial right)
  gfromPartial (left :*: right) = (:*:) <$> gfromPartial left <*> gfromPartial right

instance GHasPartial (K1 index inner) where
  gtoPartial   =      K1 . Just . unK1  
  gfromPartial = fmap K1 .        unK1

instance (Generic structure, GHasPartial (Rep structure))
    => HasPartial structure where
  toPartial   = Partial .  gtoPartial  . from
  fromPartial = fmap to . gfromPartial . runPartial

---

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

---

-- |
-- >>> defaults (Triple () "Hello" True) mempty
-- Triple () "Hello" True
--
-- >>> :set -XDataKinds
-- >>> import Control.Lens
-- >>> import Data.Function ((&))
-- >>> defaults (Person "Tom" 25 True) (mempty & field @"name" ?~ "Tobi")
-- Person {name = "Tobi", age = 25, likesDogs = True}
class Defaults (structure :: Type) where
  defaults :: structure -> Partial structure -> structure

class GDefaults (rep :: Type -> Type) where
  gdefaults :: rep p -> GPartial_ rep q -> rep r

instance GDefaults inner => GDefaults (M1 index meta inner) where
  gdefaults (M1 x) (M1 y) = M1 (gdefaults x y)

instance (GDefaults left, GDefaults right)
    => GDefaults (left :*: right) where
  gdefaults (leftX :*: rightX) (leftY :*: rightY)
    = gdefaults leftX leftY :*: gdefaults rightX rightY

instance GDefaults (K1 index inner) where
  gdefaults (K1 inner) (K1 partial) = K1 (fromMaybe inner partial)

instance (Generic structure, GDefaults (Rep structure))
    => Defaults structure where
  defaults x (Partial y) = to (gdefaults (from x) y)

-- |
-- prop> show (toPartial x) == show (x :: A)
-- prop> show (toPartial x) == show (x :: B)
-- 
-- >>> mempty @(Partial A)
-- Person {name = ???, age = ???, likesDogs = ???}
--
-- >>> mempty @(Partial B)
-- Triple ??? ??? ???
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
