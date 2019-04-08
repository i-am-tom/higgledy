{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
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
import GHC.TypeLits (type (<=?), type (+), type (-), KnownSymbol, Nat, Symbol, symbolVal)
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..))
import Test.QuickCheck.Function ((:->), Function (..), functionMap)
import Test.QuickCheck.Gen (Gen)

-- | Nothing too exciting; this synonym just makes our signatures look a little
-- less intimidating to users. The inside of a 'Partial' type is effectively a
-- value of this type constructor.
type Partial_ (structure :: Type)
  = GPartial_ (Rep structure)

-- | Given an /actual/ generic rep, create a "partial" rep: effectively, all
-- the 'K1'-wrapped values are further wrapped in 'Maybe'. Otherwise, we leave
-- the rep undisturbed.
type family GPartial_ (rep :: Type -> Type) :: Type -> Type where
  GPartial_ (M1 index meta inner) = M1 index meta (GPartial_ inner)
  GPartial_ (left :*: right)      = GPartial_ left :*: GPartial_ right
  GPartial_ (K1 index value)      = K1 index (Maybe value)
  GPartial_  U1                   = U1
  GPartial_  V1                   = V1

-- | A @Partial X@ is a representation of @X@ in which its parameters needn't
-- all be present. This can be inspected and manipulated in a partial state,
-- and we can try to reconstruct our @X@ from the partial data that we have.
newtype Partial (structure :: Type)
  = Partial
      { runPartial :: Partial_ structure Void
      }

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

-- | For types with named fields, we can use a 'Control.Lens.Lens' to access
-- the partial fields. Note that we use a 'Control.Lens.Lens' to the 'Maybe'
-- rather than a 'Control.Lens.Prism' because we want the option to "remove"
-- partial data by setting a field to 'Nothing'.
--
-- >>> :set -XDataKinds -XTypeApplications
--
-- -- prop> ((p :: Partial A) & field @"name" .~ x) ^. field @"name" == x
-- -- prop> (p & field @"name" .~ (p ^. field @"name")) == (p :: Partial A)
-- -- prop> ((p & field @"name" .~ x) & field @"name" .~ y) == ((p :: Partial A) & field @"name" .~ y)
class HasField' (field :: Symbol) (structure :: Type) (focus :: Type)
    | field structure -> focus where
  field :: Lens' (Partial structure) (Maybe focus)

type family Field (field :: Symbol) (rep :: Type -> Type) :: Maybe Type where
  Field name (S1   (_ ('Just name) _ _ _) (Rec0 (Maybe focus))) = 'Just focus
  Field _    (S1    _                      _                  ) = 'Nothing
  Field name (M1 _  _                      xs                 ) = Field name xs

  Field field (left :*: right) = Field field left <|> Field field right
  Field _      _               = 'Nothing

type family (x :: Maybe k) <|> (y :: Maybe k) :: Maybe k where
  'Just x <|> y = 'Just x
  _       <|> y =  y

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

instance (GHasField' field right focus)
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

-- | For types /without/ named fields, our lens must be positional. This works
-- just the same as 'field', but we type-apply the index (starting at @1@).
--
-- >>> :set -XDataKinds -XTypeApplications
--
-- -- prop> ((p :: Partial B) & position @2 .~ x) ^. position @2 == x
-- -- prop> (p & position @2 .~ (p ^. position @2)) == (p :: Partial B)
-- -- prop> ((p & position @2 .~ x) & position @2 .~ y) == ((p :: Partial B) & position @2 .~ y)
class HasPosition' (index :: Nat) (structure :: Type) (focus :: Type)
    | index structure -> focus where
  position :: Lens' (Partial structure) (Maybe focus)

type family Position (index :: Nat) (rep :: Type -> Type) :: Maybe Type where
  Position 1 (K1 _ (Maybe inner)) = 'Just inner
  Position _ (K1 _  _           ) = 'Nothing

  Position count (M1 index meta inner)
    = Position count inner

  Position count (left :*: right)
    = Position_ (Count left <=? count) count left right

type family Count (rep :: Type -> Type) :: Nat where
  Count (left :*: right) = Count left + Count right
  Count  _               = 1

type family Position_ (isLeft :: Bool) (count :: Nat) (left :: Type -> Type) (right :: Type -> Type) :: Maybe Type where
  Position_ 'True count left _ = Position count left
  Position_ 'False count _ right = Position count right

class GHasPosition' (index :: Nat) (rep :: Type -> Type) (focus :: Type)
    | index rep -> focus where
  gposition :: Lens' (rep p) (Maybe focus)

instance GHasPosition' count inner focus
    => GHasPosition' count (M1 index meta inner) focus where
  gposition = go . gposition @count
    where go f = fmap M1 . f . unM1

class GHasPositionProduct
    (goal  ::   Maybe Type)
    (count ::          Nat)
    (rep   :: Type -> Type)
    (focus ::         Type)
    | count rep -> focus where
  gpositionProduct :: Lens' (rep p) (Maybe focus)

instance GHasPositionProduct (Position count left) count (left :*: right) focus
    => GHasPosition' count (left :*: right) focus where
  gposition = gpositionProduct @(Position count left) @count

instance (GHasPosition' count left focus, any ~ focus)
    => GHasPositionProduct ('Just any) count (left :*: right) focus where
  gpositionProduct = go . gposition @count
    where go f (left :*: right) = fmap (:*: right) (f left)

instance (GHasPosition' (count - Count left) right focus)
    => GHasPositionProduct 'Nothing count (left :*: right) focus where
  gpositionProduct = go . gposition @(count - Count left)
    where go f (left :*: right) = fmap (left :*:) (f right)

instance any ~ focus => GHasPosition' 1 (K1 index (Maybe any)) focus where
  gposition f = fmap K1 . f . unK1

instance (Generic structure, GHasPosition' index (Partial_ structure) focus)
    => HasPosition' index structure focus where
  position = go . gposition @index
    where go f (Partial inner) = fmap Partial (f inner)

-- | We can construct partial equivalents of complete structures, and attempt
-- to build complete structures from partial representations.
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

-- | We can build structures from partial representations without a 'Maybe' if
-- we can provide a structure full of defaults. When a field is missing, we
-- just use the field in the default structure.
--
-- -- prop> defaults x mempty == (x :: A)
-- -- prop> defaults x mempty == (x :: B)
--
-- -- prop> defaults (x :: A) (mempty & field @"name" ?~ text) == x { name = text }
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

-- | For complete partial structures, the 'Show' instances should match (though
-- there are some edge-cases around, say, rendering of negative numbers).
--
-- -- prop> show (toPartial x) == show (x :: A)
-- -- prop> show (toPartial x) == show (x :: B)
-- 
-- -- >>> mempty @(Partial A)
-- -- Person {name = ???, age = ???, likesDogs = ???}
--
-- -- >>> mempty @(Partial B)
-- -- Triple ??? ??? ???
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
