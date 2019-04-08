{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Partial.Position where

import GHC.Generics
import Control.Lens (Lens')
import Data.Kind (Type)
import Data.Partial.Types (Partial (..), Partial_)
import GHC.TypeLits (Nat, type (<=?), type (+), type (-))

-- | For types /without/ named fields, our lens must be positional. This works
-- just the same as 'field', but we type-apply the index (starting at @1@).
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

