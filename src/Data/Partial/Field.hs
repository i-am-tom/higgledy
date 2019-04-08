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
module Data.Partial.Field where

import Control.Lens (Lens')
import Data.Partial.Types (Partial (..), Partial_)
import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits (Symbol)

-- | For types with named fields, we can use a 'Control.Lens.Lens' to access
-- the partial fields. Note that we use a 'Control.Lens.Lens' to the 'Maybe'
-- rather than a 'Control.Lens.Prism' because we want the option to "remove"
-- partial data by setting a field to 'Nothing'.
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

