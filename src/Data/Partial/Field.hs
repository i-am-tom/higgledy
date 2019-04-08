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

import Control.Lens (Lens', dimap)
import Data.Partial.Types (Partial (..), Partial_)
import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

-- | A la @generic-lens@, we are able to focus on a particular field within our
-- partial structure. We use a lens to a 'Maybe' rather than a prism to allow
-- users to /delete/ fields within a partial structure.
--
-- >>> :set -XDeriveGeneric -XFlexibleContexts
-- >>> import Control.Lens
-- >>> import Data.Partial.Build
--
-- We can focus in on particular fields within our structure by type-applying
-- the name:
--
-- >>> data User = User { id :: Int, name :: String } deriving Generic
-- >>> mempty @(Partial User) ^. field @"id"
-- Nothing
--
-- >>> toPartial (User 1 "Tom") ^. field @"name"
-- Just "Tom"
--
-- >>> toPartial (User 1 "Tom") ^. field @"shoes"
-- ...
-- ... User has no field called "shoes"!
-- ...
class HasField' (field :: Symbol) (structure :: Type) (focus :: Type)
    | field structure -> focus where
  field :: Lens' (Partial structure) (Maybe focus)

-------------------------------------------------------------------------------

type family Field (field :: Symbol) (rep :: Type -> Type) :: Maybe Type where
  Field s (S1 (_ ('Just s) _ _ _) (_ (Maybe x))) = 'Just x
  Field s (M1  _                   _         xs) = Field s xs

  Field s (l :*: r) =  Field s l <|> Field s r
  Field _  _        = 'Nothing

type family (x :: Maybe k) <|> (y :: Maybe k) :: Maybe k where
  'Just x <|> y = 'Just x
  _       <|> y =  y

class GHasField' (field :: Symbol) (rep :: Type -> Type) (focus :: Type)
    | field rep -> focus where
  gfield :: Lens' (rep p) (Maybe focus)

instance GHasField' field inner focus
    => GHasField' field (M1 index meta inner) focus where
  gfield = dimap unM1 (fmap M1) . gfield @field

instance GHasField' field (K1 index (Maybe focus)) focus where
  gfield = dimap unK1 (fmap K1)

instance GHasFieldProduct (Field field left) field (left :*: right) focus
    => GHasField' field (left :*: right) focus where
  gfield = gfieldProduct @(Field field left) @field

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

type HasField_ (field :: Symbol) (structure :: Type)
  = AssertHasField field structure (Field field (Partial_ structure))

type family AssertHasField
    (field     ::     Symbol)
    (structure ::       Type)
    (goal      :: Maybe Type) where
  AssertHasField _     _        ('Just xs) = xs
  AssertHasField field structure 'Nothing  = TypeError
    ( 'ShowType structure
        ':<>: 'Text " has no field called \""
        ':<>: 'Text field
        ':<>: 'Text "\"!"
    )

instance
    ( Generic structure
    , GHasField' field (Partial_ structure) focus
    , HasField_ field structure ~ focus
    ) => HasField' field structure focus where
  field = dimap runPartial (fmap Partial) . gfield @field

