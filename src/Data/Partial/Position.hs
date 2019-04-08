{-# OPTIONS_HADDOCK not-home #-}

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

{-|
Module      : Data.Partial.Position
Description : Manipulate partial structures using position indices.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental
-}
module Data.Partial.Position
  ( HasPosition' (..)
  ) where

import Control.Lens (Lens', dimap)
import Data.Kind (Type)
import Data.Monoid (Last (..))
import Data.Partial.Types (Partial (..), Partial_)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), Nat, TypeError, type (<=?), type (+), type (-))

-- | Taking another cue from @generic-lens@, we can lens into partial product
-- types whose fields /aren't/ named using a positional index.
--
-- >>> import Control.Lens
-- >>> import Data.Partial.Build
--
-- We address the positions using a type application:
--
-- >>> toPartial ("Hello", True) ^. position @1
-- Just "Hello"
--
-- >>> mempty @(Partial (Int, String, Bool)) ^. position @3
-- Nothing
--
-- >>> toPartial ("Hello", True) ^. position @4
-- ...
-- ... ([Char], Bool) has no position #4!
-- ...
class HasPosition' (index :: Nat) (structure :: Type) (focus :: Type)
    | index structure -> focus where
  position :: Lens' (Partial structure) (Maybe focus)

-------------------------------------------------------------------------------

type family Position (index :: Nat) (rep :: Type -> Type) :: Maybe Type where
  Position 1 (K1 _ (Last x)) = 'Just x
  Position _ (K1 _  _      ) = 'Nothing

  Position n (M1 _ _ x)
    = Position n x

  Position n (l :*: r)
    = Position_ (n <=? Count l) n l r

type family Count (rep :: Type -> Type) :: Nat where
  Count (left :*: right) = Count left + Count right
  Count  _               = 1

type family Position_
    (isLeft ::         Bool)
    (count  ::          Nat)
    (left   :: Type -> Type)
    (right  :: Type -> Type) :: Maybe Type
  where
    Position_ 'True count left _
      = Position count left

    Position_ 'False count left right
      = Position (count - Count left) right

class GHasPosition' (index :: Nat) (rep :: Type -> Type) (focus :: Type)
    | index rep -> focus where
  gposition :: Lens' (rep p) (Maybe focus)

instance GHasPosition' count inner focus
    => GHasPosition' count (M1 index meta inner) focus where
  gposition = dimap unM1 (fmap M1) . gposition @count

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

instance any ~ focus => GHasPosition' 1 (K1 index (Last any)) focus where
  gposition = dimap (getLast . unK1) (fmap (K1 . Last))

type HasPosition_ (position :: Nat) (structure :: Type)
  = AssertHasPosition position structure (Position position (Partial_ structure))

type family AssertHasPosition
    (position  ::        Nat)
    (structure ::       Type)
    (goal      :: Maybe Type) where
  AssertHasPosition _        _        ('Just xs) = xs
  AssertHasPosition position structure 'Nothing  = TypeError
    ( 'ShowType structure
        ':<>: 'Text " has no position #"
        ':<>: 'ShowType position
        ':<>: 'Text "!"
    )

instance
    ( Generic structure
    , GHasPosition' index (Partial_ structure) focus
    , HasPosition_ index structure ~ focus
    )
    => HasPosition' index structure focus where
  position = dimap runPartial (fmap Partial) . gposition @index

