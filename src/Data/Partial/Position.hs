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
import Data.Void (Void)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), Nat, TypeError, type (<=?), type (+), type (-))
import qualified Data.GenericLens.Internal as G
import qualified Data.Generics.Internal.VL.Lens as G

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

data PositionPredicate :: Nat -> G.TyFun (Type -> Type) (Maybe Type)
type instance G.Eval (PositionPredicate sym) tt = G.HasTotalPositionP sym tt

instance G.GLens' (PositionPredicate index) (Partial_ structure) (Last focus)
    => HasPosition' index structure focus where
  position
    = G.ravel
    $ dimap runPartial Partial
    . G.glens @(PositionPredicate index)
    . dimap getLast Last
