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
Module      : Data.Generic.HKD.Position
Description : Manipulate HKD structures using positional indices.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental
-}
module Data.Generic.HKD.Position
  ( HasPosition' (..)
  ) where

import Control.Lens (Lens', dimap)
import Data.Kind (Type)
import Data.Generic.HKD.Types (HKD (..), HKD_)
import GHC.TypeLits (Nat)
import qualified Data.GenericLens.Internal as G
import qualified Data.Generics.Internal.VL.Lens as G

-- | Product types /without/ named fields can't be addressed by field name (for
-- very obvious reason), so we instead need to address them with their
-- "position" index. This is a one-indexed type-applied natural:
--
-- >>> import Control.Lens ((^.))
-- >>> :t mempty @(HKD [] (Int, String)) ^. position @1
-- mempty @(HKD [] (Int, String)) ^. position @1 :: [Int]
class HasPosition' (index :: Nat) (f :: Type -> Type) (structure :: Type) (focus :: Type)
    | index f structure -> focus where
  position :: Lens' (HKD f structure) (f focus)

data PositionPredicate :: Nat -> G.TyFun (Type -> Type) (Maybe Type)
type instance G.Eval (PositionPredicate sym) tt = G.HasTotalPositionP sym tt

instance G.GLens' (PositionPredicate index) (HKD_ f structure) (f focus)
    => HasPosition' index f structure focus where
  position = G.ravel (dimap runHKD HKD . G.glens @(PositionPredicate index))
