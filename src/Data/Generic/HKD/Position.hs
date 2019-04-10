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

import Data.Type.Bool (type (&&))
import Control.Lens (Lens', dimap)
import Data.Generic.HKD.Types (HKD (..), HKD_)
import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), Nat, type (<=?), TypeError)
import qualified Data.GenericLens.Internal as G
import qualified Data.Generics.Internal.VL.Lens as G

-- | Product types /without/ named fields can't be addressed by field name (for
-- very obvious reason), so we instead need to address them with their
-- "position" index. This is a one-indexed type-applied natural:
--
-- >>> import Control.Lens ((^.))
--
-- >>> :t mempty @(HKD [] (Int, String)) ^. position @1
-- mempty @(HKD [] (Int, String)) ^. position @1 :: [Int]
--
-- As we're using the wonderful @generic-lens@ library under the hood, we also
-- get some beautiful error messages when things go awry:
--
-- >>> import Data.Generic.HKD.Construction
-- >>> deconstruct ("Hello", True) ^. position @4
-- ...
-- ... error:
-- ... • The type ([Char], Bool) does not contain a field at position 4
-- ...
class HasPosition' (index :: Nat) (f :: Type -> Type) (structure :: Type) (focus :: Type)
    | index f structure -> focus where
  position :: Lens' (HKD f structure) (f focus)

data HasTotalPositionPSym :: Nat -> (G.TyFun (Type -> Type) (Maybe Type))
type instance G.Eval (HasTotalPositionPSym t) tt = G.HasTotalPositionP t tt

instance
  ( ErrorUnless index structure (0 G.<? index && index <=? G.Size (HKD_ f structure))
  , G.GLens' (HasTotalPositionPSym index) (HKD_ f structure) (f focus)
  ) => HasPosition' index f structure focus where
  position = G.ravel (dimap runHKD HKD . G.glens @(HasTotalPositionPSym index))

-- Again: to be imported from generic-lens.

type family ErrorUnless (i :: Nat) (s :: Type) (hasP :: Bool) :: Constraint where
  ErrorUnless i s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field at position "
        ':<>: 'ShowType i
        )

  ErrorUnless _ _ 'True
    = ()
