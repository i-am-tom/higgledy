{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
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

import Data.Coerce (Coercible, coerce)
import Data.Type.Bool (type (&&))
import Data.Void (Void)
import GHC.Generics
import Data.Generic.HKD.Types (HKD (..), HKD_)
import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), Nat, type (+), type (<=?), TypeError)
import qualified Data.GenericLens.Internal as G
import Data.GenericLens.Internal (type (<?))
import qualified Data.Generics.Internal.VL.Lens as G
import Data.Generics.Internal.Profunctor.Lens (ALens)

-- | Product types /without/ named fields can't be addressed by field name (for
-- very obvious reason), so we instead need to address them with their
-- "position" index. This is a one-indexed type-applied natural:
--
-- >>> import Control.Lens ((^.))
--
-- >>> :t mempty @(HKD (Int, String) []) ^. position @1
-- mempty @(HKD (Int, String) []) ^. position @1 :: [Int]
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
  position :: G.Lens' (HKD structure f) (f focus)

data HasTotalPositionPSym :: Nat -> (G.TyFun (Type -> Type) (Maybe Type))
type instance G.Eval (HasTotalPositionPSym t) tt = G.HasTotalPositionP t tt

instance
    ( Generic structure
    , ErrorUnless index structure (0 <? index && index <=? G.Size (Rep structure))
    , G.GLens' (HasTotalPositionPSym index) (CRep f structure) (f focus)

    , G.HasTotalPositionP index (CRep f structure) ~ 'Just (f focus)
    , G.HasTotalPositionP index (CRep f (G.Indexed structure)) ~ 'Just (f' focus')

    , Coercible (HKD structure f) (CRep f structure Void)
    , structure ~ G.Infer structure (f' focus') (f focus)
    ) => HasPosition' index f structure focus where
  position = coerced . glens
    where
      glens :: G.Lens' (CRep f structure Void) (f focus)
      glens = G.ravel (G.glens @(HasTotalPositionPSym index))

      coerced :: G.Lens' (HKD structure f) (CRep f structure Void)
      coerced f = fmap coerce . f . coerce

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

type CRep (f :: Type -> Type) (structure :: Type)
  = Fst (Traverse (HKD_ f structure) 1)

type family Fst (p :: (a, b)) :: a where
  Fst '(a, b) = a

type family Traverse (a :: Type -> Type) (n :: Nat) :: (Type -> Type, Nat) where
  Traverse (M1 mt m s) n
    = Traverse1 (M1 mt m) (Traverse s n)
  Traverse (l :+: r) n
    = '(Fst (Traverse l n) :+: Fst (Traverse r n), n)
  Traverse (l :*: r) n
    = TraverseProd (:*:) (Traverse l n) r
  Traverse (K1 _ p) n
    = '(K1 (G.Pos n) p, n + 1)
  Traverse U1 n
    = '(U1, n)

type family Traverse1 (w :: (Type -> Type) -> (Type -> Type)) (z :: (Type -> Type, Nat)) :: (Type -> Type, Nat) where
  Traverse1 w '(i, n) = '(w i, n)

type family TraverseProd (c :: (Type -> Type) -> (Type -> Type) -> (Type -> Type)) (a :: (Type -> Type, Nat)) (r :: Type -> Type) :: (Type -> Type, Nat) where
  TraverseProd w '(i, n) r = Traverse1 (w i) (Traverse r n)
