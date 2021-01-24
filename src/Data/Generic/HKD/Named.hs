{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Data.Generic.HKD.Named
Description : Construct an HKD record with named parameters.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental
-}
module Data.Generic.HKD.Named
  ( Record (..)
  ) where

import Data.Functor.Contravariant (Contravariant (..))
import Data.Generic.HKD.Types (HKD, HKD_)
import Data.Generics.Product.Internal.Subtype (GUpcast (..))
import Data.Kind (Type)
import GHC.Generics
import Named ((:!), NamedF (..))

type family Append (xs :: Type -> Type) (ys :: Type -> Type) :: Type -> Type where
  Append (S1 meta head) tail    = S1 meta head :*: tail
  Append (left :*: right) other = left :*: Append right other

type family Rearrange (i :: Type -> Type) :: Type -> Type where
  Rearrange (S1       m inner) = S1       m (Rearrange inner)
  Rearrange (M1 index m inner) = M1 index m (Rearrange inner)
  Rearrange (left :*: right)   = Append (Rearrange left) (Rearrange right)
  Rearrange (Rec0 inner)       = Rec0 inner

-- | The 'Data.Generic.HKD.record' function lets us supply arguments to a type
-- one by one, but can cause confusion when working with a record. If the
-- record contains two fields of the same type, for example, we've introduced
-- an opportunity for bugs and confusion. The @record@ function uses the
-- wonderful @named@ package to help us:
--
-- >>> :set -XDeriveGeneric -XTypeApplications
--
-- >>> :{
-- data User
--   = User { name :: String, enemy :: String }
--   deriving Generic
-- :}
--
-- >>> :{
-- test :: _
-- test = record @User
-- :}
-- ...
-- ... Found type wildcard ...
-- ... standing for ...("name" :! f [Char])
-- ...   -> ("enemy" :! f [Char]) -> HKD User f...
-- ...
class Record (structure :: Type) (f :: Type -> Type) (k :: Type)
    | f structure -> k where
  record :: k

class GRecord (rep :: Type -> Type) (f :: Type -> Type) (structure :: Type) (k :: Type)
    | f structure rep -> k where
  grecord :: (forall p. rep p -> HKD structure f) -> k

instance GRecord inner f structure k
    => GRecord (D1 meta inner) f structure k where
  grecord rebuild = grecord (rebuild . M1)

instance GRecord inner f structure k
    => GRecord (C1 meta inner) f structure k where
  grecord rebuild = grecord (rebuild . M1)

instance
    ( rec ~ (Rec0 inner)
    , k ~ (name :! inner -> HKD structure f)
    , meta ~ 'MetaSel ('Just name) i d c
    )
    => GRecord (S1 meta rec) f structure k where
  grecord fill = \(Arg inner) -> fill (M1 (K1 inner))

instance
    ( GRecord right f structure k'
    , rec ~ Rec0 x
    , left ~ S1 ('MetaSel ('Just name) i d c) rec
    , k ~ (name :! x -> k')
    )
    => GRecord (left :*: right) f structure k where
  grecord fill = \(Arg left) -> grecord \right -> fill (M1 (K1 left) :*: right)

instance
    ( Contravariant (HKD_ f structure)
    , Functor (HKD_ f structure)

    , list ~ Rearrange (HKD_ f structure)
    , GUpcast list (HKD_ f structure)
    , GRecord list f structure k
    )
    => Record structure f k where
  record = grecord @_ @f @structure (to . gupcast @list @(HKD_ f structure))
