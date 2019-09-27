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
import Data.GenericLens.Internal (GUpcast (..))
import Data.Kind (Type)
import GHC.Generics
import Named ((:!), NamedF (..))

class Append (xs :: Type -> Type) (ys :: Type -> Type) (zs :: Type -> Type)
  | xs ys -> zs where

instance Append (S1 m h) t (S1 m h :*: t)
instance Append x y z => Append (w :*: x) y (w :*: z)

class Rearrange (i :: Type -> Type) (o :: Type -> Type) | i -> o
instance Rearrange i o => Rearrange (D1 m i) (D1 m o)
instance Rearrange i o => Rearrange (C1 m i) (C1 m o)
instance Rearrange i o => Rearrange (S1 m i) (S1 m o)
instance                  Rearrange (Rec0 x) (Rec0 x)
instance (Rearrange l l', Rearrange r r', Append l' r' o)
    => Rearrange (l :*: r) o where

-- | The 'Data.Generic.HKD.build' function lets us supply arguments to a type
-- one by one, but can cause confusion when working with a record. If the
-- record contains two fields of the same type, for example, we've introduced
-- an opportunity for bugs and confusion. The @record@ function uses the
-- wonderful @named@ package to help us:
--
-- >>> :set -XDeriveGeneric -XTypeApplications
--
-- >>> :{
-- data User
--   = User { name :: String, age :: Int, likesDogs :: Bool }
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
-- ...   -> ("age" :! f Int) -> ("likesDogs" :! f Bool) -> HKD User f...
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

instance (rec ~ (Rec0 inner), k ~ (name :! inner -> HKD structure f))
    => GRecord (S1 ('MetaSel ('Just name) i d c) rec) f structure k where
  grecord fill = \(Arg inner) -> fill (M1 (K1 inner))

instance (GRecord right f structure k, rec ~ Rec0 x)
    => GRecord (S1 ('MetaSel ('Just name) i d c) rec :*: right) f structure (name :! x -> k) where
  grecord fill = \(Arg left) -> grecord \right -> fill (M1 (K1 left) :*: right)

instance
    ( Contravariant (HKD_ f structure)
    , Functor (HKD_ f structure)

    , Rearrange (HKD_ f structure) list
    , GUpcast list (HKD_ f structure)
    , GRecord list f structure k
    )
    => Record structure f k where
  record = grecord @_ @f @structure (to . gupcast @list @(HKD_ f structure))
