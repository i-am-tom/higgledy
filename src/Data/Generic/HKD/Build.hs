{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Data.Generic.HKD.Build
Description : Construct an HKD structure with its component parameters.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental
-}
module Data.Generic.HKD.Build
  ( Build (..)
  ) where

import Data.Kind (Type)
import Data.GenericLens.Internal (HList (..))
import Data.Generic.HKD.Types (HKD (..), GHKD_)
import GHC.Generics
import Prelude hiding (uncurry)

class Fill (f :: Type -> Type) (structure :: Type) (types :: [Type])
    | structure f -> types, types -> f where
  fill :: HList types -> HKD structure f

class GFill (f :: Type -> Type) (xs :: [Type]) (ys :: [Type]) (rep :: Type -> Type)
    | xs rep -> ys, ys f rep -> xs, xs -> f where
  gfill :: HList xs -> (HList ys, GHKD_ f rep p)

instance GFill f xs ys inner
    => GFill f xs ys (M1 index meta inner) where
  gfill = fmap M1 . gfill @f

instance (GFill f xs ys left, GFill f ys zs right)
    => GFill f xs zs (left :*: right) where
  gfill xs = do
    let (ys,  left) = gfill @f xs
        (zs, right) = gfill @f ys

    (zs, left :*: right)

instance GFill f (f x ': xs) xs (Rec0 x) where
  gfill (x :> xs) = (xs, K1 x)

instance (Generic shape, GFill f with '[] (Rep shape))
    => Fill f shape with where
  fill = HKD . snd . gfill @f @_ @'[]

-- | With many HKD applications, we're working with types like 'Maybe' where it
-- makes sense for us to start with 'mempty' and add values in as we go.
--
-- This isn't always the case, however: if all the component parts of our type
-- are gathered using some 'IO' action, we'd like to construct something like
-- @HKD MyType IO@, and @IO a@ isn't a 'Monoid' for all types @a@. When this
-- happens, we need to pass in our parameters /when/ we build our structure.
--
-- The 'build' function lets us construct our type by passing explicit values
-- for each parameter:
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
-- test = build @User
-- :}
-- ...
-- ... • Found type wildcard ‘_’
-- ...     standing for ‘f [Char] -> f Int -> f Bool -> HKD User f’
-- ...
--
-- Once we call the 'build' function, and indicate the type we want to build,
-- we are free to pick whichever 'f' we like and get to work!
class Build (structure :: Type) (f :: Type -> Type) (k :: Type)
    | f structure -> k where
  build :: k

class GBuild (f :: Type -> Type) (structure :: Type) (xs :: [Type]) (k :: Type)
    | f structure xs -> k where
  gbuild :: (HList xs -> HKD structure f) -> k

instance GBuild f structure xs k
    => GBuild f structure (x ': xs) (x -> k) where
  gbuild k x = gbuild @_ @_ @xs \xs -> k (x :> xs)

instance GBuild f structure '[] (HKD structure f) where
  gbuild k = k Nil

instance (Fill f structure xs, GBuild f structure xs k)
    => Build structure f k where
  build = gbuild @f @structure @xs fill
