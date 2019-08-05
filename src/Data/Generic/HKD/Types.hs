{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Data.Generic.HKD.Types
Description : Type declarations for the HKD structure.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental
-}
module Data.Generic.HKD.Types
  ( HKD (..)

  , HKD_
  , GHKD_

  , Tuple (..)
  ) where

import Data.Barbie (ConstraintsB (..), FunctorB (..), ProductB (..), ProductBC (..), TraversableB (..))
import Data.Barbie.Constraints (Dict (..))
import Data.Function (on)
import Data.Functor.Contravariant (Contravariant (..), phantom)
import Data.Functor.Product (Product (..))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Generics.Deriving.Show (GShow' (..), gshowsPrecdefault)
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..))
import Test.QuickCheck.Function (Function (..), functionMap)

-- | Higher-kinded data (HKD) is the design pattern in which every field in our
-- type is wrapped in some functor @f@:
--
-- @
--   data User f
--    = User
--        { name :: f String
--        , age  :: f Int
--        }
-- @
--
-- Depending on the functor, we can get different behaviours: with 'Maybe', we
-- get a partial structure; with 'Validation', we get a piecemeal validator;
-- and so on. The @HKD@ newtype allows us to lift any type into an HKD-style
-- API via its generic representation.
--
-- >>> :set -XDeriveGeneric -XTypeApplications
-- >>> :{
-- data User
--   = User { name :: String, age :: Int }
--   deriving Generic
-- :}
--
-- The @HKD@ type is indexed by our choice of functor and the structure we're
-- lifting. In other words, we can define a synonym for our behaviour:
--
-- >>> import Data.Monoid (Last (..))
-- >>> type Partial a = HKD a Last
--
-- ... and then we're ready to go!
--
-- >>> mempty @(Partial User)
-- User {name = Last {getLast = Nothing}, age = Last {getLast = Nothing}}
--
-- >>> mempty @(HKD (Int, Bool) [])
-- (,) ([],[])
newtype HKD (structure :: Type) (f :: Type -> Type)
  = HKD { runHKD :: HKD_ f structure Void }

instance (Contravariant (HKD_ f structure), Functor (HKD_ f structure))
    => Generic (HKD structure f) where
  type Rep (HKD structure f) = HKD_ f structure

  from = phantom . runHKD
  to   = HKD . phantom

-------------------------------------------------------------------------------

-- | Calculate the "partial representation" of a type.
type HKD_ (f :: Type -> Type) (structure :: Type)
  = GHKD_ f (Rep structure)

-- | Calculate the "partial representation" of a generic rep.
type family GHKD_ (f :: Type -> Type) (rep :: Type -> Type)
    = (output :: Type -> Type) | output -> f rep where
  GHKD_ f (M1 index meta inner) = M1 index meta (GHKD_ f inner)
  GHKD_ f (left :*: right)      = GHKD_ f left :*: GHKD_ f right
  GHKD_ f (K1 index value)      = K1 index (f value)
  GHKD_ f (left :+: right)      = GHKD_ f left :+: GHKD_ f right

-------------------------------------------------------------------------------

instance (Eq tuple, Generic xs, Tuple f xs tuple)
    => Eq (HKD xs f) where
  (==) = (==) `on` toTuple

instance (Ord tuple, Generic xs, Tuple f xs tuple)
    => Ord (HKD xs f) where
  compare = compare `on` toTuple

instance (Semigroup tuple, Generic xs, Tuple f xs tuple)
    => Semigroup (HKD xs f) where
  x <> y = fromTuple (toTuple x <> toTuple y)

instance (Monoid tuple, Generic xs, Tuple f xs tuple)
    => Monoid (HKD xs f) where
  mempty = fromTuple mempty

-------------------------------------------------------------------------------

instance (Arbitrary tuple, GToTuple (HKD_ f structure) tuple)
    => Arbitrary (HKD structure f) where
  arbitrary = fmap (HKD . gfromTuple) arbitrary

instance (CoArbitrary tuple, GToTuple (HKD_ f structure) tuple)
    => CoArbitrary (HKD structure f) where
  coarbitrary (HKD x) = coarbitrary (gtoTuple x)

instance (Generic structure, Function tuple, Tuple f structure tuple)
    => Function (HKD structure f) where
  function = functionMap toTuple fromTuple

-------------------------------------------------------------------------------

instance (Generic (HKD structure f), GShow' (GHKD_ f (Rep structure)))
    => Show (HKD structure f) where
  showsPrec = gshowsPrecdefault

-------------------------------------------------------------------------------

class Tuple (f :: Type -> Type) (structure :: Type) (tuple :: Type)
    | f structure -> tuple where
  toTuple   :: HKD structure f -> tuple
  fromTuple :: tuple -> HKD structure f

class GToTuple (rep :: Type -> Type) (tuple :: Type)
    | rep -> tuple where
  gfromTuple :: tuple -> rep p
  gtoTuple   :: rep p -> tuple

instance GToTuple inner tuple
    => GToTuple (M1 index meta inner) tuple where
  gfromTuple = M1 . gfromTuple
  gtoTuple   = gtoTuple . unM1

instance (GToTuple left left', GToTuple right right')
    => GToTuple (left :*: right) (left', right') where
  gfromTuple (x, y) = gfromTuple x :*: gfromTuple y
  gtoTuple (x :*: y) = (gtoTuple x, gtoTuple y)

instance GToTuple (K1 index inner) inner where
  gfromTuple = K1
  gtoTuple = unK1

instance (Generic structure, GToTuple (HKD_ f structure) tuple)
    => Tuple f structure tuple where
  toTuple = gtoTuple . runHKD
  fromTuple = HKD . gfromTuple

-------------------------------------------------------------------------------

class GFunctorB (rep :: Type -> Type) where
  gbmap :: (forall a. f a -> g a) -> GHKD_ f rep p -> GHKD_ g rep p

instance GFunctorB inner => GFunctorB (M1 index meta inner) where
  gbmap f = M1 . gbmap @inner f . unM1

instance (GFunctorB left, GFunctorB right)
    => GFunctorB (left :*: right) where
  gbmap f (left :*: right) = gbmap @left f left :*: gbmap @right f right

instance GFunctorB (K1 index inner) where
  gbmap f (K1 x) = K1 (f x)

instance GFunctorB (Rep structure) => FunctorB (HKD structure) where
  bmap f = HKD . gbmap @(Rep structure) f . runHKD

-------------------------------------------------------------------------------

class GTraversableB (rep :: Type -> Type) where
  gbtraverse
    :: Applicative t
    => (forall a. f a -> t (g a))
    -> GHKD_ f rep p -> t (GHKD_ g rep p)

instance GTraversableB inner => GTraversableB (M1 index meta inner) where
  gbtraverse f = fmap M1 . gbtraverse @inner f . unM1

instance (GTraversableB left, GTraversableB right)
    => GTraversableB (left :*: right) where
  gbtraverse f (left :*: right)
    = (:*:) <$> gbtraverse @left  f left
            <*> gbtraverse @right f right

instance GTraversableB (K1 index inner) where
  gbtraverse f (K1 x) = fmap K1 (f x)

instance (FunctorB (HKD structure), GTraversableB (Rep structure))
    => TraversableB (HKD structure) where
  btraverse f = fmap HKD . gbtraverse @(Rep structure) f . runHKD

-------------------------------------------------------------------------------

class GProductB (rep :: Type -> Type) where
  gbprod :: GHKD_ f rep p -> GHKD_ g rep p -> GHKD_ (f `Product` g) rep p
  gbuniq :: (forall a. f a) -> GHKD_ f rep p

instance GProductB inner => GProductB (M1 index meta inner) where
  gbprod (M1 x) (M1 y) = M1 (gbprod @inner x y)
  gbuniq zero = M1 (gbuniq @inner zero)

instance (GProductB left, GProductB right)
    => GProductB (left :*: right) where
  gbprod (leftX :*: rightX) (leftY :*: rightY)
    = gbprod @left leftX leftY :*: gbprod @right rightX rightY

  gbuniq zero
    = gbuniq @left zero :*: gbuniq @right zero

instance GProductB (K1 index inner) where
  gbprod (K1 x) (K1 y) = K1 (Pair x y)
  gbuniq zero = K1 zero

instance (FunctorB (HKD structure), GProductB (Rep structure))
    => ProductB (HKD structure) where
  bprod (HKD x) (HKD y) = HKD (gbprod @(Rep structure) x y)
  buniq zero            = HKD (gbuniq @(Rep structure) zero)

-------------------------------------------------------------------------------

class GAllBC (rep :: Type -> Type) where
  type GAllB (c :: Type -> Constraint) rep :: Constraint

class GConstraintsB (rep :: Type -> Type) where
  gbaddDicts :: GAllB c rep => GHKD_ f rep p -> GHKD_ (Dict c `Product` f) rep p

instance GAllBC inner => GAllBC (M1 index meta inner) where
  type GAllB c (M1 index meta inner) = GAllB c inner

instance GConstraintsB inner => GConstraintsB (M1 index meta inner) where
  gbaddDicts (M1 x) = M1 (gbaddDicts @inner x)

instance (GAllBC left, GAllBC right) => GAllBC (left :*: right) where
  type GAllB c (left :*: right) = (GAllB c left, GAllB c right)

instance (GConstraintsB left, GConstraintsB right)
    => GConstraintsB (left :*: right) where
  gbaddDicts (left :*: right)
    = gbaddDicts @left left :*: gbaddDicts @right right

instance GAllBC (K1 index inner) where
  type GAllB c (K1 index inner) = c inner

instance GConstraintsB (K1 index inner) where
  gbaddDicts (K1 x) = K1 (Pair Dict x)

instance
    ( FunctorB (HKD structure)
    , GConstraintsB (Rep structure)
    , GAllBC (Rep structure)
    )
    => ConstraintsB (HKD structure) where
  type AllB c (HKD structure) = GAllB c (Rep structure)

  baddDicts
    :: forall c f
     . AllB c (HKD structure)
    => HKD structure f
    -> HKD structure (Dict c `Product` f)
  baddDicts (HKD x)
    = HKD (gbaddDicts @(Rep structure) x)

-------------------------------------------------------------------------------

class GProductBC (rep :: Type -> Type) where
  gbdicts :: GAllB c rep => GHKD_ (Dict c) rep p

instance GProductBC inner => GProductBC (M1 index meta inner) where
  gbdicts = M1 gbdicts

instance (GProductBC left, GProductBC right)
    => GProductBC (left :*: right) where
  gbdicts = gbdicts :*: gbdicts

instance GProductBC (K1 index inner) where
  gbdicts = K1 Dict

instance
    ( ProductB (HKD structure)
    , ConstraintsB (HKD structure)
    , GProductBC (Rep structure)
    ) => ProductBC (HKD structure) where
  bdicts = HKD gbdicts
