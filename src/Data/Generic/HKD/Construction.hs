{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : Data.Generic.HKD.Construction
Description : Convert to and from the generic HKD structure.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental
-}
module Data.Generic.HKD.Construction
  ( Construct (..)
  ) where

import Data.Generic.HKD.Types (HKD (..), GHKD_)
import Data.Kind (Type)
import GHC.Generics

-- | When working with the HKD representation, it is useful to have a way to
-- convert to and from our original type. To do this, we can:
--
-- * @construct@ the original type from our HKD representation, and
--
-- * @deconstruct@ the original type /into/ our HKD representation.
--
-- As an example, we can try (unsuccessfully) to construct an @(Int, Bool)@
-- tuple from an unpopulated partial structure.
-- 
-- >>> :set -XTypeApplications
-- >>> import Data.Monoid (Last)
--
-- >>> construct (mempty @(HKD (Int, Bool) Last))
-- Last {getLast = Nothing}
--
-- We can also /deconstruct/ a tuple into a partial structure:
--
-- >>> deconstruct @[] ("Hello", True)
-- (,) ["Hello"] [True]
--
-- These two methods also satisfy the round-tripping property:
--
-- prop> construct (deconstruct x) == [ x :: (Int, Bool, String) ]
class Construct (f :: Type -> Type) (structure :: Type) where
  construct   :: HKD structure f -> f structure
  deconstruct :: structure -> HKD structure f

class GConstruct (f :: Type -> Type) (rep :: Type -> Type) where
  gconstruct   :: GHKD_ f rep p -> f (rep p)
  gdeconstruct :: rep p -> GHKD_ f rep p

instance (Functor f, GConstruct f inner)
    => GConstruct f (M1 index meta inner) where
  gconstruct   = fmap M1 . gconstruct . unM1
  gdeconstruct = M1 . gdeconstruct @f . unM1

instance (Applicative f, GConstruct f left, GConstruct f right)
    => GConstruct f (left :*: right) where
  gconstruct   (l :*: r) = (:*:) <$> gconstruct      l <*> gconstruct      r
  gdeconstruct (l :*: r) =           gdeconstruct @f l :*: gdeconstruct @f r

instance Applicative f => GConstruct f (K1 index inner) where
  gconstruct (K1 x) = fmap K1 x
  gdeconstruct (K1 x) = K1 (pure x)

instance (Functor f, Generic structure, GConstruct f (Rep structure))
    => Construct f structure where
  construct   = fmap to . gconstruct . runHKD
  deconstruct = HKD . gdeconstruct @f . from
