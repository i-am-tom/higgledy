{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Data.Generic.HKD
Description : A generic-based HKD decorator for ADTs.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental
-}
module Data.Generic.HKD
  ( module Exports

  , Barbie.ConstraintsB (..)
  , Barbie.FunctorB (..)
  , Barbie.ProductBC (..)
  , Barbie.TraversableB (..)

  , position
  , field
  ) where

import Data.Generic.HKD.Build        as Exports
import Data.Generic.HKD.Construction as Exports
import Data.Generic.HKD.Labels       as Exports
import Data.Generic.HKD.Types        as Exports

import qualified Data.Barbie as Barbie

import qualified Data.Generics.Internal.VL.Lens as G
import qualified Data.Generics.Product as G

-- | When we work with records, all the fields are named, and we can refer to
-- them using these names. This class provides a lens from our HKD structure to
-- any @f@-wrapped field.
--
-- >>> :set -XDataKinds -XDeriveGeneric -XTypeApplications
-- >>> import Control.Lens ((&), (.~))
-- >>> import Data.Monoid (Last)
-- >>> import GHC.Generics
--
-- >>> data User = User { name :: String, age :: Int } deriving (Generic, Show)
-- >>> type Partial a = HKD a Last
--
-- We can create an empty partial @User@ and set its name to \"Tom\" (which, in
-- this case, is @pure \"Tom\" :: Last String@):
--
-- >>> mempty @(Partial User) & field @"name" .~ pure "Tom"
-- User {name = Last {getLast = Just "Tom"}, age = Last {getLast = Nothing}}
--
-- Thanks to some @generic-lens@ magic, we also get some pretty magical type
-- errors! If we create a (complete) partial user:
--
-- >>> import Data.Generic.HKD.Construction (deconstruct)
-- >>> total = deconstruct @Last (User "Tom" 25)
--
-- ... and then try to access a field that isn't there, we get a friendly
-- message to point us in the right direction:
--
-- >>> total & field @"oops" .~ pure ()
-- ...
-- ... error:
-- ... The type HKD User Last does not contain a field named 'oops'.
-- ...
field
  :: forall field f structure inner
   . G.HasField' field (HKD structure f) (f inner)
  => G.Lens' (HKD structure f) (f inner)

field
  = G.field' @field

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
-- ... The type HKD
-- ... ([Char], Bool) f does not contain a field at position 4
-- ...
position
  :: forall index f structure inner
   . G.HasPosition' index (HKD structure f) (f inner)
  => G.Lens' (HKD structure f) (f inner)

position
  = G.position' @index
