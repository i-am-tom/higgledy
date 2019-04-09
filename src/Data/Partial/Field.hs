{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Data.Partial.Field
Description : Manipulate partial structures using field names.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental
-}
module Data.Partial.Field
  ( HasField' (..)
  ) where

import Control.Lens (Lens', dimap)
import Data.Kind (Type)
import Data.Monoid (Last (..))
import Data.Void (Void)
import Data.Partial.Types (Partial (..), Partial_)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
import qualified Data.Generics.Internal.VL.Lens as G
import qualified Data.GenericLens.Internal as G

-- | A la @generic-lens@, we are able to focus on a particular field within our
-- partial structure. We use a lens to a 'Maybe' rather than a prism to allow
-- users to /delete/ fields within a partial structure.
--
-- >>> :set -XDeriveGeneric -XFlexibleContexts
-- >>> import Control.Lens
-- >>> import Data.Partial.Build
--
-- We can focus in on particular fields within our structure by type-applying
-- the name:
--
-- >>> data User = User { id :: Int, name :: String } deriving Generic
-- >>> mempty @(Partial User) ^. field @"id"
-- Nothing
--
-- >>> toPartial (User 1 "Tom") ^. field @"name"
-- Just "Tom"
--
-- >>> toPartial (User 1 "Tom") ^. field @"shoes"
-- ...
-- ... User has no field called "shoes"!
-- ...
class HasField' (field :: Symbol) (structure :: Type) (focus :: Type)
    | field structure -> focus where
  field :: Lens' (Partial structure) (Maybe focus)

data FieldPredicate :: Symbol -> G.TyFun (Type -> Type) (Maybe Type)
type instance G.Eval (FieldPredicate sym) tt = G.HasTotalFieldP sym tt

instance G.GLens' (FieldPredicate field) (Partial_ structure) (Last focus)
    => HasField' field structure focus where
  field
    = G.ravel
    $ dimap runPartial Partial
    . G.glens @(FieldPredicate field)
    . dimap getLast Last
