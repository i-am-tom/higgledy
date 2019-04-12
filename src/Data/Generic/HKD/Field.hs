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
Module      : Data.Generic.HKD.Field
Description : Manipulate HKD structures using field names.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental
-}
module Data.Generic.HKD.Field
  ( HasField' (..)
  ) where

import Data.Coerce (coerce)
import Data.Generic.HKD.Types (HKD (..), HKD_)
import Data.Kind (Constraint, Type)
import Data.Void (Void)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
import qualified Data.GenericLens.Internal as G
import qualified Data.Generics.Internal.VL.Lens as G

-- | When we work with records, all the fields are named, and we can refer to
-- them using these names. This class provides a lens from our HKD structure to
-- any @f@-wrapped field.
--
-- >>> :set -XDataKinds -XDeriveGeneric
-- >>> import Control.Lens ((&), (.~))
-- >>> import Data.Monoid (Last)
-- >>> import GHC.Generics
--
-- >>> data User = User { name :: String, age :: Int } deriving (Generic, Show)
-- >>> type Partial a = HKD a Last
--
-- We can create an empty partial @User@ and set its name to "Tom" (which, in
-- this case, is @pure "Tom" :: Last String@):
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
-- ... • The type User does not contain a field named 'oops'.
-- ...
class HasField'
    (field     ::       Symbol)
    (f         :: Type -> Type)
    (structure ::         Type)
    (focus     ::         Type)
    | field f structure -> focus where
  field :: G.Lens' (HKD structure f) (f focus)

data HasTotalFieldPSym :: Symbol -> (G.TyFun (Type -> Type) (Maybe Type))
type instance G.Eval (HasTotalFieldPSym sym) tt = G.HasTotalFieldP sym tt

instance
    ( ErrorUnless field structure (G.CollectField field (HKD_ f structure))
    , G.GLens' (HasTotalFieldPSym field) (HKD_ f structure) (f focus)
    ) => HasField' field f structure focus where
  field = coerced . G.ravel (G.glens @(HasTotalFieldPSym field))
    where
      coerced :: G.Lens' (HKD structure f) (HKD_ f structure Void)
      coerced f = fmap coerce . f . coerce

-- We'll import this from actual generic-lens as soon as possible:

type family ErrorUnless (field :: Symbol) (s :: Type) (stat :: G.TypeStat) :: Constraint where
  ErrorUnless field s ('G.TypeStat _ _ '[])
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field named '"
        ':<>: 'Text field ':<>: 'Text "'."
        )

  ErrorUnless field s ('G.TypeStat (n ': ns) _ _)
    = TypeError
        (     'Text "Not all constructors of the type "
        ':<>: 'ShowType s
        ':$$: 'Text " contain a field named '"
        ':<>: 'Text field ':<>: 'Text "'."
        ':$$: 'Text "The offending constructors are:"
        ':$$: G.ShowSymbols (n ': ns)
        )

  ErrorUnless _ _ ('G.TypeStat '[] '[] _)
    = ()
