{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Partial.Default where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Partial.Types (Partial (..), GPartial_)
import GHC.Generics

-- | We can build structures from partial representations without a 'Maybe' if
-- we can provide a structure full of defaults. When a field is missing, we
-- just use the field in the default structure.
class Defaults (structure :: Type) where
  defaults :: structure -> Partial structure -> structure

class GDefaults (rep :: Type -> Type) where
  gdefaults :: rep p -> GPartial_ rep q -> rep r

instance GDefaults inner => GDefaults (M1 index meta inner) where
  gdefaults (M1 x) (M1 y) = M1 (gdefaults x y)

instance (GDefaults left, GDefaults right)
    => GDefaults (left :*: right) where
  gdefaults (leftX :*: rightX) (leftY :*: rightY)
    = gdefaults leftX leftY :*: gdefaults rightX rightY

instance GDefaults (K1 index inner) where
  gdefaults (K1 inner) (K1 partial) = K1 (fromMaybe inner partial)

instance (Generic structure, GDefaults (Rep structure))
    => Defaults structure where
  defaults x (Partial y) = to (gdefaults (from x) y)
