{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Partial.Build where

import Control.Lens (Prism', prism')
import Data.Kind (Type)
import Data.Partial.Types (Partial (..), GPartial_)
import GHC.Generics

-- | We can construct partial equivalents of complete structures, and attempt
-- to build complete structures from partial representations.
impartial
  :: ( Generic structure
     , HasPartial structure
     )
  => Prism' (Partial structure) structure

impartial = prism' toPartial fromPartial

class HasPartial (structure :: Type) where
  toPartial   ::         structure -> Partial structure
  fromPartial :: Partial structure -> Maybe   structure

class GHasPartial (rep :: Type -> Type) where
  gtoPartial   ::           rep p -> GPartial_ rep p
  gfromPartial :: GPartial_ rep p -> Maybe    (rep p)

instance GHasPartial inner => GHasPartial (M1 index meta inner) where
  gtoPartial   =      M1 .  gtoPartial  . unM1
  gfromPartial = fmap M1 . gfromPartial . unM1

instance (GHasPartial left, GHasPartial right)
    => GHasPartial (left :*: right) where
  gtoPartial   (left :*: right) = (:*:)     (gtoPartial left)     (gtoPartial right)
  gfromPartial (left :*: right) = (:*:) <$> gfromPartial left <*> gfromPartial right

instance GHasPartial (K1 index inner) where
  gtoPartial   =      K1 . Just . unK1
  gfromPartial = fmap K1 .        unK1

instance (Generic structure, GHasPartial (Rep structure))
    => HasPartial structure where
  toPartial   = Partial .  gtoPartial  . from
  fromPartial = fmap to . gfromPartial . runPartial
