{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Partial.Build
  ( impartial
  , HasPartial (..)
  ) where

import Control.Applicative (liftA2)
import Control.Lens (Prism', prism')
import Data.Kind (Type)
import Data.Monoid (Last (..))
import Data.Partial.Types (Partial (..), GPartial_)
import GHC.Generics

-- | We can construct a prism from a partial structure to its complete
-- structure, which will "succeed" if all values in the structure have been
-- populated.
--
-- >>> :set -XTypeApplications
-- >>> import Control.Lens
--
-- >>> mempty ^? impartial @(Int, String)
-- Nothing
--
-- >>> toPartial ("Hello", True) ^? impartial
-- Just ("Hello",True)
impartial :: (Generic a, HasPartial a) => Prism' (Partial a) a
impartial = prism' toPartial fromPartial

-------------------------------------------------------------------------------

-- | As this is implemented with generics, an instance is implied for any type
-- that implements 'Generic' sensibly.
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
  gtoPartial   (l :*: r) =        (:*:) (  gtoPartial l) (  gtoPartial r)
  gfromPartial (l :*: r) = liftA2 (:*:) (gfromPartial l) (gfromPartial r)

instance GHasPartial (K1 index inner) where
  gtoPartial   =      K1 . pure    . unK1
  gfromPartial = fmap K1 . getLast . unK1

instance (Generic structure, GHasPartial (Rep structure))
    => HasPartial structure where
  toPartial   = Partial .  gtoPartial  . from
  fromPartial = fmap to . gfromPartial . runPartial
