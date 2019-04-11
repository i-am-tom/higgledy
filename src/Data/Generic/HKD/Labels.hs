{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Generic.HKD.Labels where

import Data.Barbie (ProductB (..), TraversableB (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Product (Product (..))
import Data.Generic.HKD.Types (HKD (..), GHKD_)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, TypeError, symbolVal)

-- | For any record type, we can extract the labels generically using the
-- `Const` functor.
--
-- >>> import Data.Generic.HKD
-- >>> import Data.Functor.Identity (Identity (..))
--
-- >>> data User = User { name :: String, age :: Int } deriving Generic
-- >>> label (deconstruct @Identity (User "Tom" 25))
-- User {name = Const "name", age = Const "age"}
class Label (structure :: Type) where
  label :: HKD structure f -> HKD structure (Const String)

class GLabels (rep :: Type -> Type) where
  glabel :: GHKD_ f rep p -> GHKD_ (Const String) rep p

instance GLabels inner => GLabels (D1 meta inner) where
  glabel = M1 . glabel . unM1

instance GLabels inner
    => GLabels (C1 ('MetaCons name fixity 'True) inner) where
  glabel = M1 . glabel . unM1

instance TypeError ('Text "You can't collect labels for a non-record type!")
    => GLabels (C1 ('MetaCons name fixity 'False) inner) where
  glabel = undefined

instance KnownSymbol name
    => GLabels (S1 ('MetaSel ('Just name) i d c) (K1 index inner)) where
  glabel _ = M1 (K1 (Const (symbolVal (Proxy @name))))

instance (GLabels left, GLabels right) => GLabels (left :*: right) where
  glabel (left :*: right) = glabel left :*: glabel right

instance (Generic structure, GLabels (Rep structure)) => Label structure where
  label = HKD . glabel . runHKD

-- | Because all HKD types are valid barbies, and we have the above mechanism
-- for extracting field names, we can ask some pretty interesting questions.
--
-- >>> import Control.Lens
-- >>> import Data.Maybe (isNothing)
-- >>> import Data.Monoid (Last (..))
-- >>> import Data.Generic.HKD
--
-- Let's imagine, for example, that we're half way through filling in a user's
-- details:
--
-- >>> data User = User { name :: String, age :: Int } deriving Generic
-- >>> test = mempty @(HKD User Last) & field @"name" .~ pure "Tom"
--
-- We want to send a JSON response back to the client containing the fields
-- that have yet to be finished. All we need to do is pick the fields where the
-- values are @Last Nothing@:
--
-- >>> labelsWhere (isNothing . getLast) test
-- ["age"]
labelsWhere
  :: forall structure f
   . ( Label structure
     , ProductB (HKD structure)
     , TraversableB (HKD structure)
     )
  => (forall a. f a -> Bool)
  -> HKD structure f
  -> [String]

labelsWhere p xs
  = getConst (btraverse go (label xs `bprod` xs))
  where
    go :: Product (Const String) f a -> (Const [String]) (Maybe a)
    go (Pair (Const key) value) = Const if p value then [key] else []
