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
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Generic.HKD.Labels
  ( Label (..)
  ) where

import Data.Barbie (ProductB (..), TraversableB (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Product (Product (..))
import Data.Generic.HKD.Types (HKD (..), GHKD_, Nested (..))
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
-- >>> label @User
-- User {name = Const "name", age = Const "age"}
class Label (structure :: Type) where
  label :: HKD structure (Const String)

class GLabels (rep :: Type -> Type) where
  glabel :: GHKD_ (Const String) rep p

instance GLabels inner => GLabels (D1 meta inner) where
  glabel = M1 glabel

instance GLabels inner
    => GLabels (C1 ('MetaCons name fixity 'True) inner) where
  glabel = M1 glabel

instance TypeError ('Text "You can't collect labels for a non-record type!")
    => GLabels (C1 ('MetaCons name fixity 'False) inner) where
  glabel = undefined

instance (KnownSymbol name, Label inner)
    => GLabels (S1 ('MetaSel ('Just name) i d c) (K1 index (Nested inner))) where
  glabel = M1 (K1 label)

instance {-# OVERLAPPABLE #-}
    ( KnownSymbol name
    , GHKD_ (Const String) (K1 index inner) ~ K1 index (Const String inner)
    )
    => GLabels (S1 ('MetaSel ('Just name) i d c) (K1 index inner)) where
  glabel = M1 (K1 (Const (symbolVal (Proxy @name))))

instance (GLabels left, GLabels right) => GLabels (left :*: right) where
  glabel = glabel :*: glabel

instance (Generic structure, GLabels (Rep structure)) => Label structure where
  label = HKD glabel
