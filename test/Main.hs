{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
module Main where

import Control.Lens (Lens', (.~), (^.))
import Barbies.Constraints (Dict)
import Data.Function ((&), on)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.Generic.HKD
import Data.Monoid (Last (..))
import GHC.Generics
import Test.Hspec
import Test.QuickCheck

type Partial a = HKD a Last
type WTF     a = HKD a []

main :: IO ()
main = hspec do
  describe "Unnamed" do
    eq         @(Partial Triple)
    ord        @(Partial Triple)
    semigroup  @(Partial Triple)
    idempotent @(Partial Triple)
    monoid     @(Partial Triple)

    eq        @(WTF Triple)
    ord       @(WTF Triple)
    semigroup @(WTF Triple)
    monoid    @(WTF Triple)

    lens @(Partial Triple) (position @1)
    lens @(Partial Triple) (position @2)
    lens @(Partial Triple) (position @3)

    lens @(WTF Triple) (position @1)
    lens @(WTF Triple) (position @2)
    lens @(WTF Triple) (position @3)

  describe "Named" do
    eq         @(Partial Person)
    ord        @(Partial Person)
    semigroup  @(Partial Person)
    idempotent @(Partial Person)
    monoid     @(Partial Person)

    eq        @(WTF Person)
    ord       @(WTF Person)
    semigroup @(WTF Person)
    monoid    @(WTF Person)

    lens @(WTF Person) (position @1)
    lens @(WTF Person) (position @2)
    lens @(WTF Person) (position @3)

    lens @(Partial Person) (field @"name")
    lens @(Partial Person) (field @"age")
    lens @(Partial Person) (field @"likesDogs")

    lens @(WTF Person) (field @"name")
    lens @(WTF Person) (field @"age")
    lens @(WTF Person) (field @"likesDogs")

-------------------------------------------------------------------------------

data Person
  = Person
      { name      :: String
      , age       :: Int
      , likesDogs :: Bool
      }
  deriving (Eq, Generic, Ord, Show)

data Triple
  = Triple String Bool ()
  deriving (Eq, Generic, Ord, Show)

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary

instance CoArbitrary Person
instance Function    Person

instance Arbitrary Triple where
  arbitrary = Triple <$> arbitrary <*> arbitrary <*> arbitrary

instance CoArbitrary Triple
instance Function    Triple

-------------------------------------------------------------------------------

eq
  :: forall a. (Arbitrary a, CoArbitrary a, Eq a, Function a, Show a)
  => SpecWith ()

eq = describe "Eq" do
  it "is reflexive" $ property \(x :: a) ->
    x == x

  it "is symmetric"  $ property \(x :: a) y ->
    (x == y) == (y == x)

  it "is transitive" $ property \(x :: a) y z ->
    not (x == y && y == z) || (x == z)

  it "substitutes" $ property \(x :: a) y (Fun _ f :: Fun a Int) ->
    not (x == y) || (f x == f y)

ord :: forall a. (Arbitrary a, Ord a, Show a) => SpecWith ()
ord = describe "Ord" do
  it "is transitive" $ property \(x :: a) y z ->
    not (x <= y && y <= z) || (x <= z)

  it "is reflexive" $ property \(x :: a) ->
    x <= x

  it "is antisymmetric" $ property \(x :: a) y ->
    not (x <= y && y <= x) || (x == y)

semigroup :: forall a. (Arbitrary a, Eq a, Semigroup a, Show a) => SpecWith ()
semigroup = describe "Semigroup" do
  it "is associative" $ property \(x :: a) y z ->
    x <> (y <> z) == (x <> y) <> z

idempotent :: forall a. (Arbitrary a, Eq a, Semigroup a, Show a) => SpecWith ()
idempotent = describe "Idempotence" do
  it "has right idempotence" $ property \(x :: a) y ->
    x <> y <> y == x <> y

monoid :: forall a. (Arbitrary a, Eq a, Monoid a, Show a) => SpecWith ()
monoid = describe "Monoid" do
  it "has left identity"  $ property \(x :: a) -> mempty <> x == x
  it "has right identity" $ property \(x :: a) -> x <> mempty == x

lens
  :: forall s a
   . ( Arbitrary s, Arbitrary a
     , Show      s, Show      a
     , Eq        a, Eq        s
     )
  => Lens' s a
  -> SpecWith ()

lens l = describe "Lens laws" do
  it "- get l . set l x == x" $ property \(s :: s) (a :: a) ->
    (s & l .~ a) ^. l == a

  it "- set l (get l s) == s" $ property \(s :: s) ->
    (s & l .~ (s ^. l)) == s

  it "- set l b . set l a == set l b" $ property \(s :: s) (a :: a) (b :: a) ->
    (s & l .~ a & l .~ b) == (s & l .~ b)

-------------------------------------------------------------------------------

partials
  :: forall a
   . ( Arbitrary a
     , Show      a
     , Ord       a
     , Generic   a
     , Construct Last a
     , Construct [] a
     , Ord       (Partial a)
     , Ord       (WTF     a)
     )
  => SpecWith ()

partials = describe "HKD" do
  describe "Eq" do
    it "is monotonic with respect to ordering (Partial)" $ property \(x :: a) y ->
      (x <= y) == ((<=) `on` deconstruct @Last) x y

    it "is monotonic with respect to ordering (WTF)" $ property \(x :: a) y ->
      (x <= y) == ((<=) `on` deconstruct @[]) x y

    it "round-trips" $ property \(x :: a) ->
      construct (deconstruct @Last x) == pure x

    it "round-trips" $ property \(x :: a) ->
      construct (deconstruct @[] x) == pure x

-- Just to test that `baddDicts` does what it's told.
data Y = Y { getY :: Int } deriving (Generic, Show)

test :: HKD Y (Product (Dict Num) Identity)
test = baddDicts test
  where
    test :: HKD Y Identity
    test = deconstruct @Identity (Y 10)
