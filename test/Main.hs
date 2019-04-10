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
import Data.Function ((&))
import Data.Generic.HKD
import GHC.Generics
import Test.DocTest
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Data.Monoid (Last (..))

main :: IO ()
main = do
  doctest ["src", "test"]

  hspec do
    describe "Unnamed" do
      eq         @(HKD Last Triple)
      ord        @(HKD Last Triple)
      semigroup  @(HKD Last Triple)
      idempotent @(HKD Last Triple)
      monoid     @(HKD Last Triple)

      partials @Triple
      lens @(HKD Maybe Triple) (position @1)
      lens @(HKD Maybe Triple) (position @2)
      lens @(HKD Maybe Triple) (position @3)

    describe "Named" do
      eq         @(HKD Last Person)
      ord        @(HKD Last Person)
      semigroup  @(HKD Last Person)
      idempotent @(HKD Last Person)
      monoid     @(HKD Last Person)

      partials @Person
      lens @(HKD Maybe Person) (field @"name")
      lens @(HKD Maybe Person) (field @"age")
      lens @(HKD Maybe Person) (field @"likesDogs")

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

instance Arbitrary Triple where
  arbitrary = Triple <$> arbitrary <*> arbitrary <*> arbitrary

-------------------------------------------------------------------------------

eq :: forall a. (Arbitrary a, CoArbitrary a, Eq a, Function a, Show a) => SpecWith ()
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

partials
  :: forall a
   . ( Arbitrary a, Arbitrary (HKD Last a)
     , Show      a, Show      (HKD Last a)
     , Ord       a, Ord       (HKD Last a)

     , Generic a
     , Construct Last a
     , Monoid (HKD Last a)
     )
  => SpecWith ()

partials = describe "HKD" do
  describe "Eq" do
    it "is monotonic with respect to ordering"
      $ property \(x :: a) y ->
          (x <= y) == (deconstruct @Last x <= deconstruct @Last y)

    it "round-trips"
      $ property \(x :: a) ->
          construct (deconstruct @Last x) == pure x

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
