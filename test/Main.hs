{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
module Main where

import Data.Partial
import GHC.Generics
import Test.DocTest
import Test.Hspec
import Test.QuickCheck.Arbitrary
import Test.QuickCheck

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

main :: IO ()
main = do
  doctest ["src", "test"]

  hspec do
    describe "Unnamed" do
      eq         @(Partial Triple)
      ord        @(Partial Triple)
      semigroup  @(Partial Triple)
      idempotent @(Partial Triple)
      monoid     @(Partial Triple)

    describe "Named" do
      eq         @(Partial Person)
      ord        @(Partial Person)
      semigroup  @(Partial Person)
      idempotent @(Partial Person)
      monoid     @(Partial Person)

    it "is monotonic with respect to ordering" $ property \(x :: Person) y ->
      (x <= y) == (toPartial x <= toPartial y)

    describe "Show" do
      it "mimics the total structure's 'Show' instance"
        $ property \(x :: Person) -> show (toPartial x) == show x

      it "prints (unnamed) empty fields as ???" do
        show (mempty :: Partial Triple) `shouldBe`
          "Triple ??? ??? ???"

      it "prints (named) empty fields as ???" do
        show (mempty @(Partial Person)) `shouldBe`
          "Person {name = ???, age = ???, likesDogs = ???}"

    describe "toPartial / fromPartial" do
      it "round-trips (named)" $ property \(x :: Person) ->
        fromPartial (toPartial x) == Just x

      it "round-trips (unnamed)" $ property \(x :: Triple) ->
        fromPartial (toPartial x) == Just x
