{-# LANGUAGE ScopedTypeVariables #-}
module BindingsTest
  ( tests
  ) where

import           Hedgehog                       ( (===)
                                                , Property
                                                , forAll
                                                , property
                                                )
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.HUnit                     ( (@?=)
                                                , Assertion
                                                )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( testCase )
import           Test.Tasty.Hedgehog            ( testProperty )

import qualified Bindings

tests :: TestTree
tests = testGroup
  "Bindings"
  [ testCase "succInt8 example" succInt8_example
  , testProperty "succInt8 prop" (succProp Bindings.succInt8)
  ]

succInt8_example :: Assertion
succInt8_example = Bindings.succInt8 0 @?= 1

succProp :: forall a . (Show a, Integral a, Bounded a) => (a -> a) -> Property
succProp succFn = property $ do
  input <- forAll $ Gen.integral (Range.constant minBound maxBound)
  let expected :: a
      expected = if input < maxBound then input + 1 else minBound
  succFn input === expected
