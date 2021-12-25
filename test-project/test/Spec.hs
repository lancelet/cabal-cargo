-- |

module Main
  ( main
  ) where

import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )

import qualified BindingsTest                   ( tests )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Test Project Tests" [BindingsTest.tests]
