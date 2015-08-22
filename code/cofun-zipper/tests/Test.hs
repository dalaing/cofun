module Main where

import Test.Tasty

import qualified Zipper.List as L
import qualified Zipper.ListT as LT
import qualified Zipper.Seq as S

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
        LT.laws
      ]

