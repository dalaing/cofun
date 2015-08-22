{-# LANGUAGE FlexibleContexts #-}
module Zipper.ListT (
    ListZipperT(..)
  , laws
  ) where

import Zipper.Class
import qualified Zipper.List as Z

import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Monad

import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Poly

data ListZipperT w a = ListZipperT [w a] (w a) [w a]
                  deriving (Eq, Show)

instance Arbitrary (w a) => Arbitrary (ListZipperT w a) where
  arbitrary = frequency [
      (1, ListZipperT <$> pure [] <*> arbitrary <*> pure [])
    , (2, ListZipperT <$> arbitrary <*> arbitrary <*> pure [])
    , (2, ListZipperT <$> pure [] <*> arbitrary <*> arbitrary)
    , (10, ListZipperT <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

instance Zipper (ListZipperT w) where
    isLeft (ListZipperT l _ _) = null l
    maybeLeft (ListZipperT [] _ _) = Nothing
    maybeLeft (ListZipperT (l:ls) f r) = Just $ ListZipperT ls l (f:r)
    left z = fromMaybe z . maybeLeft $ z

    isRight (ListZipperT _ _ r) = null r
    maybeRight (ListZipperT _ _ []) = Nothing
    maybeRight (ListZipperT l f (r:rs)) = Just $ ListZipperT (f:l) r rs
    right z = fromMaybe z . maybeRight $ z

instance Functor w => Functor (ListZipperT w) where
    fmap g (ListZipperT l f r) = ListZipperT (fmap (fmap g) l) (fmap g f) (fmap (fmap g) r)

instance Comonad w => Comonad (ListZipperT w) where
    extract (ListZipperT _ f _) = extract f
    duplicate z = ListZipperT lefts (g z) rights
      where
        g t = t <$ (lower t)
        gather f = fmap g . tail . catMaybes . takeWhile isJust . iterate (>>= f) . Just
        lefts = gather maybeLeft z
        rights = gather maybeRight z

instance ComonadTrans ListZipperT where
    lower (ListZipperT _ w _) = w

functorIdentity :: ListZipperT Z.ListZipper A -> Bool
functorIdentity = (==) <$> id <*> (fmap id)

functorComposition :: Fun B C -> Fun A B -> ListZipperT Z.ListZipper A -> Bool
functorComposition ff fg =
  let
    f = apply ff
    g = apply fg
  in
    (==) <$> fmap (f . g) <*> (fmap f . fmap g)

functorLaws :: TestTree
functorLaws = testGroup "functor" [
    testProperty "identity" functorIdentity
  , testProperty "composition" functorComposition
  ]

comonadLaw1 :: ListZipperT Z.ListZipper A -> Bool
comonadLaw1 = (==) <$> id <*> extract . duplicate

comonadLaw2 :: ListZipperT Z.ListZipper A -> Bool
comonadLaw2 = (==) <$> id <*> (fmap extract . duplicate)

comonadLaw3 :: ListZipperT Z.ListZipper A -> Bool
comonadLaw3 = (==) <$> (duplicate . duplicate) <*> (fmap duplicate . duplicate)

comonadLaws :: TestTree
comonadLaws = testGroup "comonad" [
    testProperty "law 1" comonadLaw1
  , testProperty "law 2" comonadLaw2
  , testProperty "law 3" comonadLaw3
  ]

laws :: TestTree
laws = testGroup "ListZipperT" [
    functorLaws
  , comonadLaws
  ]
