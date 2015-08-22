module Zipper.List (
    ListZipper(..)
  , laws
  ) where

import Zipper.Class

import Control.Comonad
import Control.Monad

import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Poly

data ListZipper a = ListZipper [a] a [a]
                  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ListZipper a) where
  arbitrary = frequency [
      (1, ListZipper <$> pure [] <*> arbitrary <*> pure [])
    , (2, ListZipper <$> arbitrary <*> arbitrary <*> pure [])
    , (2, ListZipper <$> pure [] <*> arbitrary <*> arbitrary)
    , (10, ListZipper <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

instance Zipper ListZipper where
    isLeft (ListZipper l _ _) = null l
    maybeLeft (ListZipper [] _ _) = Nothing
    maybeLeft (ListZipper (l:ls) f r) = Just $ ListZipper ls l (f:r)
    left z = fromMaybe z . maybeLeft $ z

    isRight (ListZipper _ _ r) = null r
    maybeRight (ListZipper _ _ []) = Nothing
    maybeRight (ListZipper l f (r:rs)) = Just $ ListZipper (f:l) r rs
    right z = fromMaybe z . maybeRight $ z

instance Functor ListZipper where
    fmap g (ListZipper l f r) = ListZipper (fmap g l) (g f) (fmap g r)

instance Foldable ListZipper where
    foldMap g (ListZipper l f r) = F.foldMap g (reverse l) <> g f <> F.foldMap g r

instance Traversable ListZipper where
    traverse g (ListZipper l f r) =
      ListZipper <$> (reverse <$> T.traverse g (reverse l)) <*> g f <*> T.traverse g r

instance Applicative ListZipper where
    pure = return
    (<*>) = ap

instance Monad ListZipper where
    return x = ListZipper [] x []
    (ListZipper ls f rs) >>= g =
      let
        (ListZipper ls' f' rs') = g f
      in
        ListZipper (ls' ++ (ls >>= (reverse . F.toList . g))) f' (rs' ++ (rs >>= (F.toList . g)))

instance Comonad ListZipper where
    extract (ListZipper _ f _) = f
    duplicate z = ListZipper lefts z rights
      where
        gather f = tail . catMaybes . takeWhile isJust . iterate (>>= f) . Just
        lefts = gather maybeLeft z
        rights = gather maybeRight z

instance ComonadApply ListZipper where
    (<@>) = (<*>)

functorIdentity :: ListZipper A -> Bool
functorIdentity = (==) <$> id <*> (fmap id)

functorComposition :: Fun B C -> Fun A B -> ListZipper A -> Bool
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

applicativeIdentity :: ListZipper A -> Bool
applicativeIdentity v = v == (pure id <*> v)

applicativeComposition :: ListZipper (Fun B C) -> ListZipper (Fun A B) -> ListZipper A -> Bool
applicativeComposition x y w =
  let
    u = fmap apply x
    v = fmap apply y
  in
    (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

applicativeHomomorphism :: Fun A B -> A -> Bool
applicativeHomomorphism ff x =
  let
    f = apply ff
  in
    (pure f <*> pure x) == (pure (f x) :: ListZipper B)

applicativeInterchange :: ListZipper (Fun A B) -> A -> Bool
applicativeInterchange z y =
  let
    u = fmap apply z
  in
    (u <*> pure y) == (pure ($ y) <*> u)

applicativeLaws :: TestTree
applicativeLaws = testGroup "applicative" [
    testProperty "identity" applicativeIdentity
  , testProperty "composition" applicativeComposition
  , testProperty "homomorphism" applicativeHomomorphism
  , testProperty "interchange" applicativeInterchange
  ]

monadLeftIdentity :: A -> Fun A (ListZipper B) -> Bool
monadLeftIdentity a ff =
  let
    f = apply ff
  in
    (return a >>= f) == f a

monadRightIdentity :: ListZipper A -> Bool
monadRightIdentity z = (z >>= return) == z

monadAssociativity :: ListZipper A -> Fun A (ListZipper B) -> Fun B (ListZipper C) -> Bool
monadAssociativity m ff fg =
  let
    f = apply ff
    g = apply fg
  in
    ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))

monadLaws :: TestTree
monadLaws = testGroup "monad" [
    testProperty "left identity" monadLeftIdentity
  , testProperty "right identity" monadRightIdentity
  , testProperty "associativity" monadAssociativity
  ]

comonadLaw1 :: ListZipper A -> Bool
comonadLaw1 = (==) <$> id <*> extract . duplicate

comonadLaw2 :: ListZipper A -> Bool
comonadLaw2 = (==) <$> id <*> (fmap extract . duplicate)

comonadLaw3 :: ListZipper A -> Bool
comonadLaw3 = (==) <$> (duplicate . duplicate) <*> (fmap duplicate . duplicate)

comonadLaws :: TestTree
comonadLaws = testGroup "comonad" [
    testProperty "law 1" comonadLaw1
  , testProperty "law 2" comonadLaw2
  , testProperty "law 3" comonadLaw3
  ]

coapplicativeLaw1 :: ListZipper (Fun B C) -> ListZipper (Fun A B) -> ListZipper A -> Bool
coapplicativeLaw1 x y w =
  let
    u = fmap apply x
    v = fmap apply y
  in
    ((.) <$> u <@> v <@> w) == (u <@> (v <@> w))

coapplicativeLaw2 :: ListZipper (Fun A B) -> ListZipper A -> Bool
coapplicativeLaw2 z q =
  let
    p = fmap apply z
  in
    extract (p <@> q) == extract p (extract q)

coapplicativeLaw3 :: ListZipper (Fun A B) -> ListZipper A -> Bool
coapplicativeLaw3 z q =
  let
    p = fmap apply z
  in
    duplicate (p <@> q) == ((<@>) <$> duplicate p <@> duplicate q)

coapplicativeLaws :: TestTree
coapplicativeLaws = testGroup "coapplicative" [
    testProperty "law 1" coapplicativeLaw1
  , testProperty "law 2" coapplicativeLaw2
  , testProperty "law 3" coapplicativeLaw3
  ]

laws :: TestTree
laws = testGroup "ListZipper" [
    functorLaws
  , applicativeLaws
  , monadLaws
  , comonadLaws
  , coapplicativeLaws
  ]
