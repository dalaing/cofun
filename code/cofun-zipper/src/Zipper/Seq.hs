module Zipper.Seq (
    SeqZipper(..)
  , laws
  ) where

import Zipper.Class

import Control.Comonad
import Control.Monad

import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Poly

data SeqZipper a = SeqZipper (S.Seq a) a (S.Seq a)
                  deriving (Eq, Show)

toSeq :: SeqZipper a -> S.Seq a
toSeq (SeqZipper l f r) = (S.reverse l) S.>< (f S.<| r)

toReverseSeq :: SeqZipper a -> S.Seq a
toReverseSeq (SeqZipper l f r) = (S.reverse r) S.>< (f S.<| l)


instance Arbitrary a => Arbitrary (SeqZipper a) where
  arbitrary = frequency [
      (1, SeqZipper <$> pure (S.empty) <*> arbitrary <*> pure (S.empty))
    , (2, SeqZipper <$> (S.fromList <$> arbitrary) <*> arbitrary <*> pure (S.empty))
    , (2, SeqZipper <$> pure (S.empty) <*> arbitrary <*> (S.fromList <$> arbitrary))
    , (10, SeqZipper <$> (S.fromList <$> arbitrary) <*> arbitrary <*> (S.fromList <$> arbitrary))
    ]

instance Zipper SeqZipper where
    isLeft (SeqZipper l _ _) = S.null l
    maybeLeft (SeqZipper l f r) = case S.viewl l of
      S.EmptyL -> Nothing
      l' S.:< ls' -> Just $ SeqZipper ls' l' (f S.<| r)
    left z = fromMaybe z . maybeLeft $ z

    isRight (SeqZipper _ _ r) = S.null r
    maybeRight (SeqZipper l f r) = case S.viewl r of
      S.EmptyL -> Nothing
      r' S.:< rs' -> Just $ SeqZipper (f S.<| l) r' rs'
    right z = fromMaybe z . maybeRight $ z

instance Functor SeqZipper where
    fmap g (SeqZipper l f r) = SeqZipper (fmap g l) (g f) (fmap g r)

instance Foldable SeqZipper where
    foldMap g (SeqZipper l f r) = F.foldMap g (S.reverse l) <> g f <> F.foldMap g r

instance Traversable SeqZipper where
    traverse g (SeqZipper l f r) =
      SeqZipper <$> (S.reverse <$> T.traverse g (S.reverse l)) <*> g f <*> T.traverse g r

instance Applicative SeqZipper where
    pure = return
    (<*>) = ap

instance Monad SeqZipper where
    return x = SeqZipper S.empty x S.empty
    (SeqZipper ls f rs) >>= g =
      let
        (SeqZipper ls' f' rs') = g f
      in
        SeqZipper (ls' S.>< (ls >>= (toReverseSeq . g))) f' (rs' S.>< (rs >>= (toSeq . g)))

instance Comonad SeqZipper where
    extract (SeqZipper _ f _) = f
    duplicate z = SeqZipper lefts z rights
      where
        gather f = S.fromList . tail . catMaybes . takeWhile isJust . iterate (>>= f) . Just
        lefts = gather maybeLeft z
        rights = gather maybeRight z

instance ComonadApply SeqZipper where
    (<@>) = (<*>)

functorIdentity :: SeqZipper A -> Bool
functorIdentity = (==) <$> id <*> (fmap id)

functorComposition :: Fun B C -> Fun A B -> SeqZipper A -> Bool
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

applicativeIdentity :: SeqZipper A -> Bool
applicativeIdentity v = v == (pure id <*> v)

applicativeComposition :: SeqZipper (Fun B C) -> SeqZipper (Fun A B) -> SeqZipper A -> Bool
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
    (pure f <*> pure x) == (pure (f x) :: SeqZipper B)

applicativeInterchange :: SeqZipper (Fun A B) -> A -> Bool
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

monadLeftIdentity :: A -> Fun A (SeqZipper B) -> Bool
monadLeftIdentity a ff =
  let
    f = apply ff
  in
    (return a >>= f) == f a

monadRightIdentity :: SeqZipper A -> Bool
monadRightIdentity z = (z >>= return) == z

monadAssociativity :: SeqZipper A -> Fun A (SeqZipper B) -> Fun B (SeqZipper C) -> Bool
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

comonadLaw1 :: SeqZipper A -> Bool
comonadLaw1 = (==) <$> id <*> extract . duplicate

comonadLaw2 :: SeqZipper A -> Bool
comonadLaw2 = (==) <$> id <*> (fmap extract . duplicate)

comonadLaw3 :: SeqZipper A -> Bool
comonadLaw3 = (==) <$> (duplicate . duplicate) <*> (fmap duplicate . duplicate)

comonadLaws :: TestTree
comonadLaws = testGroup "comonad" [
    testProperty "law 1" comonadLaw1
  , testProperty "law 2" comonadLaw2
  , testProperty "law 3" comonadLaw3
  ]

coapplicativeLaw1 :: SeqZipper (Fun B C) -> SeqZipper (Fun A B) -> SeqZipper A -> Bool
coapplicativeLaw1 x y w =
  let
    u = fmap apply x
    v = fmap apply y
  in
    ((.) <$> u <@> v <@> w) == (u <@> (v <@> w))

coapplicativeLaw2 :: SeqZipper (Fun A B) -> SeqZipper A -> Bool
coapplicativeLaw2 z q =
  let
    p = fmap apply z
  in
    extract (p <@> q) == extract p (extract q)

coapplicativeLaw3 :: SeqZipper (Fun A B) -> SeqZipper A -> Bool
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
laws = testGroup "SeqZipper" [
    functorLaws
  , applicativeLaws
  , monadLaws
  , comonadLaws
  , coapplicativeLaws
  ]
