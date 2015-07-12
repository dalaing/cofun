{-# LANGUAGE FlexibleContexts #-}
module AdderService.Adder.Console.Interpreter (
    mkCoAdderWithLogging
  ) where

import           AdderService.Functors        (CoAdderF (..))

import           Control.Comonad.Env.Class    (ComonadEnv, ask)
import           Control.Comonad.Store.Class  (ComonadStore, pos, seek)
import           Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import           Control.Comonad.Trans.Env    (EnvT (..))
import           Control.Comonad.Trans.Store  (StoreT (..))
import           Data.Functor.Identity        (Identity (..))

type CoAdderT = CofreeT CoAdderF
type CoAdder = CoAdderT (StoreT Int (EnvT Int Identity))

coAdd :: (ComonadEnv Int w, ComonadStore Int w) => w a -> Int -> (Bool, w a)
coAdd w x = (test, seek next w)
  where
    count = pos w
    limit = ask w
    count' = count + x
    test = count' <= limit
    next = if test then count' else count

coClear :: ComonadStore Int w => w a -> w a
coClear = seek 0

coTotal :: ComonadStore Int w => w a -> (Int, w a)
coTotal w = (pos w, w)

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count =
    coiterT next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = flip StoreT count . EnvT limit . Identity $ const ()

addResultLogging :: Functor f => CoAdderF (f a) -> CoAdderF (f (IO ()))
addResultLogging (CoAdderF a c t) = CoAdderF a' c' t'
  where
    a' x =
      let
        (b, k) = a x
      in
        (b, putStrLn ("add result: " ++ show b) <$ k)
    c' = return () <$ c
    t' =
      let
        (i, k) = t
      in
        (i, putStrLn ("total result: " ++ show i) <$ k)

mkCoAdderWithLogging :: Int -> Int -> CoAdder (IO ())
mkCoAdderWithLogging limit count =
    coiterT (addResultLogging <$> next) (return () <$ start)
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = flip StoreT count . EnvT limit . Identity $ const ()
