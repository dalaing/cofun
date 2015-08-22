{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module AdderService.CoAdder (
    CoAdder
  , mkCoAdder
  , mkCoAdderWithLogging
  ) where

import           Components.Add.CoAdd         (coAdd)
import           Components.Add.Functors      (CoAddF (..))
import           Components.Clear.CoClear     (coClear)
import           Components.Clear.Functors    (CoClearF (..))
import           Components.Total.CoTotal     (coTotal)
import           Components.Total.Functors    (CoTotalF (..))

import           Util.Console (ConsoleInterpreter(..))
import           Util.Coproduct               (ProductF, (*:*))

import           Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import           Control.Comonad.Trans.Env    (EnvT (..))
import           Control.Comonad.Trans.Store  (StoreT (..))
import           Data.Functor.Identity        (Identity (..))

type CoAdderT = CofreeT (ProductF '[CoAddF, CoClearF, CoTotalF])
type CoAdder = CoAdderT (StoreT Int (EnvT Int Identity))

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count =
    coiterT next start
  where
    next = coAdd *:* (coClear *:* coTotal)
    start = flip StoreT count . EnvT limit . Identity $ const ()

mkCoAdderWithLogging :: Int -> Int -> CoAdder (IO ())
mkCoAdderWithLogging limit count =
    coiterT (addResultLogging <$> next) (return () <$ start)
  where
    next = coAdd *:* (coClear *:* coTotal)
    start = flip StoreT count . EnvT limit . Identity . const $ ()
