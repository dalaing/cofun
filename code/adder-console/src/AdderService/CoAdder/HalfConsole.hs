{-# LANGUAGE FlexibleContexts      #-}
module AdderService.CoAdder.HalfConsole (
    CoAdder
  , mkCoAdder
  ) where

import           AdderService.Functors        (CoAdderF (..))

import           Control.Applicative          ((<$>), (<*>))
import           Control.Comonad.Env.Class    (ComonadEnv, ask)
import           Control.Comonad.Store.Class  (ComonadStore, pos, seek)
import           Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import           Control.Comonad.Trans.Env    (EnvT (..))
import           Control.Comonad.Trans.Store  (StoreT (..))
import           Data.Functor ((<$))
import           Data.Functor.Identity        (Identity (..))

type CoAdderT = CofreeT CoAdderF
type CoAdder = CoAdderT (StoreT Int (EnvT Int Identity))

coAdd :: (ComonadEnv Int w, ComonadStore Int w) => w (IO ()) -> Int -> (Bool, w (IO ()))
coAdd w x = (test, putStrLn ("add result: " ++ show test) <$ seek next w)
  where
    count = pos w
    limit = ask w
    count' = count + x
    test = count' <= limit
    next = if test then count' else count

coClear :: ComonadStore Int w => w (IO ()) -> w (IO ())
coClear w = return () <$ seek 0 w

coTotal :: ComonadStore Int w => w (IO ()) -> (Int, w (IO ()))
coTotal w = (total, putStrLn ("total result: " ++ show total) <$ w)
  where
    total = pos w

mkCoAdder :: Int -> Int -> CoAdder (IO ())
mkCoAdder limit count =
    coiterT next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = flip StoreT count . EnvT limit . Identity $ const (return ())

