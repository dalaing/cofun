module AdderService.CoAdder.TransOther (
    CoAdder
  , mkCoAdder
  ) where

import           AdderService.Functors        (CoAdderF (..))

import           Control.Applicative          ((<$>), (<*>))
import           Control.Comonad              (duplicate)
import           Control.Comonad.Store        (StoreT (..), pos, peek)
import           Control.Comonad.Trans.Class  (lower)
import           Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import           Control.Comonad.Trans.Env    (EnvT (..), ask)
import           Data.Functor.Identity        (Identity (..))

type CoAdder = CoAdderT Base
type Base = EnvT Int (StoreT Int Identity)
type CoAdderT = CofreeT CoAdderF

coAdd :: Base a -> Int -> (Bool, Base a)
coAdd w x = (test, peek next . lower . duplicate $ w)
  where
    count = pos . lower $ w
    limit = ask w
    count' = count + x
    test = count' <= limit
    next = if test then count' else count

coClear :: Base a -> Base a
coClear = peek 0 . lower . duplicate

coTotal :: Base a -> (Int, Base a)
coTotal w = (pos (lower w), w)

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count =
    coiterT next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = EnvT limit . flip StoreT count . Identity $ const ()
