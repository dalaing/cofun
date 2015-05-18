module AdderService.CoAdder.Trans (
    CoAdder
  , mkCoAdder
  ) where

import           AdderService.Functors        (CoAdderF (..))

import           Control.Applicative          ((<$>), (<*>))
import           Control.Comonad.Store        (StoreT (..), pos, seek)
import           Control.Comonad.Trans.Class  (lower)
import           Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import           Control.Comonad.Trans.Env    (EnvT (..), ask)
import           Data.Functor.Identity        (Identity (..))

type CoAdder = CoAdderT Base
type Base = StoreT Int (EnvT Int Identity)
type CoAdderT = CofreeT CoAdderF

coAdd :: Base a -> Int -> (Bool, Base a)
coAdd w x = (test, seek next w)
  where
    count = pos  w
    limit = ask . lower $ w
    x' = count + x
    test = x' <= limit
    next = if test then x' else count

coClear :: Base a -> Base a
coClear = seek 0

coTotal :: Base a -> (Int, Base a)
coTotal w = (pos w, w)

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count = coiterT next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = flip StoreT count . EnvT limit . Identity $ const ()
