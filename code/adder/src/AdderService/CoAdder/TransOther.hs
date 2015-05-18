module AdderService.CoAdder.TransOther (
    CoAdder
  , mkCoAdder
  ) where

import           AdderService.Functors        (CoAdderF (..))

import           Control.Applicative          ((<$>), (<*>))
import           Control.Comonad              (extend, extract)
import           Control.Comonad.Store        (StoreT (..), pos, seek)
import           Control.Comonad.Trans.Class  (lower)
import           Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import           Control.Comonad.Trans.Env    (EnvT (..), ask)
import           Data.Functor.Identity        (Identity (..))

type CoAdder = CoAdderT Base
type Base = EnvT Int (StoreT Int Identity)
type CoAdderT = CofreeT CoAdderF

coAdd :: Base a -> Int -> (Bool, Base a)
coAdd w x = (test, extend (extract . seek next . lower) w)
  where
    count = pos . lower $ w
    limit = ask w
    x' = count + x
    test = x' <= limit
    next = if test then x' else count

coClear :: Base a -> Base a
coClear = extend (extract . seek 0 . lower)

coTotal :: Base a -> (Int, Base a)
coTotal w = (pos (lower w), w)

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count =
    coiterT next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = EnvT limit . flip StoreT count . Identity $ const ()
