module AdderService.CoAdder.Pure (
    CoAdder
  , mkCoAdder
  ) where

import           AdderService.Functors  (CoAdderF (..))

import           Control.Comonad.Cofree (Cofree, coiter)

type CoAdder = Cofree CoAdderF

coAdd :: (Int, Int) -> Int -> (Bool, (Int, Int))
coAdd (limit, count) x = (test, (limit, next))
  where
    count' = count + x
    test = count' <= limit
    next = if test then count' else count

coClear :: (Int, Int) -> (Int, Int)
coClear (limit, _) = (limit, 0)

coTotal :: (Int, Int) -> (Int, (Int, Int))
coTotal (limit, count) = (count, (limit, count))

mkCoAdder :: Int -> Int -> CoAdder (Int, Int)
mkCoAdder limit count = coiter next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = (limit, count)
