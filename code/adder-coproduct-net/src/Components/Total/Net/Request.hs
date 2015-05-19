module Components.Total.Net.Request (
    TotalReq(..)
  ) where

import           Control.Applicative (empty)
import           Data.Binary         (Binary (..), getWord8, putWord8)

data TotalReq = TotalReq
  deriving (Eq, Show)

instance Binary TotalReq where
  put TotalReq = putWord8 2

  get = do
    x <- getWord8
    case x of
     2 -> return TotalReq
     _ -> empty

