module Components.Total.Net.Response (
    TotalRes(..)
  ) where

import           Control.Applicative (empty, (<$>))
import           Data.Binary         (Binary (..), getWord8, putWord8)

data TotalRes = TotalRes Int
  deriving (Eq, Show)

instance Binary TotalRes where
  put (TotalRes x) = putWord8 2 >> put x

  get = do
    x <- getWord8
    case x of
     2 -> TotalRes <$> get
     _ -> empty

