module Components.Clear.Net.Response (
    ClearRes(..)
  ) where

import           Control.Applicative (empty)
import           Data.Binary         (Binary (..), getWord8, putWord8)

data ClearRes = ClearRes
  deriving (Eq, Show)

instance Binary ClearRes where
  put ClearRes = putWord8 1

  get = do
    x <- getWord8
    case x of
     1 -> return ClearRes
     _ -> empty

