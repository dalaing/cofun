module Components.Clear.Net.Request (
    ClearReq(..)
  ) where

import           Control.Applicative (empty)
import           Data.Binary         (Binary (..), getWord8, putWord8)

data ClearReq = ClearReq
  deriving (Eq, Show)

instance Binary ClearReq where
  put ClearReq = putWord8 1

  get = do
    x <- getWord8
    case x of
     1 -> return ClearReq
     _ -> empty

