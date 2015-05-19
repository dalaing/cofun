module Components.Add.Net.Request (
    AddReq(..)
  ) where

import           Control.Applicative (empty, (<$>))
import           Data.Binary         (Binary (..), getWord8, putWord8)

data AddReq = AddReq Int
  deriving (Eq, Show)

instance Binary AddReq where
  put (AddReq x) = putWord8 0 >> put x

  get = do
    x <- getWord8
    case x of
     0 -> AddReq <$> get
     _ -> empty

