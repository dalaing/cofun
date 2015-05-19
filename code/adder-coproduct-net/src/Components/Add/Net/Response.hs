module Components.Add.Net.Response (
    AddRes(..)
  ) where

import           Control.Applicative (empty, (<$>))
import           Data.Binary         (Binary (..), getWord8, putWord8)

data AddRes = AddRes Bool
  deriving (Eq, Show)

instance Binary AddRes where
  put (AddRes x) = putWord8 0 >> put x

  get = do
    x <- getWord8
    case x of
     0 -> AddRes <$> get
     _ -> empty

