module AdderService.Net.Response (
    Response(..)
  ) where

import           Control.Applicative (empty, (<$>))
import           Data.Binary         (Binary (..), getWord8, putWord8)

data Response =
    AddRes Bool
  | ClearRes
  | TotalRes Int
  deriving (Eq, Show)

instance Binary Response where
  put (AddRes x) = putWord8 0 >> put x
  put ClearRes = putWord8 1
  put (TotalRes x) = putWord8 2 >> put x

  get = do
    x <- getWord8
    case x of
     0 -> AddRes <$> get
     1 -> return ClearRes
     2 -> TotalRes <$> get
     _ -> empty

