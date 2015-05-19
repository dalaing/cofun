module AdderService.Net.Request (
    Request(..)
  ) where

import           Control.Applicative (empty, (<$>))
import           Data.Binary         (Binary (..), getWord8, putWord8)

data Request =
    AddReq Int
  | ClearReq
  | TotalReq
  deriving (Eq, Show)

instance Binary Request where
  put (AddReq x) = putWord8 0 >> put x
  put ClearReq = putWord8 1
  put TotalReq = putWord8 2

  get = do
    x <- getWord8
    case x of
     0 -> AddReq <$> get
     1 -> return ClearReq
     2 -> return TotalReq
     _ -> empty

