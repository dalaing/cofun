module Components.Total.Packets (TotalReq(..), TotalRes(..)) where

import Data.Binary

data TotalReq = TotalReq
                deriving (Eq, Show)

instance Binary TotalReq where
  put TotalReq = return ()
  get = return TotalReq

data TotalRes = TotalRes Int
                deriving (Eq, Show)

instance Binary TotalRes where
  put (TotalRes i) = put i
  get = TotalRes <$> get

