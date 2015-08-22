module Components.Add.Packets (AddReq(..), AddRes(..)) where

import Data.Binary

data AddReq = AddReq Int
              deriving (Eq, Show)

instance Binary AddReq where
  put (AddReq i) = put i
  get = AddReq <$> get

data AddRes = AddRes Bool
              deriving (Eq, Show)

instance Binary AddRes where
  put (AddRes b) = put b
  get = AddRes <$> get
