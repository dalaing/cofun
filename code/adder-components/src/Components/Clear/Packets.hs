module Components.Clear.Packets (ClearReq(..), ClearRes(..)) where

import Data.Binary

data ClearReq = ClearReq
                deriving (Eq, Show)

instance Binary ClearReq where
  put ClearReq = return ()
  get = return ClearReq

data ClearRes = ClearRes
                deriving (Eq, Show)

instance Binary ClearRes where
  put ClearRes = return ()
  get = return ClearRes

