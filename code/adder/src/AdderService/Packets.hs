module AdderService.Packets (
      AdderReq(..)
    , AdderRes(..)
    ) where

import Control.Applicative
import Data.Binary

data AdderReq = AddReq Int 
              | ClearReq
              | TotalReq
              deriving (Eq, Show)

instance Binary AdderReq where
    put (AddReq i) = putWord8 0 >> put i
    put ClearReq = putWord8 1 
    put TotalReq = putWord8 2

    get = do
        x <- getWord8
        case x of
            0 -> AddReq <$> get
            1 -> return ClearReq
            2 -> return TotalReq
            _ -> empty

data AdderRes = AddRes Bool
              | ClearRes
              | TotalRes Int
              deriving (Eq, Show)

instance Binary AdderRes where
    put (AddRes b) = putWord8 0 >> put b
    put ClearRes = putWord8 1
    put (TotalRes i) = putWord8 2 >> put i

    get = do
        x <- getWord8
        case x of
            0 -> AddRes <$> get
            1 -> return ClearRes
            2 -> TotalRes <$> get
            _ -> empty

