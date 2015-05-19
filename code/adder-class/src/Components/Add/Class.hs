module Components.Add.Class (
    HasAdd(..)
  ) where

class HasAdd c where
  add :: Int -> c Bool
