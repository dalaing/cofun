module Components.Total.Class (
    HasTotal(..)
  ) where

class HasTotal c where
  total :: c Int
