module Components.Clear.Class (
    HasClear(..)
  ) where

class HasClear c where
  clear :: c ()
