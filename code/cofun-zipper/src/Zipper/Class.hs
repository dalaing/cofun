module Zipper.Class (
      Zipper(..)
    ) where

class Zipper z where
  isLeft :: z a -> Bool
  left :: z a -> z a
  maybeLeft :: z a -> Maybe (z a)

  isRight :: z a -> Bool
  right :: z a -> z a
  maybeRight :: z a -> Maybe (z a)
