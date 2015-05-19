module AdderService.Net.Errors (
    NetError(..)
  ) where

data NetError = Disconnected
              | UnexpectedRequest
              | UnexpectedResponse
              deriving (Eq, Show)

