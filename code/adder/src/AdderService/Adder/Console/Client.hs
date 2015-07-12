module AdderService.Adder.Console.Client (
    consoleAdder
  ) where

import           AdderService.Adder     (AdderT, add, clear, total)

import           Control.Monad          (forever, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)

consoleAdder' :: MonadIO m => AdderT m ()
consoleAdder' = do
    l <- liftIO getLine
    case words l of
      ["add", x] -> void $ add (read x)
      ["clear"] -> clear
      ["total"] -> void total
      _ -> output prompt
  where
    output = liftIO . putStrLn
    prompt = unlines [
             "Commands:"
           , "  add [int]"
           , "  clear"
           , "  total"
           ]

consoleAdder :: MonadIO m => AdderT m ()
consoleAdder = forever consoleAdder'
