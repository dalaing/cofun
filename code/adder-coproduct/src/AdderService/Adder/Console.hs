{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module AdderService.Adder.Console (
    consoleAdder
  ) where

import           Components.Add.Add        (add)
import           Components.Add.Functors   (AddF)
import           Components.Clear.Clear    (clear)
import           Components.Clear.Functors (ClearF)
import           Components.Total.Functors (TotalF)
import           Components.Total.Total    (total)

import           Util.Coproduct            ((:<:) (..))

import           Control.Monad             (forever)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Free  (FreeT)

consoleAdder' :: (MonadIO m, AddF :<: f, ClearF :<: f, TotalF :<: f)=> FreeT f m ()
consoleAdder' = do
    l <- liftIO getLine
    case words l of
      ["add", x] -> add (read x) >>= \b ->
        output $ "add result: " ++ show b
      ["clear"] -> clear
      ["total"] -> total >>= \t ->
        output $ "total result: " ++ show t
      _ -> output prompt
  where
   output = liftIO . putStrLn
   prompt = unlines [
            "Commands:"
          , "  add [int]"
          , "  clear"
          ,"  total"
          ]

consoleAdder :: (MonadIO m, AddF :<: f, ClearF :<: f, TotalF :<: f)=> FreeT f m ()
consoleAdder = forever consoleAdder'

-- test :: IO ()
-- test = pairEffect (\_ r -> r) (mkCoAdder 10 0) consoleAdder
