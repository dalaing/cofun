module AdderService.Console (
    run
  ) where

import           AdderService.CoAdder (mkCoAdderWithLogging)

import           Util.Pairing (pairEffect)
import           Util.Console (runConsole, ConsoleLogging(..))

run :: IO ()
run = pairEffect (\_ r -> r) (mkCoAdderWithLogging 10 0) (runConsole WithoutLogging)

