module Components.Console.Run (
      run
    ) where

import Util.Pairing (pairEffect')

import AdderService.CoAdder (mkCoAdderWithLogging)

import Components.Console (runConsole)

run :: IO ()
run = pairEffect' (\_ r -> r) (mkCoAdderWithLogging 10 0) runConsole

