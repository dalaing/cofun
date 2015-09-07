module AdderService.Network (
      runClient
    , runInterpreter
    ) where

import AdderService.Adder (AdderF)
import           AdderService.CoAdder (mkCoAdderWithLogging)

import Util.Console
import qualified Util.Network.Client as C
import qualified Util.Network.Interpreter as I
import Util.Network.Errors

import Control.Monad.Trans.Free
import Control.Monad.Reader
import Control.Monad.Except

import Network.Simple.TCP

runClient :: HostName -> ServiceName -> IO (Either NetError ())
runClient host service = C.runClient host service (C.pairClient console)
  where
    console = runConsole WithLogging :: FreeT AdderF (ReaderT Socket (ExceptT NetError IO)) ()

runInterpreter :: HostName -> ServiceName -> IO ()
runInterpreter host service = I.runInterpreter host service (I.pairInterpreter $ mkCoAdderWithLogging 10 0)
