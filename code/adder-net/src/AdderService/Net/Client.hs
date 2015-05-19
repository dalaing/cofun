{-# LANGUAGE FlexibleContexts      #-}
module AdderService.Net.Client (
    runClient
  ) where

import           AdderService.Net.Errors      (NetError (..))
import           AdderService.Net.Functors    (ServerF (..))

import           AdderService.Adder.Console   (consoleAdder)

import           Util.Pairing                 (pairEffectM)

import           Control.Applicative          ((<$>))
import           Control.Comonad.Trans.Cofree (Cofree, coiterT)
import           Control.Monad.Except         (MonadError, runExceptT)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Reader         (MonadReader, ask, runReaderT)
import           Data.Functor.Identity        (Identity (..))

import           Data.Binary                  (decode, encode)
import Control.Error.Util (note)
import qualified Data.ByteString.Lazy         as L (fromStrict, toStrict)
import           Network.Simple.TCP           (HostName, ServiceName, Socket,
                                               connect, recv, send)

mkServer :: (Functor m, MonadReader Socket m, MonadIO m) => Cofree (ServerF m) ()
mkServer = coiterT f (Identity ())
  where
    f w = ServerF $ \req -> do
      s <- ask
      send s . L.toStrict . encode $ req
      res <- fmap (decode . L.fromStrict) <$> recv s 1024
      return (note Disconnected res, w)

runClient' :: (Functor m, MonadReader Socket m, MonadError NetError m, MonadIO m) => m ()
runClient' = pairEffectM (\_ r -> return r) mkServer consoleAdder

runClient :: HostName -> ServiceName -> IO ()
runClient host service = connect host service $ \(sock, _) -> do
  e <- runExceptT $ runReaderT runClient' sock
  case e of
   Left l -> print l
   Right _ -> return ()
