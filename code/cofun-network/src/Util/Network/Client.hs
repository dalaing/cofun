{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Util.Network.Client (
      mkClientConnector
    , pairClient
    , runClient
    ) where

import Util.Network
import Util.Network.Functors
import Util.Network.Errors

import Util.Pairing

import Control.Comonad.Trans.Cofree
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Free
import Control.Monad.Catch
import Data.Functor.Identity

import Data.Binary
import qualified Data.ByteString.Lazy as L
import Network.Simple.TCP

mkClientConnector :: (Functor m, MonadReader Socket m, MonadError NetError m, MonadIO m, Binary req, Binary res) => Cofree (NetworkInterpreterF req res m) (m ())
mkClientConnector = coiterT f (Identity (return ()))
  where
    f w = NetworkInterpreterF $ \req -> do
      s <- ask
      send s . L.toStrict . encode $ req
      res <- fmap (decode . L.fromStrict) <$> recv s 1024
      case res of
          Nothing -> throwError Disconnected
          Just x -> return (x, w)

pairClient :: (Functor m, MonadReader Socket m, MonadError NetError m, MonadIO m, ToNetworkClient c m, Binary (ClientReq c), Binary (ClientRes c)) => FreeT c m () -> m ()
pairClient = pairEffectM (\_ r -> return r) mkClientConnector . transFreeT toNetworkClient

runClient :: (MonadIO m, MonadMask m) => HostName -> ServiceName -> ReaderT Socket (ExceptT NetError m) a -> m (Either NetError a)
runClient host service x = connect host service $ \(sock, _) -> runExceptT . flip runReaderT sock $ x

