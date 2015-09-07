{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Util.Network.Interpreter (
      mkInterpreterConnector
    , pairInterpreter
    , runInterpreter
    ) where

import Util.Network
import Util.Network.Functors
import Util.Network.Errors

import Util.Pairing

import Control.Comonad
import Control.Comonad.Trans.Cofree
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Free
import Data.Functor.Identity

import Data.Binary
import qualified Data.ByteString.Lazy as L
import Network.Simple.TCP

mkInterpreterConnector :: (Functor m, MonadReader Socket m, MonadError NetError m, MonadIO m, Binary req, Binary res) => FreeT (NetworkClientF req res m) m ()
mkInterpreterConnector = do
  s <- ask
  r <- fmap (decode . L.fromStrict) <$> recv s 1024
  case r of
    Nothing -> FreeT $ throwError Disconnected
    Just x -> FreeT . return . Free . NetworkClientF $ (x, \t -> do
          _ <- liftIO . send s . L.toStrict . encode $ t
          return mkInterpreterConnector
        )

transCofreeF :: (forall x. f x -> g x) -> CofreeF f a b -> CofreeF g a b
transCofreeF t (a :< fb) = a :< t fb

transCofreeT :: (Functor g, Comonad w) => (forall x. f x -> g x) -> CofreeT f w a -> CofreeT g w a
transCofreeT t = CofreeT . liftW (fmap (transCofreeT t) . transCofreeF t) . runCofreeT

pairInterpreter :: (Functor m, Comonad w, MonadReader Socket m, MonadError NetError m, MonadIO m, ToNetworkInterpreter i m, Binary (InterpreterReq i), Binary (InterpreterRes i)) => CofreeT i w (m ()) -> m ()
pairInterpreter server = pairEffectM (\_ r -> return r) (transCofreeT toNetworkInterpreter server) mkInterpreterConnector

runInterpreter :: HostName -> ServiceName -> ReaderT Socket (ExceptT NetError IO) () -> IO ()
runInterpreter host service x =
  serve (Host host) service $ \(sock, _) ->
     void . runExceptT . flip runReaderT sock $ x

