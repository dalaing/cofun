{-# LANGUAGE FlexibleContexts      #-}
module AdderService.Net.Server (
    runServer
  ) where

import           AdderService.Net.Errors    (NetError (..))
import           AdderService.Net.Functors  (ClientF (..))

import           AdderService.CoAdder.Class (mkCoAdder)

import           Util.Pairing               (pairEffectM)

import           Control.Applicative        ((<$>))
import           Control.Monad              (forever)
import           Control.Monad.Except       (MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask, runReaderT)
import           Control.Monad.Trans.Free   (FreeF (..), FreeT (..))
import qualified Data.Foldable              as F (mapM_)

import           Data.Binary                (decode, encode)
import qualified Data.ByteString.Lazy       as L (fromStrict, toStrict)
import           Network.Simple.TCP         (HostName, HostPreference (..),
                                             ServiceName, Socket, recv, send,
                                             serve)

mkClient :: (Functor m, MonadReader Socket m, MonadError NetError m, MonadIO m) => FreeT (ClientF m) m ()
mkClient = do
  s <- ask
  r <- fmap (decode . L.fromStrict) <$> recv s 1024
  case r of
      Nothing -> FreeT $ throwError Disconnected
      Just x -> FreeT . return .
                Free . ClientF x . return .
                F.mapM_ $
                liftIO . send s . L.toStrict . encode

runServer' :: (Functor m, MonadReader Socket m, MonadError NetError m, MonadIO m) => m ()
runServer' = pairEffectM (\_ r -> return r) (mkCoAdder 10 0) (forever mkClient)

runServer :: HostName -> ServiceName -> IO ()
runServer host service = serve (Host host) service $ \(sock, _) -> do
  e <- runExceptT $ runReaderT runServer' sock
  case e of
   Left l -> print l
   Right _ -> return ()
