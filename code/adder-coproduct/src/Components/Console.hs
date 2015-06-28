{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Components.Console (
      Console(..)
    , runConsole
    ) where

import Util.Coproduct

import Data.Proxy (Proxy(..))
import Control.Applicative ((<|>))
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Free (MonadFree)
import Control.Monad.Trans.Free (liftF)

import Text.Parsec.Prim as P (parse)
import Text.Parser.Combinators (try)
import Text.Parser.Char (CharParsing)

class Console f where
  prompt :: Proxy (f ()) -> [String]
  parser :: forall m. (Monad m, CharParsing m) => m (f ())

instance (Console a, Console b) => Console (a :+: b) where
    prompt _ = prompt (Proxy :: Proxy (a ())) ++ prompt (Proxy :: Proxy (b ()))
    parser = try (fmap InL parser) <|> try (fmap InR parser)

runConsole' :: forall f m. (Functor f, MonadIO m, Console f, MonadFree f m) => m ()
runConsole' = do
    l <- liftIO getLine
    either (\_ -> output help) liftF . parse parser "console parser" $ l
  where
    output = liftIO . putStrLn
    help :: String
    help = unlines . ("Commands:" :) . map ("  " ++) $ prompt (Proxy :: Proxy (f ()))

runConsole :: forall f m. (Functor f, MonadIO m, Console f, MonadFree f m) => m ()
runConsole = forever runConsole'
