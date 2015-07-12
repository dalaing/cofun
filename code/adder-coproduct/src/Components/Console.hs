{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Components.Console (
      ConsoleClient(..)
    , ConsoleServer(..)
    , runConsole
    ) where

import Util.Coproduct

import Data.Proxy (Proxy(..))
import Control.Applicative ((<|>))
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Free (FreeT(..), liftF)
import Control.Comonad (Comonad(..))
import Control.Comonad.Cofree (ComonadCofree(..))
import Control.Comonad.Trans.Class (lower)
import Control.Comonad.Trans.Cofree (CofreeT(..), CofreeF(..))

import Text.Parsec.Prim as P (parse)
import Text.Parser.Combinators (try)
import Text.Parser.Char (CharParsing)

class ConsoleClient f where
  prompt :: Proxy (f ()) -> [String]
  parser :: forall m. (Monad m, CharParsing m) => m (f ())

instance (ConsoleClient a, ConsoleClient b) => ConsoleClient (a :+: b) where
    prompt _ = 
      prompt (Proxy :: Proxy (a ())) ++
      prompt (Proxy :: Proxy (b ()))
    parser = try (fmap InL parser) <|> fmap InR parser

runConsole' :: forall f m. (Functor f, MonadIO m, ConsoleClient f, Monad m) => FreeT f m ()
runConsole' = 
    liftIO getLine >>=
        either (\_ -> output help) liftF . 
        parse parser "console parser"
  where
    output = liftIO . putStrLn
    help :: String
    help = unlines .
      ("Commands:" :) . 
      map ("  " ++) $ 
      prompt (Proxy :: Proxy (f ()))

runConsole :: forall f m. (Functor f, MonadIO m, ConsoleClient f, Monad m) => FreeT f m ()
runConsole = forever runConsole'

class ConsoleServer f where
  addResultLogging :: forall g a. Functor g => f (g a) -> f (g (IO ()))

instance (ConsoleServer a, ConsoleServer b) => ConsoleServer (a :*: b) where
  addResultLogging (a :*: b) = addResultLogging a :*: addResultLogging b

