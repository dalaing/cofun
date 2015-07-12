{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Components.Console (
      ConsoleClient(..)
    , ConsoleInterpreter(..)
    , runConsole
    ) where

import           Util.Coproduct

import           Control.Applicative      ((<|>))
import           Control.Monad            (forever)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Trans.Free (FreeT (..), liftF)
import           Data.Proxy               (Proxy (..))

import           Text.Parsec.Prim         as P (parse)
import           Text.Parser.Char         (CharParsing)
import           Text.Parser.Combinators  (try)

class ConsoleClient f where
  prompt :: Proxy (f ()) -> [String]
  parser :: (Monad m, CharParsing m) => m (f ())

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
    help = unlines .
      ("Commands:" :) .
      map ("  " ++) $
      prompt (Proxy :: Proxy (f ()))

runConsole :: forall f m. (Functor f, MonadIO m, ConsoleClient f, Monad m) => FreeT f m ()
runConsole = forever runConsole'

class ConsoleInterpreter f where
  addResultLogging :: Functor g => f (g a) -> f (g (IO ()))

instance (ConsoleInterpreter a, ConsoleInterpreter b) => ConsoleInterpreter (a :*: b) where
  addResultLogging (a :*: b) = addResultLogging a :*: addResultLogging b

