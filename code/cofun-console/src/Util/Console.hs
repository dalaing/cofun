{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Util.Console (
      ConsoleClient(..)
    , ConsoleInterpreter(..)
    , ConsoleLogging(..)
    , runConsole
    ) where

import           Util.Coproduct (Sum(..), Product(..), NotIn)

import           Control.Applicative      ((<|>))
import           Control.Monad            (forever)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Trans.Free (FreeT (..), FreeF(..), liftF)
import           Data.Proxy               (Proxy (..))

import           Text.Parsec.Prim         as P (parse)
import           Text.Parser.Char         (CharParsing)
import           Text.Parser.Combinators  (try)

class ConsoleClient (f :: * -> *) where
  prompt :: Proxy f -> [String]
  parser :: (Monad m, CharParsing m) => m (f ())
  addOutput :: MonadIO m => f (m a) -> f (m a)

instance (Functor a, ConsoleClient a) => ConsoleClient (Sum (a ': '[])) where
  prompt _ = prompt (Proxy :: Proxy a)
  parser = try (fmap SAdd parser)
  addOutput (SAdd h) = SAdd (addOutput h)

instance (Functor a, ConsoleClient a, NotIn a (b ': c), ConsoleClient (Sum (b ': c))) => ConsoleClient (Sum (a ': (b ': c))) where
  prompt _ =
    prompt (Proxy :: Proxy a) ++
    prompt (Proxy :: Proxy (Sum (b ': c)))

  parser = try (fmap SAdd parser) <|> fmap SNext parser

  addOutput (SAdd h)  = SAdd (addOutput h)
  addOutput (SNext t) = SNext (addOutput t)

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
      prompt (Proxy :: Proxy f)

addConsoleOutput :: (Functor f, ConsoleClient f, MonadIO m) => FreeT f m a -> FreeT f m a
addConsoleOutput = FreeT . fmap f . runFreeT
  where
    f (Pure x) = Pure x
    f (Free y) = Free (addOutput . fmap addConsoleOutput $ y)

data ConsoleLogging = WithLogging | WithoutLogging

runConsole :: forall f m. (Functor f, MonadIO m, ConsoleClient f, Monad m) => ConsoleLogging -> FreeT f m ()
runConsole cl = forever . f cl $ runConsole'
  where
    f WithLogging    = addConsoleOutput
    f WithoutLogging = id

class ConsoleInterpreter f where
  addResultLogging :: (Functor g, MonadIO m) => f (g a) -> f (g (m ()))

instance (ConsoleInterpreter a) => ConsoleInterpreter (Product (a ': '[])) where
  addResultLogging (POne a) = POne (addResultLogging a)

instance (ConsoleInterpreter a, ConsoleInterpreter (Product (b ': c))) => ConsoleInterpreter (Product (a ': (b ': c))) where
  addResultLogging (PMult a b) = PMult (addResultLogging a) (addResultLogging b) 

