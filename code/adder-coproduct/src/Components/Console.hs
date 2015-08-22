{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Components.Console (
      ConsoleClient(..)
    , ConsoleInterpreter(..)
    , runConsole
    ) where

import           Util.Coproduct (SumF(..), ProductF(..), NotIn)

import           Control.Applicative      ((<|>))
import           Control.Monad            (forever)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Trans.Free (FreeT (..), liftF)
import           Data.Proxy               (Proxy (..))

import           Text.Parsec.Prim         as P (parse)
import           Text.Parser.Char         (CharParsing)
import           Text.Parser.Combinators  (try)

class ConsoleClient (f :: * -> *) where
  prompt :: Proxy f -> [String]
  parser :: (Monad m, CharParsing m) => m (f ())

instance (Functor a, ConsoleClient a) => ConsoleClient (SumF (a ': '[])) where
    prompt _ = prompt (Proxy :: Proxy a)
    parser = try (fmap SAdd parser)

instance (Functor a, ConsoleClient a, NotIn a (b ': c), ConsoleClient (SumF (b ': c))) => ConsoleClient (SumF (a ': (b ': c))) where
    prompt _ =
      prompt (Proxy :: Proxy a) ++
      prompt (Proxy :: Proxy (SumF (b ': c)))
    parser = try (fmap SAdd parser) <|> fmap SNext parser

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

runConsole :: forall f m. (Functor f, MonadIO m, ConsoleClient f, Monad m) => FreeT f m ()
runConsole = forever runConsole'

class ConsoleInterpreter f where
  addResultLogging :: Functor g => f (g a) -> f (g (IO ()))

-- instance ConsoleInterpreter (ProductF '[]) where
--  addResultLogging p = fmap ((return ()) <$) p

instance (ConsoleInterpreter a) => ConsoleInterpreter (ProductF (a ': '[])) where
  addResultLogging (POne a) = POne (addResultLogging a)

instance (ConsoleInterpreter a, ConsoleInterpreter (ProductF (b ': c))) => ConsoleInterpreter (ProductF (a ': (b ': c))) where
  addResultLogging (PAdd a b) = PAdd (addResultLogging a) (addResultLogging b) 

