{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module AdderService.Functors (
    AdderF(..)
  , CoAdderF(..)
  ) where

import           AdderService.Packets (AdderReq(..), AdderRes(..))

import           Util.Pairing (Pairing (..))
import           Util.Console (ConsoleClient(..), ConsoleInterpreter(..))
import           Util.Network (ToNetworkClient(..), ToNetworkInterpreter(..))
import           Util.Network.Functors (NetworkClientF(..), NetworkInterpreterF(..))
import           Util.Network.Errors (NetError(..))

import           Control.Applicative
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.IO.Class (liftIO)
import           Text.Parser.Char
import           Text.Parser.Combinators

data AdderF k =
    Add Int (Bool -> k)
  | Clear k
  | Total (Int -> k)

instance Functor AdderF where
  fmap f (Add x k) = Add x (f . k)
  fmap f (Clear k) = Clear (f k)
  fmap f (Total k) = Total (f . k)

data CoAdderF k = CoAdderF {
    addH   :: Int -> (Bool,k)
  , clearH :: k
  , totalH :: (Int,k)
  }

instance Functor CoAdderF where
  fmap f (CoAdderF a c t) =
    CoAdderF
      (fmap (fmap f) a)
      (f c)
      (fmap f t)

instance Pairing CoAdderF AdderF where
  pair f (CoAdderF a _ _) (Add x k) = pair f (a x) k
  pair f (CoAdderF _ c _) (Clear k) = f c k
  pair f (CoAdderF _ _ t) (Total k) = pair f t k

instance Pairing AdderF CoAdderF where
  pair p c s = pair (flip p) s c

instance ConsoleClient AdderF where
  prompt _ = ["add (int)", "clear", "total"]
  parser = try addParser <|> try clearParser <|> totalParser 
    where
      addParser = string "add" >> space >> many digit >>= \xs -> return $ Add (read xs) (const ())
      clearParser = Clear () <$ string "clear"
      totalParser = Total (const ()) <$ string "total"

  addOutput (Add x k) = Add x $ \b -> do
    liftIO $ putStrLn ("add result: " ++ show b)
    k b
  addOutput (Clear k) = Clear k
  addOutput (Total k) = Total $ \i -> do
    liftIO $ putStrLn ("total result: " ++ show i)
    k i

instance ConsoleInterpreter CoAdderF where
  addResultLogging (CoAdderF a c t) = CoAdderF a' c' t'
    where
      a' x =
        let
          (b, k) = a x
        in
          (b, (liftIO $ putStrLn ("add result: " ++ show b)) <$ k)
      c' = return () <$ c
      t' =
        let
          (i, k) = t
        in
          (i, (liftIO $ putStrLn ("total result: " ++ show i)) <$ k)

instance (Monad m, MonadError NetError m) => ToNetworkClient m AdderF where
  type ClientReq AdderF = AdderReq
  type ClientRes AdderF = AdderRes
  toNetworkClient (Add x f) = NetworkClientF (AddReq x, g)
    where 
      g (AddRes b) = return $ f b
      g _ = throwError UnexpectedResponse
  toNetworkClient (Clear k) = NetworkClientF (ClearReq, g)
    where
      g ClearRes = return k
      g _ = throwError UnexpectedResponse
  toNetworkClient (Total f) = NetworkClientF (TotalReq, g)
    where
      g (TotalRes i) = return $ f i
      g _ = throwError UnexpectedResponse

instance Monad m => ToNetworkInterpreter m CoAdderF where
  type InterpreterReq CoAdderF = AdderReq
  type InterpreterRes CoAdderF = AdderRes
  toNetworkInterpreter (CoAdderF a c t) = NetworkInterpreterF $ \rq -> case rq of
      AddReq i -> let 
                    (b, k) = a i
                  in return (AddRes b, k)
      ClearReq -> return (ClearRes, c)
      TotalReq -> let
                    (i, k) = t
                  in return (TotalRes i, k)
