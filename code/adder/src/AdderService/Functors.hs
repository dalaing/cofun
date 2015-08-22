{-# LANGUAGE MultiParamTypeClasses #-}
module AdderService.Functors (
    AdderF(..)
  , CoAdderF(..)
  ) where

import           Util.Pairing (Pairing (..))
import           Util.Console (ConsoleClient(..), ConsoleInterpreter(..))

import           Control.Applicative
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

instance ConsoleInterpreter CoAdderF where
  addResultLogging (CoAdderF a c t) = CoAdderF a' c' t'
    where
      a' x =
        let
          (b, k) = a x
        in
          (b, putStrLn ("add result: " ++ show b) <$ k)
      c' = return () <$ c
      t' =
        let
          (i, k) = t
        in
          (i, putStrLn ("total result: " ++ show i) <$ k)
