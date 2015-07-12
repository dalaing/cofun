{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Add.Functors (
      AddF(..)
    , CoAddF(..)
    ) where

import           Components.Console      (ConsoleClient (..),
                                          ConsoleInterpreter (..))

import           Util.Pairing            (Pairing (..))

import           Text.Parser.Char
import           Text.Parser.Combinators

data AddF k = Add Int (Bool -> k)

instance Functor AddF where
  fmap f (Add x k) = Add x (f . k)

data CoAddF k = CoAdd (Int -> (Bool, k))

instance Functor CoAddF where
  fmap f (CoAdd a) = CoAdd (fmap (fmap f) a)

instance Pairing CoAddF AddF where
    pair f (CoAdd a) (Add x k) = pair f (a x) k

instance ConsoleClient AddF where
    prompt _ = ["add (int)"]
    parser =
      string "add" >>
      space >>
      many digit >>= \xs ->
        return $ Add (read xs) (const ())

instance ConsoleInterpreter CoAddF where
  addResultLogging (CoAdd f) = CoAdd (fmap (\(b, k) -> (b, putStrLn ("add result: " ++ show b) <$ k)) f)

