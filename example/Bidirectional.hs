{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
module Bidirectional where

import           Data.Functor.Compose
import           Data.Monoid
import           GHC.Generics
import           Text.Read

import           SumTotality.Bidirectional

data T = W Bool
       | X Int
       | Y ()
       | Z Double
       deriving (Show, Generic)

tC :: Codec String Parser T
tC = total
   . add W readShowCodec
   . add X readShowCodec
   . add Y readShowCodec
   . add Z readShowCodec
   $ end

readShowCodec :: (Read a, Show a) => Codec String Parser a
readShowCodec = MkCodec (Parser readMaybe) show

newtype Parser a = Parser { runParser :: String -> Maybe a }
  deriving stock Functor
  deriving (Semigroup, Monoid) via (String -> Alt Maybe a)
