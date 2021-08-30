{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Control.Monad
import           Data.Monoid
import           GHC.Generics

import           SumTotality.NArity

main :: IO ()
main = putStrLn "Hello, Haskell!"

data T = A Bool
       | B String Int Double
       | C ()
       | D Int
       | E
  deriving (Generic, Show)

buildTIO :: IO T
buildTIO
  = getAlt
  . total
  . add A (Alt $ fail "whoops")
  . add B (Alt $ pure ("test", (3, 2.3)))
  . add C (Alt $ pure ())
  . add D (Alt $ pure 23)
  . add E (Alt $ pure NoData)
  $ end

intToT :: Int -> Maybe T
intToT i
  = getAlt
  . total
  . add A (Alt $ True <$ guard (i == 1))
  . add B (Alt $ ("blah", (2, 4.1)) <$ guard (i == 2))
  . add C (Alt $ () <$ guard (i == 3))
  . add D (Alt $ 8 <$ guard (i == 4))
  $ end1 E (Alt $ pure NoData)
