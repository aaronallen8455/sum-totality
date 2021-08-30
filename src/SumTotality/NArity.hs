{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module SumTotality.NArity
  ( Aggregator
  , add
  , end
  , end1
  , total
  , Args
  , NoData(..)
  ) where

import           Data.Kind
import           GHC.Generics
import           GHC.TypeLits

newtype Aggregator (tys :: [Type]) (f :: Type -> Type) sum
  = MkAgg { unAgg :: f sum }

add :: forall f sum con numArgs argsTuple ts argsList.
       ( Functor f
       , Semigroup (f sum)
       , Codomain con ~ sum
       , ArgsTuple con ~ argsTuple
       , TupleToList argsTuple ~ argsList
       , CountArgs con ~ numArgs
       , Apply numArgs con argsTuple
       )
    => con
    -> f argsTuple
    -> Aggregator ts f sum
    -> Aggregator (Args argsList ': ts) f sum
add con args (MkAgg rest) =
  MkAgg $ (apply @numArgs con <$> args) <> rest

data Args (args :: [Type])

end :: Monoid (f sum) => Aggregator '[] f sum
end = MkAgg mempty

end1 :: forall f sum con numArgs argsTuple ts argsList.
        ( Functor f
        , Codomain con ~ sum
        , ArgsTuple con ~ argsTuple
        , TupleToList argsTuple ~ argsList
        , CountArgs con ~ numArgs
        , Apply numArgs con argsTuple
        )
     => con -> f argsTuple -> Aggregator '[Args argsList] f sum
end1 con fx = MkAgg $ apply @numArgs con <$> fx

total :: ( Generic sum
         , tys ~ ExtractTypes (Rep sum)
         )
      => Aggregator tys f sum -> f sum
total = unAgg

type family ExtractTypes (x :: k -> Type) :: [Type] where
  ExtractTypes (D1 metaData con) = ConTypes con
  ExtractTypes other = TypeError (Text "Not a valid sum type.")

type family ConTypes (x :: k -> Type) :: [Type] where
  ConTypes (c1 :+: c2) = ConTypes c1 `Append` ConTypes c2
  ConTypes (C1 metaCons (S1 metaSel (Rec0 ty))) = '[Args '[ty]]
  ConTypes (C1 metaCons (s1 :*: s2)) = '[Args (ProductTypes (s1 :*: s2))]
  ConTypes (C1 metaCons U1) = '[Args '[]]
  ConTypes other = TypeError (Text "Not a valid sum type.")

type family ProductTypes (x :: k -> Type) :: [Type] where
  ProductTypes (s1 :*: s2) = ProductTypes s1 `Append` ProductTypes s2
  ProductTypes (S1 metaSel (Rec0 x)) = '[x]
  ProductTypes other = TypeError (Text "Expected arguments")

type family Append (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Append (x ': xs) ys = x ': Append xs ys
  Append '[] ys = ys

type family Codomain (f :: Type) :: Type where
  Codomain (a -> b) = Codomain b
  Codomain a = a

data NoData = NoData

type family ArgsTuple (f :: Type) :: Type where
  ArgsTuple (a -> b -> c) = (a, ArgsTuple (b -> c))
  ArgsTuple (a -> b) = a
  ArgsTuple a = NoData

data N = S N | Z

type family CountArgs (f :: Type) :: N where
  CountArgs (a -> b) = 'S (CountArgs b)
  CountArgs a = Z

class Apply (z :: N) f args where
  apply :: f -> args -> Codomain f

instance Codomain b ~ b => Apply ('S 'Z) (a -> b) a where
  apply = ($)

instance Apply n f rest => Apply ('S n) (a -> f) (a, rest) where
  apply f (a, rest) = apply @n (f a) rest

instance Codomain a ~ a => Apply 'Z a NoData where
  apply a NoData = a

type family TupleToList (x :: Type) :: [Type] where
  TupleToList NoData = '[]
  TupleToList (a, b) = a ': TupleToList b
  TupleToList a = '[a]
