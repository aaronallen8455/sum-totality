{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module SumTotality
  ( Aggregator
  , add
  , end
  , end1
  , total
  ) where

import           Data.Kind
import           GHC.Generics
import           GHC.TypeLits

newtype Aggregator (tys :: [Type]) (f :: Type -> Type) sum
  = MkAgg { unAgg :: f sum }

add :: (Functor f, Semigroup (f sum))
    => (x -> sum)
    -> f x
    -> Aggregator ts f sum
    -> Aggregator (x ': ts) f sum
add con fx (MkAgg rest) =
  MkAgg $ fmap con fx <> rest

end :: Monoid (f sum) => Aggregator '[] f sum
end = MkAgg mempty

end1 :: Functor f => (x -> sum) -> f x -> Aggregator '[x] f sum
end1 con fx = MkAgg $ con <$> fx

total :: (Generic sum, tys ~ ExtractTypes (Rep sum))
      => Aggregator tys f sum -> f sum
total = unAgg

type family ExtractTypes (x :: k -> Type) :: [Type] where
  ExtractTypes (D1 metaData con) = ConTypes con
  ExtractTypes other = TypeError (Text "Not a valid sum type.")

type family ConTypes (x :: k -> Type) :: [Type] where
  ConTypes (c1 :+: c2) = ConTypes c1 `Append` ConTypes c2
  ConTypes (C1 metaCons (S1 metaSel (Rec0 ty))) = '[ty]
  ConTypes other = TypeError (Text "Not a valid sum type.")

type family Append (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Append (x ': xs) ys = x ': Append xs ys
  Append '[] ys = ys
