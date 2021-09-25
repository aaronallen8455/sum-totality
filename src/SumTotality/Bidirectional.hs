{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
module SumTotality.Bidirectional
  ( Aggregator
  , Codec(..)
  , add
  , end
  , end1
  , total
  ) where

import           Unsafe.Coerce (unsafeCoerce)
import qualified Data.Array as A
import           Data.Kind
import           Data.Proxy
import           GHC.Exts
import           GHC.Generics
import           GHC.TypeLits

data Codec e f a =
  MkCodec
    { decode :: f a
    , encode :: a -> e
    }

data Aggregator (tys :: [Type]) (e :: Type) (f :: Type -> Type) sum
  = MkAgg { aggDecode :: f sum, aggEncode :: [Any -> e] }

add :: (Functor f, Semigroup (f sum))
    => (x -> sum)
    -> Codec e f x
    -> Aggregator ts e f sum
    -> Aggregator (x ': ts) e f sum
add con codec (MkAgg dec enc) =
  MkAgg (fmap con (decode codec) <> dec)
        (unsafeCoerce (encode codec) : enc)

end :: Monoid (f sum)
    => Aggregator '[] e f sum
end = MkAgg mempty []

end1 :: Functor f
     => (x -> sum)
     -> Codec e f x
     -> Aggregator '[x] e f sum
end1 con codec = MkAgg (con <$> decode codec) [unsafeCoerce $ encode codec]

total :: ( Generic sum
         , expected ~ ExtractTypes (Rep sum)
         , UnsafeSumIndex (Rep sum)
         , CheckTypes sum expected tys
         )
      => Aggregator tys e f sum -> Codec e f sum
total (MkAgg decoder encList) =
  MkCodec
    decoder
    (apply . unsafeSumIndex . from)
  where
    encoders = A.listArray (0, length encList - 1) encList
    apply (STuple a i) = encoders A.! i $ a

-- Could incrementally check each type so that we know the index of the type
-- mismatch. Would be more efficient and would greatly improve error messages

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

data STuple =
  STuple { sFst :: !Any, sSnd :: {-# UNPACK #-} !Int}

class UnsafeSumIndex (r :: k -> Type) where
  unsafeSumIndex :: r a -> STuple

instance UnsafeSumIndex x => UnsafeSumIndex (D1 m x) where
  unsafeSumIndex (M1 x) = unsafeSumIndex x

instance ( UnsafeSumIndex a
         , UnsafeSumIndex b
         , leftCount ~ CountConstructors a
         , KnownNat leftCount
         ) => UnsafeSumIndex (a :+: b) where
  unsafeSumIndex = \case
    L1 a -> unsafeSumIndex a
    R1 b ->
      let leftCount = fromIntegral $ natVal (Proxy :: Proxy leftCount)
          STuple a i = unsafeSumIndex b
       in STuple a (i + leftCount)

instance UnsafeSumIndex (C1 m1 (S1 m2 (Rec0 ty))) where
  unsafeSumIndex (M1 (M1 (K1 x))) = STuple (unsafeCoerce x) 0

type family CountConstructors x :: Nat where
  CountConstructors (x :+: y) = CountConstructors x + CountConstructors y
  CountConstructors (C1 m1 (S1 m2 (Rec0 ty))) = 1

type family CheckTypes (sum :: Type) (expected :: [Type]) (actual :: [Type]) :: Constraint where
  CheckTypes s (x : xs) (y : ys) = (x ~ y, CheckTypes s xs ys)
  CheckTypes s '[] '[] = ()
  CheckTypes s '[] ys =
    TypeError (Text "There are more branches than constructors of ‘"
          :<>: ShowType s :<>: Text "’"
              )
  CheckTypes s xs '[] =
    TypeError (Text "Missing coverage for constructors of ‘"
          :<>: ShowType s :<>: Text "’"
              )
