{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.TypeNat.Vect (

    Vect(..)

  , vectMap
  , vectSnoc
  , vectToList
  , listToVect
  , showVect

  , module Data.TypeNat.Nat

  ) where

import Data.TypeNat.Nat

-- | Nat-indexed list, where the nat determines the length.
data Vect :: * -> Nat -> * where
  VNil :: Vect a Z
  VCons :: a -> Vect a n -> Vect a (S n)

-- | A kind of fmap for Vect.
vectMap :: (a -> b) -> Vect a n -> Vect b n
vectMap f vect = case vect of
  VNil -> VNil
  VCons x v -> VCons (f x) (vectMap f v)

-- | VCons to the end of a Vect.
vectSnoc :: a -> Vect a n -> Vect a (S n)
vectSnoc x vect = case vect of
  VNil -> VCons x VNil
  VCons y v -> VCons y (vectSnoc x v)

showVect :: Show a => Vect a l -> String
showVect VNil = "VNil"
showVect (VCons x xs) = show x ++ " , " ++ showVect xs

-- | Drop the length index from a Vect, giving a typical list.
vectToList :: Vect a n -> [a]
vectToList v = case v of
  VNil -> []
  VCons x xs -> x : vectToList xs

-- | Used to implement listToVect through natRecursion.
newtype MaybeVect a n = MV {
    unMV :: Maybe (Vect a n)
  }

-- | Try to produce a Vect from a list. The nat index must be fixed somehow,
--   perhaps with the help of ScopedTypeVariables.
listToVect:: IsNat n => [a] -> Maybe (Vect a n)
listToVect = unMV . listToVect'

  where

    listToVect':: IsNat n => [a] -> MaybeVect a n
    listToVect' = natRecursion inductive base reduce
    
    inductive :: [a] -> MaybeVect a m -> MaybeVect a (S m)
    inductive xs maybeVect = case maybeVect of
      MV Nothing -> MV Nothing
      MV (Just vect) -> case xs of 
        [] -> MV Nothing
        (x : _) -> MV (Just (VCons x vect))

    base :: [a] -> MaybeVect a Z
    base xs = case xs of
      [] -> MV (Just VNil)
      _ -> MV Nothing

    reduce :: [a] -> [a]
    reduce xs = case xs of
      [] -> []
      (x : xs) -> xs
