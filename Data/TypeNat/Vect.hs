{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.TypeNat.Vect (

    Vect(..)

  , vectToList
  , listToVect
  , showVect

  , module Data.TypeNat.Nat

  ) where

import Data.TypeNat.Nat

data Vect :: * -> Nat -> * where
  VNil :: Vect a Z
  VCons :: a -> Vect a n -> Vect a (S n)

showVect :: Show a => Vect a l -> String
showVect VNil = "VNil"
showVect (VCons x xs) = show x ++ " , " ++ showVect xs

vectToList :: Vect a n -> [a]
vectToList v = case v of
  VNil -> []
  VCons x xs -> x : vectToList xs

newtype MaybeVect a n = MV {
    unMV :: Maybe (Vect a n)
  }

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
