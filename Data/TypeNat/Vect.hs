{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

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
data Vect :: Nat -> * -> * where
  VNil :: Vect Z a
  VCons :: a -> Vect n a -> Vect (S n) a

deriving instance Eq a => Eq (Vect n a)
deriving instance Show a => Show (Vect n a)

instance Functor (Vect n) where
    fmap = vectMap

instance Foldable (Vect n) where
    foldr f b vect = case vect of
        VNil -> b
        VCons x rest -> f x (foldr f b rest)

instance Traversable (Vect n) where
    traverse f vect = case vect of
        VNil -> pure VNil
        VCons x rest -> VCons <$> f x <*> traverse f rest

-- | A kind of fmap for Vect.
vectMap :: (a -> b) -> Vect n a -> Vect n b
vectMap f vect = case vect of
  VNil -> VNil
  VCons x v -> VCons (f x) (vectMap f v)

-- | VCons to the end of a Vect.
vectSnoc :: a -> Vect n a -> Vect (S n) a
vectSnoc x vect = case vect of
  VNil -> VCons x VNil
  VCons y v -> VCons y (vectSnoc x v)

-- | Show a Vect.
showVect :: Show a => Vect l a -> String
showVect VNil = "VNil"
showVect (VCons x xs) = show x ++ " , " ++ showVect xs

-- | Drop the length index from a Vect, giving a typical list.
vectToList :: Vect n a -> [a]
vectToList v = case v of
  VNil -> []
  VCons x xs -> x : vectToList xs

-- | Used to implement listToVect through natRecursion.
newtype MaybeVect a n = MV {
    unMV :: Maybe (Vect n a)
  }

-- | Try to produce a Vect from a list. The nat index must be fixed somehow,
--   perhaps with the help of ScopedTypeVariables.
listToVect:: IsNat n => [a] -> Maybe (Vect n a)
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
