{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module Data.TypeNat.Nat (

    Nat(..)
  , IsNat
  , natRecursion

  , One
  , Two
  , Three
  , Four
  , Five
  , Six
  , Seven
  , Eight
  , Nine
  , Ten

  ) where

data Nat = Z | S Nat

type One = S Z
type Two = S One
type Three = S Two
type Four = S Three
type Five = S Four
type Six = S Five
type Seven = S Six
type Eight = S Seven
type Nine = S Eight
type Ten = S Nine

-- | Proof that a given type is a Nat.
--   With this fact, you can do type-directed computation.
class IsNat (n :: Nat) where
  natRecursion :: (forall m . b -> a m -> a (S m)) -> (b -> a Z) -> (b -> b) -> b -> a n

instance IsNat Z where
  natRecursion _ ifZ _ = ifZ

instance IsNat n => IsNat (S n) where
  natRecursion ifS ifZ reduce x = ifS x (natRecursion ifS ifZ reduce (reduce x))
