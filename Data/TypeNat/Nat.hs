{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- TBD can we get around incoherent instances!?!
{-# LANGUAGE IncoherentInstances #-}

module Data.TypeNat.Nat (

    Nat(..)
  , IsNat
  , natRecursion

  , LTE
  , lteInduction
  , lteRecursion

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

-- | Nat @n@ is less than or equal to nat @m@.
--   Comes with functions to do type-directed computation for Nat-indexed
--   datatypes.
class LTE (n :: Nat) (m :: Nat) where
  lteInduction :: (forall k . LTE (S k) m => d k -> d (S k)) -> d n -> d m
  lteRecursion :: (forall k . LTE n k => d (S k) -> d k) -> d m -> d n

instance LTE n n where
  lteInduction f x = x
  lteRecursion f x = x

instance LTE n m => LTE n (S m) where

  lteInduction
    :: forall (d :: Nat -> *) .
       (forall (k :: Nat) . LTE (S k) (S m) => d k -> d (S k))
    -> d n
    -> d (S m)
  lteInduction f x =
      let sub :: d m
          sub = lteInduction f x
      in  f sub

  lteRecursion f x = lteRecursion f (f x)
