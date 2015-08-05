{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TypeNat.Nat (

    Nat(..)
  , IsNat(..)

  , LTE(..)
  , StrongLTE

  , Zero
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

import Data.Proxy
import GHC.Exts (Constraint)

data Nat = Z | S Nat

instance Eq Nat where
    Z == Z = True
    (S n) == (S m) = n == m
    _ == _ = False

instance Ord Nat where
    Z `compare` Z = EQ
    S n `compare` S m = n `compare` m
    S n `compare` Z = GT
    Z `compare` S m = LT

type Zero = Z
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

-- | A constrint which includes LTE k m for every k <= m.
type family StrongLTE (n :: Nat) (m :: Nat) :: Constraint where
  StrongLTE Z m = LTE Z m
  StrongLTE (S n) m = (LTE (S n) m, StrongLTE n m)

-- | Nat @n@ is less than or equal to nat @m@.
--   Comes with functions to do type-directed computation for Nat-indexed
--   datatypes.
class LTE (n :: Nat) (m :: Nat) where
  lteInduction
    :: StrongLTE m l
    => Proxy l
    -> (forall k . LTE (S k) l => d k -> d (S k))
    -- ^ The parameter l is fixed by any call to lteInduction, but due to
    --   the StrongLTE m l constraint, we have LTE j l for every j <= m.
    --   This allows us to implement the nontrivial case in the
    --     @LTE p q => LTE p (S q)@
    --   instance, where we need to use this function to get @x :: d p@ and then
    --   again to get @f x :: d (S p)@. So long as @p@ and @S p@ are both
    --   less or equal to @l@, this can be done.
    -> d n
    -> d m
  lteRecursion :: (forall k . LTE n k => d (S k) -> d k) -> d m -> d n

instance LTE n n where
  lteInduction _ f x = x
  lteRecursion f x = x

instance LTE n m => LTE n (S m) where
  lteInduction (proxy :: Proxy l) f (x :: d n) = f (lteInduction proxy f x)
  lteRecursion f x = lteRecursion f (f x)
