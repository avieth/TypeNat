{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.TypeNat.Fin (

    Fin(..)

  , ix1
  , ix2
  , ix3
  , ix4
  , ix5
  , ix6
  , ix7
  , ix8
  , ix9
  , ix10

  , safeIndex
  , safeUpdate

  , module Data.TypeNat.Nat

  ) where

import Data.TypeNat.Nat
import Data.TypeNat.Vect

-- | Finite set datatype.
data Fin :: Nat -> * where
  FZ :: Fin (S n)
  FS :: Fin k -> Fin (S k)

ix1 = FZ
ix2 = FS ix1
ix3 = FS ix2
ix4 = FS ix3
ix5 = FS ix4
ix6 = FS ix5
ix7 = FS ix6
ix8 = FS ix7
ix9 = FS ix8
ix10 = FS ix9

-- | Safely index a Vect.
safeIndex :: Fin n -> Vect n a -> a
safeIndex FZ (VCons a _) = a
safeIndex (FS x) (VCons _ v) = safeIndex x v

safeUpdate :: Fin n -> (a -> a) -> Vect n a -> Vect n a
safeUpdate FZ f (VCons x rest) = VCons (f x) rest
safeUpdate (FS fs) f (VCons x rest) = VCons x (safeUpdate fs f rest)
