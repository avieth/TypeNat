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

  , module Data.TypeNat.Nat

  ) where

import Data.TypeNat.Nat
import Data.TypeNat.Vect

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

safeIndex :: Vect a n -> Fin n -> a
safeIndex (VCons a _) FZ = a
safeIndex (VCons _ v) (FS x) = safeIndex v x
