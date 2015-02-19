Data.TypeNat
============

The classic first example of a dependently typed program is the list of
fixed length, or `Vect`, where the length is in its type.

It's known that GHC's type system can handle this, through the
GADTs, KindSignatures, and DataKinds extensions.

This module provides the fixed-length list type `Vect a Nat`, as well as the
`Nat` definitions which it demands. Through the use of a type-directed
recursion in the `IsNat` class, we are able to provide the function
`listToVect :: IsNat n => [a] -> Maybe (Vect a n)`; we did not require
a special class just for the implementation of this function!

Here is an example use:

```Haskell
import Data.TypeNat.Vect
import Data.TypeNat.Fin

-- Typechecks
myVector :: Vect String Three
myVector = VCons "Static" (VCons "Types" (VCons "Rule" VNil))

-- Does not typecheck
myBadVector :: Vect String Four
myBadVector = VCons "Static" (VCons "Types" (VCons "Rule" VNil))

-- Typechecks
myVectorAsList :: [String]
myVectorAsList = vectToList myVector

-- Typechecks, is Just
myVectorAgain :: Maybe (Vect String Three)
myVectorAgain = listToVect myVectorAsList

-- Typechecks, is Nothing
myVectorAgainBad :: Maybe (Vect String Four)
myVectorAgainBad = listToVect myVectorAsList

-- Typechecks
mySecondElement :: String
mySecondElement = safeIndex myVector ix2

-- Does not typecheck
myFourthElement :: String
myFourthElement = safeIndex myVector ix4

main = do

  putStrLn $ showVect myVector

  -- Prints the list, since 'vectToList myVector' has length 3 and we gave
  -- the type Three in our annotation.
  case (listToVect (vectToList myVector) :: Maybe (Vect String Three)) of
    Nothing -> putStrLn "Nothing"
    Just v -> putStrLn $ showVect v

  -- Prints "Nothing", since 'vectToList myVector' has length 3 and we gave
  -- the type Two in our annotation.
  case (listToVect (vectToList myVector) :: Maybe (Vect String Two)) of
    Nothing -> putStrLn "Nothing"
    Just v -> putStrLn $ showVect v
```
