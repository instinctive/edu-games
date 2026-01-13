module Total where

import Data.Array.Base
import Data.Array.IArray
import GHC.Ix ( unsafeIndex )

#define ENUM(a) Eq a, Ord a, Enum a, Bounded a, Ix a, Show a

type TOTAL a i e = (IArray a e, ENUM(i))

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

newtype TotalArray arr i e = TotalArray (arr i e)

totalAt :: TOTAL a i e => i -> Lens' (TotalArray a i e) e
totalAt i f (TotalArray a) =
    let n = unsafeIndex (minBound,maxBound) i in
    f (unsafeAt a n) <&> \e -> TotalArray (unsafeReplace a [(n,e)])

totalArray :: TOTAL a i e => (i -> e) -> TotalArray a i e
totalArray f = TotalArray $ listArray (minBound, maxBound) $ f <$> [minBound..maxBound]

(!) :: TOTAL a i e => TotalArray a i e -> i -> e
(!) (TotalArray a) i = unsafeAt a (unsafeIndex (minBound,maxBound) i)

(//) :: TOTAL a i e => TotalArray a i e -> [(i,e)] -> TotalArray a i e
(//) (TotalArray a) l = TotalArray $ unsafeReplace a $ first (unsafeIndex (minBound,maxBound)) <$> l

indices :: TOTAL a i e => TotalArray a i e -> [i]
indices _ = [minBound..maxBound]

elems :: TOTAL a i e => TotalArray a i e -> [e]
elems (TotalArray a) = Data.Array.IArray.elems a

assocs :: TOTAL a i e => TotalArray a i e -> [(i,e)]
assocs (TotalArray a) = zip [minBound..maxBound] (Data.Array.IArray.elems a)
