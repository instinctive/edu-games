module Total where

import Data.Array.Base
import Data.Array.IArray
import Data.Array.IArray qualified as I
import Data.Array.Unboxed qualified as U
import GHC.Ix ( unsafeIndex )

#define ENUM(a) Eq a, Ord a, Enum a, Bounded a, Ix a, Show a

type TOTAL a i e = (IArray a e, ENUM(i))

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

type Array = I.Array
type UArray = U.UArray

totalAt :: TOTAL a i e => i -> Lens' (a i e) e
totalAt i f a =
    let n = unsafeIndex (minBound,maxBound) i in
    f (unsafeAt a n) <&> \e -> unsafeReplace a [(n,e)]

totalArray :: TOTAL a i e => (i -> e) -> a i e
totalArray f = listArray (minBound, maxBound) $ f <$> [minBound..maxBound]

(!) :: TOTAL a i e => a i e -> i -> e
(!) a i = unsafeAt a (unsafeIndex (minBound,maxBound) i)

(//) :: TOTAL a i e => a i e -> [(i,e)] -> a i e
(//) a l = unsafeReplace a $ first (unsafeIndex (minBound,maxBound)) <$> l

indices :: TOTAL a i e => a i e -> [i]
indices _ = [minBound..maxBound]

elems :: TOTAL a i e => a i e -> [e]
elems a = I.elems a

assocs :: TOTAL a i e => a i e -> [(i,e)]
assocs a = zip [minBound..maxBound] (I.elems a)
