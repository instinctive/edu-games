module MCT where

import Control.Lens
import Control.Monad.Random.Class
import Data.Array.Unboxed

import Game

data Tree a = Tree a [Tree a] deriving (Show, Functor)

data MCT p = MCT
    -- _mctId    :: Int  -- for psqueue?
    { _mctWins  :: UArray p Int
    , _mctPlays :: Int
    } deriving Show

mctWins :: Ix p => p -> Lens' (MCT p) Int
mctWins p f mct = f (_mctWins mct ! p) <&> \wins -> mct { _mctWins = _mctWins mct // [(p,wins)] }

mctPlays :: Lens' (MCT p) Int
mctPlays f mct = f (_mctPlays mct) <&> \plays -> mct { _mctPlays = plays }

pickOne :: MonadRandom m => [a] -> m a
pickOne l = getRandomR (0, length l - 1) <&> (l!!)

rollout g
    | gameStatus g == Play = pickOne (nextGames g) >>= rollout
    | otherwise = pure g

mkTree g = Tree g (mkTree <$> nextGames g)

rolloutTree (Tree g []) = pure g
rolloutTree (Tree _ tt) = pickOne tt >>= rolloutTree
