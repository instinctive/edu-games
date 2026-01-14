{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module MCT where

import Control.Lens
import Control.Monad.Random.Class

import Total
import Game

-- #define ENUM(a) Eq a, Ord a, Bounded a, Enum a, Ix a, Show a

type Reward g = UArray (Player g) Double

data MCT g = MCT
    { _mctMove    :: Move g
    , _mctWins    :: Reward g
    , _mctPlays   :: Double
    , _mctPlayed  :: [MCT g]
    , _mctWaiting :: [g]
    }
makeLenses ''MCT

deriving instance (Show g, Show (Move g), Show (Player g), Ix (Player g)) => Show (MCT g)

pickOne l = getRandomR (0, length l - 1) <&> (l!!)

rollout (g :: g) = case gameStatus g of
    Play  -> pickOne (nextGames g) >>= rollout
    Draw  -> pure $ totalArray (const $ 1 / fromIntegral (numPlayers @(Player g)))
    Win p -> pure $ totalArray (const 0) & set (totalAt p) 1

expand g = rollout g <&> \reward ->
    (reward, MCT
        { _mctMove = lastMove g
        , _mctWins = reward
        , _mctPlays = 1
        , _mctPlayed = []
        , _mctWaiting = nextGames g
        })
