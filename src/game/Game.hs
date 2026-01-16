{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Game where

import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )

-- | Player class.
class (Eq a, Ord a, Bounded a, Enum a, Ix a, Show a) => PlayerId a where
    numPlayers :: Int    -- Number of players
    drawValue  :: Double -- Value of a draw
    succPlayer :: a -> a -- Next player, will wrap around.
    predPlayer :: a -> a -- Previous player, will wrap around.
    succPlayer p = if p == maxBound then minBound else succ p
    predPlayer p = if p == minBound then maxBound else pred p

#define ENUM Eq,Ord,Bounded,Enum,Ix,Show
data P1 = P1a                         deriving (ENUM) -- ^ 1 Player
data P2 = P2a | P2b                   deriving (ENUM) -- ^ 2 Players
data P3 = P3a | P3b | P3c             deriving (ENUM) -- ^ 3 Players
data P4 = P4a | P4b | P4c | P4d       deriving (ENUM) -- ^ 4 Players
data P5 = P5a | P5b | P5c | P5d | P5e deriving (ENUM) -- ^ 5 Players
#undef ENUM

instance PlayerId P1 where numPlayers = 1; drawValue = 1/1
instance PlayerId P2 where numPlayers = 2; drawValue = 1/2
instance PlayerId P3 where numPlayers = 3; drawValue = 1/3
instance PlayerId P4 where numPlayers = 4; drawValue = 1/4
instance PlayerId P5 where numPlayers = 5; drawValue = 1/5

-- | Game result.
data Result p = Draw | Win p deriving (Eq,Ord,Show)

-- | Game class.
class Game g where
    type Player g :: Type
    type Move   g :: Type
    gameStatus :: g -> Maybe (Result (Player g))
    gamePlayer :: g -> Player g
    gameMoves  :: g -> NonEmpty (Move g)
    nextGames  :: g -> [g]

type GAME g = (Game g, PlayerId (Player g))
