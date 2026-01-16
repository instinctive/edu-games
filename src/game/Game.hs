{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Game where

import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )

-- | Player class.
class (Eq a, Ord a, Bounded a, Enum a, Ix a, Show a) => PlayerId a where
    numPlayers :: Int    -- Number of players
    succPlayer :: a -> a -- Next player, will wrap around.
    predPlayer :: a -> a -- Previous player, will wrap around.
    numPlayers = fromEnum (maxBound @a) - fromEnum (minBound @a) + 1
    succPlayer p = if p == maxBound then minBound else succ p
    predPlayer p = if p == minBound then maxBound else pred p

#define PLAYERID Eq,Ord,Bounded,Enum,Ix,Show,PlayerId
data P1 = P1a                         deriving (PLAYERID) -- ^ 1 Player
data P2 = P2a | P2b                   deriving (PLAYERID) -- ^ 2 Players
data P3 = P3a | P3b | P3c             deriving (PLAYERID) -- ^ 3 Players
data P4 = P4a | P4b | P4c | P4d       deriving (PLAYERID) -- ^ 4 Players
data P5 = P5a | P5b | P5c | P5d | P5e deriving (PLAYERID) -- ^ 5 Players
#undef PLAYERID

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
