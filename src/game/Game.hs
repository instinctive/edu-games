{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Game where

import Data.Kind ( Type )

class (Eq a, Ord a, Bounded a, Enum a, Ix a, Show a) => PlayerId a where
    numPlayers :: Int
    succPlayer :: a -> a
    predPlayer :: a -> a
    numPlayers = fromEnum (maxBound @a) - fromEnum (minBound @a) + 1
    succPlayer p = if p == maxBound then minBound else succ p
    predPlayer p = if p == minBound then maxBound else pred p

#define PLAYERID Eq,Ord,Bounded,Enum,Ix,Show,PlayerId
data P1 = P1a                         deriving (PLAYERID)
data P2 = P2a | P2b                   deriving (PLAYERID)
data P3 = P3a | P3b | P3c             deriving (PLAYERID)
data P4 = P4a | P4b | P4c | P4d       deriving (PLAYERID)
data P5 = P5a | P5b | P5c | P5d | P5e deriving (PLAYERID)
#undef PLAYERID

data Status p = Play | Draw | Win p deriving (Eq,Ord,Show)

class Game g where
    type Player g :: Type
    type Move   g :: Type
    gamePlayer :: g -> Player g
    gameStatus :: g -> Status (Player g)
    nextGames  :: g -> [g]
    lastMove   :: g -> Move g
