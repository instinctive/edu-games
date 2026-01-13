module Game where

import Data.Kind ( Type )

#define ENUM Eq,Ord,Enum,Bounded,Ix,Show
data P1 = P1a                         deriving (ENUM)
data P2 = P2a | P2b                   deriving (ENUM)
data P3 = P3a | P3b | P3c             deriving (ENUM)
data P4 = P4a | P4b | P4c | P4d       deriving (ENUM)
data P5 = P5a | P5b | P5c | P5d | P5e deriving (ENUM)
#undef ENUM

succPlayer p = if p == maxBound then minBound else succ p
predPlprev p = if p == minBound then maxBound else pred p

data Status p = Play | Draw | Win p deriving (Eq,Ord,Show)

class Game g where
    type Player g :: Type
    type Move   g :: Type
    gamePlayer :: g -> Player g
    gameStatus :: g -> Status (Player g)
    nextGames  :: g -> [g]
    lastMove   :: g -> Move g
