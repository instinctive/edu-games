module Game where

import Data.Kind ( Type )

data Player = P1 | P2                  deriving ( Eq, Ord, Show )
data Status = Play | Draw | Win Player deriving ( Eq, Ord, Show )

class Game g where
    type Move g :: Type
    gamePlayer :: g -> Player
    gameStatus :: g -> Status
    nextGames :: g -> [g]
    lastMove :: g -> Maybe (Move g)

opp P1 = P2
opp P2 = P1
