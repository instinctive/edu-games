module Game where

import Data.Kind ( Type )

data Result p = Draw | Win p deriving (Eq,Ord,Show)

class Game g where
    type Player g :: Type
    type Move   g :: Type
    gameStatus :: g -> Maybe (Result (Player g))
    gamePlayer :: g -> Player g
    gameMoves  :: g -> NonEmpty (Move g)
    nextGames  :: g -> [g]
