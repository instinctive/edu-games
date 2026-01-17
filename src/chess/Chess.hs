{-# LANGUAGE OverloadedStrings #-}

module Chess
    ( Chess
    , initChess
    ) where

import Data.Text ( Text )

import Game ( Game, PlayerId, Result )
import Game qualified as G

data Color = White | Black deriving (Eq,Ord,Bounded,Enum,Ix,Show)
instance PlayerId Color where numPlayers = 2; drawValue = 0.5

type Move = Text

data Chess = Chess
    { _chessPlayer :: Color
    , _chessStatus :: !(Maybe (Result Color))
    , _chessMoves  :: !(NonEmpty Move)
    } deriving Show

instance Game Chess where
    type Player Chess = Color
    type Move   Chess = Move
    gameStatus = _chessStatus
    gamePlayer = _chessPlayer
    gameMoves  = _chessMoves
    nextGames  = const []

initChess = Chess White Nothing ("init" :| [])
