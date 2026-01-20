{-# LANGUAGE OverloadedStrings #-}

module Chess
    ( Chess
    , initChess
    ) where

import Data.Text ( Text )

import Game

data Color = White | Black deriving (Eq,Ord,Bounded,Enum,Ix,Show)

type Move = Text

data Chess = Chess
    { _chessPlayer :: Color
    , _chessStatus :: !(Maybe (Result Color))
    , _chessMoves  :: !(NonEmpty Move)
    } deriving Show

instance Game Chess Color Move where
    gameStatus = _chessStatus
    gamePlayer = _chessPlayer
    gameMoves  = _chessMoves
    nextGames  = const []

initChess = Chess White Nothing ("init" :| [])
