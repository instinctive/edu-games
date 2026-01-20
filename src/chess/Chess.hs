{-# LANGUAGE OverloadedStrings #-}

module Chess
    ( Chess
    , initChess
    ) where

import Data.Text ( Text )

import Game

data Color = White | Black deriving (Eq,Ord,Bounded,Enum,Ix,Show)

type Move = Text

type ChessResult = Result2P Color

data Chess = Chess
    { _chessPlayer :: Color
    , _chessStatus :: !(Maybe ChessResult)
    , _chessMoves  :: !(NonEmpty Move)
    } deriving Show

instance Game Chess Color Move ChessResult where
    gameStatus   = _chessStatus
    gamePlayer   = _chessPlayer
    gameMoves    = _chessMoves
    gameChildren = const []

initChess = Chess White Nothing ("init" :| [])
