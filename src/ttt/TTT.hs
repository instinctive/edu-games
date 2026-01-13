{-# LANGUAGE TemplateHaskell #-}

module TTT
    ( TTT
    , initTTT, showTTT, parseTTT
    ) where

import Control.Lens
import Data.List.Split ( chunksOf )

import Game

type Sq = Either Int Player
type Board = [Sq]

data TTT = TTT
    { _gPlayer :: Player
    , _gStatus :: Status
    , _gMoves  :: [Int]
    , _gBoard  :: Board
    } deriving Show
makeLenses ''TTT

instance Game TTT where
    type Move TTT = Int
    gamePlayer = _gPlayer
    gameStatus = _gStatus
    nextGames = tttNext
    lastMove g = case _gMoves g of [] -> Nothing; m:_ -> Just m

showPlayer P1 = "X"
showPlayer P2 = "O"

showStatus Play = "Game is in play."
showStatus Draw = "Game is a draw."
showStatus (Win p) = showPlayer p <> " has won."

showSq = either show showPlayer

showTTT TTT{..} =
    intersperse "---+---+---"
    [ intercalate "|"
      [ " " <> showSq sq <> " " | sq <- row ]
      | row <- chunksOf 3 _gBoard ]
    <>
    [ "Moves: " <> intercalate ", " (reverse $ show <$> _gMoves) ]
    <>
    [ if _gStatus == Play
      then showPlayer _gPlayer <> " to move."
      else showStatus _gStatus ]

initTTT = TTT P1 Play [] $ Left <$> [1..9]

parseTTT = readMaybe @Int

update g@TTT{..}
    | isWin     = g & over gPlayer opp . set gStatus (Win _gPlayer)
    | isDraw    = g & over gPlayer opp . set gStatus Draw
    | otherwise = g & over gPlayer opp
  where
    isDraw = all isRight _gBoard
    isWin = any (all (==Right _gPlayer)) $ rows <> cols <> diag
    rows = chunksOf 3 _gBoard
    cols = transpose rows
    diag = chunksOf 3 $ (_gBoard!!) <$> [0,4,8,2,4,6]

tttNext g@TTT{..} = if _gStatus /= Play then [] else
    [ g & update . over gMoves (m:) . set gBoard board
    | m <- lefts _gBoard
    , let (aa,_:bb) = splitAt (m-1) _gBoard
    , let board = aa <> [Right _gPlayer] <> bb
    ]
