{-# LANGUAGE TemplateHaskell #-}

module TTT
    ( TTT
    , initTTT
    ) where

import Control.Lens hiding ((<|))
import Data.List.Split (chunksOf)
import Data.List.NonEmpty (NonEmpty((:|)),(<|))
import qualified Data.List.NonEmpty as N

import Game

data Player = X | O deriving (Eq,Ord,Bounded,Enum,Ix,Show)

opp X = O
opp O = X

type Sq    = Either Int Player
type Board = [Sq]

data TTT = TTT
    { _gPlayer :: !Player
    , _gStatus :: !(Maybe (Result Player))
    , _gMoves  :: !(NonEmpty Int)
    , _gBoard  :: Board
    } deriving Show
makeLenses ''TTT

instance Game TTT Player Int where
    gameStatus = _gStatus
    gamePlayer = _gPlayer
    gameMoves  = _gMoves
    nextGames  = tttNext

initTTT = TTT X Nothing (0:|[]) $ Left <$> [1..9]

update g@TTT{..}
    | isWin     = g & over gPlayer opp . set gStatus (Just $ Win _gPlayer)
    | isDraw    = g & over gPlayer opp . set gStatus (Just Draw)
    | otherwise = g & over gPlayer opp
  where
    isDraw = all isRight _gBoard
    isWin = any (all (==Right _gPlayer)) $ rows <> cols <> diag
    rows = chunksOf 3 _gBoard
    cols = transpose rows
    diag = chunksOf 3 $ (_gBoard!!) <$> [0,4,8,2,4,6]

tttNext g@TTT{..} = _gStatus & flip maybe (const [])
    [ g & update . over gMoves (m<|) . set gBoard board
    | m <- lefts _gBoard
    , let (aa,_:bb) = splitAt (m-1) _gBoard
    , let board = aa <> [Right _gPlayer] <> bb
    ]

tttLastMove = N.head . _gMoves

playTTT g@TTT{..} = do
    putTTT g
    case tttNext g of [] -> pure _gStatus; gg -> loop gg
  where
    loop gg = do
        l <- getLine
        case readMaybe @Int l >>= \m -> find ((==m).tttLastMove) gg of
            Nothing -> putStrLn "invalid move" >> loop gg
            Just g' -> playTTT g'

-- Text interface

showResult Draw = "Game is a draw."
showResult (Win p) = show p <> " has won."

showSq = either show show

showTTT TTT{..} =
    intersperse "---+---+---"
    [ intercalate "|"
      [ " " <> showSq sq <> " " | sq <- row ]
      | row <- chunksOf 3 _gBoard ]
    <>
    [ "Moves: " <> intercalate ", " (reverse $ show <$> N.toList _gMoves) ]
    <>
    [ _gStatus & maybe
      (show _gPlayer <> " to move.")
      showResult ]

putTTT = traverse_ putStrLn . showTTT

parseTTT = readMaybe @Int
