{-# LANGUAGE TemplateHaskell #-}

module TTT where

import Control.Lens hiding ((<|))
import Data.List.Split (chunksOf)
import Data.List.NonEmpty (NonEmpty((:|)),(<|))
import qualified Data.List.NonEmpty as N

import Game

opp :: PlayerId a => a -> a
opp = succPlayer

type Sq    p = Either Int p
type Board p = [Sq p]

data TTT p = TTT
    { _gPlayer :: p
    , _gStatus :: Status p
    , _gMoves  :: NonEmpty Int
    , _gBoard  :: Board p
    } deriving Show
makeLenses ''TTT

initTTT :: Bounded p => TTT p
initTTT = TTT minBound Play (0:|[]) $ Left <$> [1..9]

showPlayer p = ["ABCDE" !! fromEnum p]

showStatus Play = "Game is in play."
showStatus Draw = "Game is a draw."
showStatus (Win p) = showPlayer p <> " has won."

showSq :: Enum p => Sq p -> String
showSq = either show showPlayer

showTTT TTT{..} =
    intersperse "---+---+---"
    [ intercalate "|"
      [ " " <> showSq sq <> " " | sq <- row ]
      | row <- chunksOf 3 _gBoard ]
    <>
    [ "Moves: " <> intercalate ", " (reverse $ show <$> N.toList _gMoves) ]
    <>
    [ if _gStatus == Play
      then showPlayer _gPlayer <> " to move."
      else showStatus _gStatus ]

putTTT :: (Eq p, Enum p) => TTT p -> IO ()
putTTT = traverse_ putStrLn . showTTT

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

-- ----------------------------------------------------------------------

instance PlayerId p => Game (TTT p) where
    type Player (TTT p) = p
    type Move   (TTT p) = Int
    gameStatus = _gStatus
    gamePlayer = _gPlayer
    gameMoves  = _gMoves
    nextGames  = tttNext
