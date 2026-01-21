-- Pragmas and Imports {{{
-- ------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Chess where

import Control.Lens hiding ( universe )
import Data.Array.Unboxed
import Data.Bits.Lens ( bitAt )
import Data.Text ( Text )

import Game

-- }}}

-- Colors and Pieces {{{
-- ------------------------------------------------------------

#define ENUM Eq,Ord,Bounded,Enum,Ix,Show
data Color = White | Black deriving (ENUM)
data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (ENUM)
#undef ENUM

class (Eq a, Ord a, Bounded a, Enum a, Ix a, Show a) => ENUM a
instance ENUM Color
instance ENUM Piece

-- }}}

-- Squares {{{
-- ------------------------------------------------------------

newtype Sq = Sq Int
newtype File = File Int
newtype Rank = Rank Int

instance Show File where show (File i) = [ chr $ ord 'a' + i ]
instance Show Rank where show (Rank i) = [ intToDigit $ i + 1 ]
instance Show Sq where
    show (Sq q) = show file <> show rank where
        (file,rank) = bimap File Rank $ quotRem q 8
instance Read Sq where
    readsPrec _ (c:d:more) = maybeToList $ (,more) <$> readSq [c,d]
    readsPrec _ _ = []

readSq [c,d] = mk <$> file <*> rank where
    mk file rank = Sq $ file * 8 + rank
    file = findIndex (==c) "abcdefgh"
    rank = findIndex (==d) "12345678"

instance IsString Sq where fromString = fromJust . readSq

-- }}}

-- BitBoard {{{
-- ------------------------------------------------------------

data BitBoard = BitBoard
    { _bbColor :: !(UArray Color Word64)
    , _bbPiece :: !(UArray Piece Word64)
    } deriving Show
makeLenses ''BitBoard

universe :: forall a. (Bounded a, Enum a) => [a]
universe = [minBound..maxBound]

totalArray :: forall i e. (ENUM i, IArray UArray e) => [e] -> UArray i e
totalArray = listArray (minBound,maxBound)

aryAt :: Ix i => i -> Lens' (UArray i Word64) Word64
aryAt i f ary = f (ary!i) <&> \e -> ary // [(i,e)]

bbAt :: Sq -> Lens' BitBoard (Maybe (Color,Piece))
bbAt (Sq q) f bb@BitBoard{..} =
    f m <&> \m' -> case (m,m') of
        (Nothing,Nothing) -> bb
        (Just (c,p),Nothing) ->
            bb & set (bbColor.aryAt c.bitAt q) False
               . set (bbPiece.aryAt p.bitAt q) False
        (Nothing,Just (c',p')) ->
            bb & set (bbColor.aryAt c'.bitAt q) True
               . set (bbPiece.aryAt p'.bitAt q) True
        (Just (c,p),Just (c',p')) ->
            bb & set (bbColor.aryAt c.bitAt q) False
               . set (bbPiece.aryAt p.bitAt q) False
               . set (bbColor.aryAt c'.bitAt q) True
               . set (bbPiece.aryAt p'.bitAt q) True
  where
    m = (,) <$> c <*> p
    c = listToMaybe [ i | i <- universe, testBit (_bbColor!i) q ]
    p = listToMaybe [ i | i <- universe, testBit (_bbPiece!i) q ]

initBitBoard = BitBoard (totalArray $ repeat 0) (totalArray $ repeat 0)

-- }}}

type Move = Text

type ChessResult = Result2P Color

data Chess = Chess
    { _chessPlayer :: Color
    , _chessStatus :: !(Maybe ChessResult)
    , _chessMoves  :: !(NonEmpty Move)
    , _chessBoard  :: BitBoard
    } deriving Show

instance Game Chess Color Move ChessResult where
    gameStatus   = _chessStatus
    gamePlayer   = _chessPlayer
    gameMoves    = _chessMoves
    gameChildren = const []
