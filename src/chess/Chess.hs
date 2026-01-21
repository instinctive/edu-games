-- Pragmas and Imports {{{
-- ------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Chess where

import Control.Lens hiding ( universe )
import Data.Array.Base ( unsafeAt, unsafeReplace )
import Data.Array.Unboxed
import Data.Bits.Lens ( bitAt )
import Data.Text ( Text )
import Linear.V2

import Game

-- }}}

-- Colors and Pieces {{{
-- ------------------------------------------------------------

#define ENUM Eq,Ord,Bounded,Enum,Ix,Show
data Color = White | Black deriving (ENUM)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving (ENUM)
#undef ENUM

class (Eq a, Ord a, Bounded a, Enum a, Ix a, Show a) => ENUM a
instance ENUM Color
instance ENUM PType

data Piece = Piece
    { _pColor :: !Color
    , _pType :: !PType
    }
makeLenses ''Piece

instance Show Piece where
    show Piece{..} = [color _pColor, ptype _pType] & adjust _pColor where
        adjust White = map toUpper
        adjust Black = id
        color White  = 'w'
        color Black  = 'b'
        ptype Pawn   = 'p'
        ptype Knight = 'n'
        ptype Bishop = 'b'
        ptype Rook   = 'r'
        ptype Queen  = 'q'
        ptype King   = 'k'

opp White = Black
opp Black = White

-- }}}

-- Squares {{{
-- ------------------------------------------------------------

newtype File = File Int
newtype Rank = Rank Int
instance Show File where show (File i) = [ chr $ ord 'a' + i ]
instance Show Rank where show (Rank i) = [ intToDigit $ i + 1 ]

fileA = File 0
fileB = File 1
fileC = File 2
fileD = File 3
fileE = File 4
fileF = File 5
fileG = File 6
fileH = File 7

rank1 = Rank 0
rank2 = Rank 1
rank3 = Rank 2
rank4 = Rank 3
rank5 = Rank 4
rank6 = Rank 5
rank7 = Rank 6
rank8 = Rank 7

pawnRank White = rank2
pawnRank Black = rank7

newtype Sq = Sq Int deriving (Eq,Ord,Enum,Ix)

sqFileRank (Sq q) = bimap File Rank (unsafeShiftR q 3, q .&. 7)
mkSq (File file) (Rank rank) = Sq $ unsafeShiftL file 3 .|. rank
allFiles = File <$> [0..7]
rankSqs rank = flip mkSq rank <$> allFiles

instance Bounded Sq where
    minBound = Sq  0
    maxBound = Sq 63

instance Show Sq where
    show sq = show file <> show rank where
        (file,rank) = sqFileRank sq

instance Read Sq where
    readsPrec _ (c:d:more) = maybeToList $ (,more) <$> readSq [c,d]
    readsPrec _ _ = []

readSq [c,d] = mk <$> file <*> rank where
    mk file rank = Sq $ file * 8 + rank
    file = findIndex (==c) "abcdefgh"
    rank = findIndex (==d) "12345678"

instance IsString Sq where fromString = fromJust . readSq

toBlack, toWhite, toKSide, toQSide :: Sq -> Maybe Sq
toBlack (sqFileRank -> (file, Rank i)) = i < 7 & bool Nothing (Just $ mkSq file (Rank (i+1)))
toWhite (sqFileRank -> (file, Rank i)) = i > 0 & bool Nothing (Just $ mkSq file (Rank (i-1)))
toKSide (sqFileRank -> (File i, rank)) = i < 7 & bool Nothing (Just $ mkSq (File (i+1)) rank)
toQSide (sqFileRank -> (File i, rank)) = i > 0 & bool Nothing (Just $ mkSq (File (i-1)) rank)

toColor Black = toBlack
toColor White = toWhite

-- }}}

-- Total {{{
-- ------------------------------------------------------------

universe :: forall a. (Bounded a, Enum a) => [a]
universe = [minBound..maxBound]

newtype Total a i e = Total (a i e) deriving Show

totalArray :: forall a i e. (IArray a e, Bounded i, Enum i, Ix i) => [e] -> Total a i e
totalArray = Total . listArray (minBound,maxBound)

totalAt :: forall a i e. (IArray a e, Bounded i, Enum i, Ix i) => i -> Lens' (Total a i e) e
totalAt i f (Total ary) = let x = fromEnum i in
    f (unsafeAt ary x) <&> \e -> Total $ unsafeReplace ary [(x,e)]

-- }}}

-- BitBoard {{{
-- ------------------------------------------------------------

data BitBoard = BitBoard
    { _bbColor :: !(Total UArray Color Word64)
    , _bbPType :: !(Total UArray PType Word64)
    } deriving Show
makeLenses ''BitBoard

bbAt :: Sq -> Lens' BitBoard (Maybe Piece)
bbAt (Sq q) f bb@BitBoard{..} =
    f m <&> \m' -> case (m,m') of
        (Nothing,   Nothing)   -> bb
        (Just prev, Nothing)   -> bb & update prev False
        (Nothing,   Just next) -> bb & update next True
        (Just prev, Just next) -> bb & update prev False . update next True
  where
    m = Piece <$> c <*> p
    c = listToMaybe [ i | i <- universe, testBit (_bbColor^.totalAt i) q ]
    p = listToMaybe [ i | i <- universe, testBit (_bbPType^.totalAt i) q ]
    update (Piece c p) b
      = set (bbColor.totalAt c.bitAt q) b
      . set (bbPType.totalAt p.bitAt q) b

emptyBitBoard = BitBoard (totalArray $ repeat 0) (totalArray $ repeat 0)

startBitBoard = foldl' f emptyBitBoard $
    zip (rankSqs rank8) (Piece Black <$> ptypes) <>
    zip (rankSqs rank1) (Piece White <$> ptypes) <>
    zip (rankSqs rank7) (replicate 8 (Piece Black Pawn)) <>
    zip (rankSqs rank2) (replicate 8 (Piece White Pawn))
  where
    ptypes = [Rook,Knight,Bishop,Queen,King,Bishop,Knight,Rook]
    f !bb (sq,piece) = bb & bbAt sq .~ Just piece

-- }}}

-- Moves {{{
-- ------------------------------------------------------------

data MType
    = Jump Sq | Slide [Sq]
    | Advance Sq | Double Sq Sq | EnPassant Sq Sq | Promote Sq PType
    | CastleKSide | CastleQSide
    deriving Show

moveArray :: Total Array (Sq,Color,PType) [MType]
moveArray = Total $ listArray (minBound,maxBound)
    [ mkMoves p sq c | sq <- universe, c  <- universe, p  <- universe ]

mkMoves :: Sq -> Color -> PType -> [MType]

mkMoves sq c Pawn   = undefined
mkMoves sq c Bishop = undefined
mkMoves sq c Rook   = undefined
mkMoves sq c Queen  = undefined

mkMoves q c King   = Jump <$> delete q (mapMaybe ($ q) xfmKing)
mkMoves q c Knight = Jump <$> mapMaybe ($ q) xfmKnight

xfmKing = (>=>) <$> [toBlack, Just, toWhite] <*> [toKSide, Just, toQSide]

xfmKnight =
    ( (>=>) <$> [twice toBlack, twice toWhite] <*> [toKSide, toQSide] ) <>
    ( (>=>) <$> [toBlack, toWhite] <*> [twice toKSide, twice toQSide] ) 
  where twice f = f >=> f

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
