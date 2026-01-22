-- Pragmas and Imports {{{
-- ------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Chess where

import Control.Lens hiding ( universe )
import Control.Lens.Extras ( is )
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
data BSide = KSide | QSide deriving (ENUM)
data Color = White | Black deriving (ENUM)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving (ENUM)
#undef ENUM

class (Eq a, Ord a, Bounded a, Enum a, Ix a, Show a) => ENUM a
instance ENUM BSide
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

newtype File = File Int deriving (Eq,Ord) -- {{{
newtype Rank = Rank Int deriving (Eq,Ord)
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
epRank White = rank5
epRank Black = rank4
-- }}}

newtype Sq = Sq Int -- {{{
    deriving (Eq,Ord,Enum,Ix)

instance Bounded Sq where
    minBound = Sq  0
    maxBound = Sq 63

instance Show Sq where
    show sq = show file <> show rank where
        (file,rank) = sqFileRank sq

instance Read Sq where
    readsPrec _ (c:d:more) = maybeToList $ (,more) <$> readSq [c,d]
    readsPrec _ _ = []

instance IsString Sq where fromString = fromJust . readSq

readSq [c,d] = mk <$> file <*> rank where
    mk file rank = Sq $ file * 8 + rank
    file = findIndex (==c) "abcdefgh"
    rank = findIndex (==d) "12345678"

-- }}}

sqRank (Sq q) = Rank $ q .&. 7
sqFileRank (Sq q) = bimap File Rank (unsafeShiftR q 3, q .&. 7)
mkSq (File file) (Rank rank) = Sq $ unsafeShiftL file 3 .|. rank
allFiles = File <$> [0..7]
rankSqs rank = flip mkSq rank <$> allFiles

kingSq Black  = read @Sq "e8"
kingSq White  = read @Sq "e1"
rookSq Black KSide = read @Sq "h8"
rookSq White KSide = read @Sq "h1"
rookSq Black QSide = read @Sq "a8"
rookSq White QSide = read @Sq "a1"

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

hackAt (Total ary) idx = ary!idx

-- }}}

-- Castling Rights {{{
-- ------------------------------------------------------------

newtype Castle = Castle Int
instance Show Castle where
    show x = show
        [ (c,s) | c <- universe, s <- universe, x^.castle (c,s) ]

castle :: (Color,BSide) -> Lens' Castle Bool
castle (c,s) f (Castle r) =
    let i = fromEnum c * 2 + fromEnum s in
    f (testBit r i) <&> Castle . bool (clearBit r i) (setBit r i)

castleArray :: Total Array Sq [(Color,BSide)]
castleArray = totalArray
    [ if | sq == kingSq Black -> [(Black,KSide),(Black,QSide)]
         | sq == kingSq White -> [(White,KSide),(White,QSide)]
         | sq == rookSq Black KSide -> [(Black,KSide)]
         | sq == rookSq Black QSide -> [(Black,QSide)]
         | sq == rookSq White KSide -> [(White,KSide)]
         | sq == rookSq White QSide -> [(White,QSide)]
         | otherwise -> []
    | sq <- universe ]

-- }}}

-- BitBoard {{{
-- ------------------------------------------------------------

data BitBoard = BitBoard
    { _bbColor :: !(Total UArray Color Word64)
    , _bbPType :: !(Total UArray PType Word64)
    , _bbCastle :: !Castle
    , _bbEnPassant :: !(Maybe Sq)
    } deriving Show
makeLenses ''BitBoard

sqColor BitBoard{..} (Sq q) =
    listToMaybe [ i | i <- universe, testBit (_bbColor^.totalAt i) q ]

sqPType BitBoard{..} (Sq q) =
    listToMaybe [ i | i <- universe, testBit (_bbPType^.totalAt i) q ]

bbAt :: Sq -> Lens' BitBoard (Maybe Piece)
bbAt sq@(Sq q) f bb@BitBoard{..} =
    f m <&> \m' -> case (m,m') of
        (Nothing,   Nothing)   -> bb
        (Just prev, Nothing)   -> bb & update prev False
        (Nothing,   Just next) -> bb & update next True
        (Just prev, Just next) -> bb & update prev False . update next True
  where
    m = Piece <$> sqColor bb sq <*> sqPType bb sq
    update (Piece c p) b
        = set (bbColor.totalAt c.bitAt q) b
        . set (bbPType.totalAt p.bitAt q) b
        . \bb -> foldr f bb (castleArray^.totalAt sq)
      where f idx = bbCastle.castle idx .~ False

emptyBitBoard = BitBoard
    (totalArray $ repeat 0)
    (totalArray $ repeat 0)
    (Castle 0)
    Nothing

startBitBoard = addCastle . foldl' f emptyBitBoard $
    zip (rankSqs rank8) (Piece Black <$> ptypes) <>
    zip (rankSqs rank1) (Piece White <$> ptypes) <>
    zip (rankSqs rank7) (replicate 8 (Piece Black Pawn)) <>
    zip (rankSqs rank2) (replicate 8 (Piece White Pawn))
  where
    ptypes = [Rook,Knight,Bishop,Queen,King,Bishop,Knight,Rook]
    f !bb (Sq q,Piece c p) = bb
        & set (bbColor.totalAt c.bitAt q) True
        . set (bbPType.totalAt p.bitAt q) True
    addCastle bb = foldr add bb $ (,) <$> universe <*> universe where
        add idx = bbCastle.castle idx .~ True

-- }}}

-- MoveArray {{{
-- ------------------------------------------------------------

data MType
    = Jump Sq | Slide [Sq]
    | Advance Sq | Capture Sq
    | Double Sq Sq | EnPassant Sq Sq
    | CastleKSide [Sq] | CastleQSide [Sq]
    deriving Show
makePrisms ''MType

moveArray :: Array (Sq,Color,PType) [MType] -- {{{
moveArray = genArray (minBound,maxBound) \(q,c,p) -> mkMoves q c p

mkMoves q c Pawn = concat
    [ Capture <$> mapMaybe ($ q) (xfmPawnCaptures c)
    , sqRank q == epRank c & bool [] enPassants
    , sqRank q == pawnRank c & bool
        ( Advance <$> mapMaybe ($ q) [toColor $ opp c] )
        ( maybeToList $ Double <$> one <*> two )
    ]
  where
    one = toColor (opp c) q
    two = toColor (opp c) =<< one
    enPassants = catMaybes
        [ EnPassant <$> mv <*> ep
        | side <- [toQSide,toKSide]
        , let ep = side q
        , let mv = side q >>= toColor (opp c) ]

mkMoves q c King =
    ( Jump <$> delete q (mapMaybe ($ q) xfmKing) ) <>
    ( q == kingSq c & bool []
        [ CastleQSide $ take 4 $ iterate (fromJust.toQSide) (kingSq c)
        , CastleKSide $ take 3 $ iterate (fromJust.toKSide) (kingSq c)
        ] )

mkMoves q c Rook   = Slide <$> xfmRepeat q <$> xfmOrtho
mkMoves q c Bishop = Slide <$> xfmRepeat q <$> xfmDiags
mkMoves q c Queen  = mkMoves q c Rook <> mkMoves q c Bishop
mkMoves q c Knight = Jump <$> mapMaybe ($ q) xfmKnight

xfmRepeat q xfm = unfoldr (fmap double . xfm) q where double x = (x,x)

xfmPawnCaptures c = (>=>) (toColor $ opp c) <$> [toQSide,toKSide]
xfmOrtho = [toBlack,toWhite,toQSide,toKSide]
xfmDiags = (>=>) <$> [toBlack,toWhite] <*> [toQSide,toKSide]
xfmKing = (>=>) <$> [toBlack, Just, toWhite] <*> [toKSide, Just, toQSide]
xfmKnight =
    ( (>=>) <$> [twice toBlack, twice toWhite] <*> [toKSide, toQSide] ) <>
    ( (>=>) <$> [toBlack, toWhite] <*> [twice toKSide, twice toQSide] )
  where twice f = f >=> f
-- }}}
-- }}}

-- getMoves {{{
-- ------------------------------------------------------------

data Valid
    = MoveTo Sq
    | DoubleTo Sq | EnPassantTo Sq Sq
    | KSideTo Sq | QSideTo Sq
    deriving Show
makePrisms ''Valid

getMoves bb q c p =
    concatMap (validMoves bb c) (moveArray!(q,c,p))

validMoves bb c m = case m of
    Jump t | sqColor bb t /= Just c -> [MoveTo t]
    Slide tt ->
        let (ee,xx@(~(x:_))) = span ((==Nothing).sqColor bb) tt in
        if null xx || sqColor bb x == Just c
           then MoveTo <$> ee
           else MoveTo <$> (x:ee)
    Advance t | sqColor bb t == Nothing      -> [MoveTo t]
    Capture t | sqColor bb t == Just (opp c) -> [MoveTo t]
    EnPassant u v | bb^.bbEnPassant == Just u -> [EnPassantTo u v]
    Double u v | sqColor bb u == Nothing -> [MoveTo u] <>
        bool [] [DoubleTo v] (sqColor bb v == Nothing)
    CastleKSide tt | canCastle KSide tt -> [KSideTo $ tt!!2]
    CastleQSide tt | canCastle QSide tt -> [QSideTo $ tt!!2]
    _ -> []
  where
    canCastle side tt =
        ( bb^.bbCastle.castle (c,side)) &&
        ( all ((==Nothing).sqColor bb) (tail tt) ) &&
        ( not $ any (isAttacked bb c) (take 3 tt) )

-- }}}

-- isAttacked {{{
-- ------------------------------------------------------------

isAttacked bb c q =
    pawn || king || knight || rook || bishop
  where
    pawn = moveArray ^.. ix (q,c,Pawn) . traversed . filtered (is _Capture)
        & concatMap (ptypes . validMoves bb c)
        & any (==Pawn)
    king = moveArray ^.. ix (q,c,King) . traversed . filtered (is _Jump)
        & concatMap (ptypes . validMoves bb c)
        & any (==King)
    knight = moveArray ^. ix (q,c,Knight)
        & concatMap (ptypes . validMoves bb c)
        & any (==Knight)
    rook = moveArray ^. ix (q,c,Rook)
        & concatMap (ptypes . final . validMoves bb c)
        & any (`elem` [Rook,Queen])
    bishop = moveArray ^. ix (q,c,Bishop)
        & concatMap (ptypes . final . validMoves bb c)
        & any (`elem` [Bishop,Queen])
    final (_:xx@(_:_)) = final xx
    final xx = xx
    ptypes = concatMap \(MoveTo sq) -> sqPType bb sq & maybeToList

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
