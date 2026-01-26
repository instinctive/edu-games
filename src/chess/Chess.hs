-- Pragmas and Imports {{{
-- ------------------------------------------------------------

-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Chess where
-- module Chess ( initChess ) where

import Control.Lens hiding ( universe, (<|) )
import Control.Lens.Extras ( is )
import Data.Array.Base ( unsafeAt, unsafeReplace )
import Data.Array.Unboxed
import Data.Bits.Lens ( bitAt )
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as M
import Data.List.NonEmpty ( NonEmpty((:|)), (<|) )
import Data.Text ( Text, pack )
import GHC.Ix ( unsafeIndex )
import Linear.V2

import Game

-- }}}

-- Colors and Pieces {{{
-- ------------------------------------------------------------

#define ENUM Eq,Ord,Bounded,Enum,Ix,Show
data Side = KSide | QSide deriving (ENUM)
data Color = White | Black deriving (ENUM)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving (ENUM)
#undef ENUM

class (Eq a, Ord a, Bounded a, Enum a, Ix a, Show a) => ENUM a
instance ENUM Side
instance ENUM Color
instance ENUM PType

data Piece = Piece
    { _pColor :: !Color
    , _pType  :: !PType
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

pieceRank White = rank1
pieceRank Black = rank8
pawnRank White = rank2
pawnRank Black = rank7
epRank White = rank5
epRank Black = rank4
-- }}}

newtype Sq = Sq (V2 Int) -- {{{
    deriving (Eq,Ord,Ix,Num)

instance Bounded Sq where
    minBound = Sq (V2 0 0)
    maxBound = Sq (V2 7 7)

instance Show Sq where
    show (Sq (V2 file rank)) = show (File file) <> show (Rank rank)

instance Read Sq where
    readsPrec _ (c:d:more) = maybeToList $ (,more) <$> readSq []
    readsPrec _ _ = []

instance IsString Sq where fromString = fromJust . readSq

readSq [c,d] = mk <$> file <*> rank where
    mk file rank = Sq (V2 file rank)
    file = findIndex (==c) "abcdefgh"
    rank = findIndex (==d) "12345678"
readSq s = error $ "readSq " <> show s

readSq' = fromJust . readSq

-- }}}

sqFileRank (Sq (V2 file rank)) = (File file, Rank rank)
sqFile (Sq (V2 file rank)) = File file
sqRank (Sq (V2 file rank)) = Rank rank
mkSq (File file) (Rank rank) = Sq (V2 file rank)
allFiles = File <$> [0..7]
rankSqs rank = flip mkSq rank <$> allFiles

kingSq Black       = readSq' "e8"
kingSq White       = readSq' "e1"
rookSq Black KSide = readSq' "h8"
rookSq White KSide = readSq' "h1"
rookSq Black QSide = readSq' "a8"
rookSq White QSide = readSq' "a1"

toKSide q = q + (Sq (V2 1 0))
toQSide q = q - (Sq (V2 1 0))
toBlack q = q + (Sq (V2 0 1))
toWhite q = q - (Sq (V2 0 1))

toSide  KSide = toKSide
toSide  QSide = toQSide
toColor Black = toBlack
toColor White = toWhite

toDiag color side = toColor color . toSide side

isEPRank c q = sqRank q == epRank c
epSq q t = mkSq (sqFile t) (sqRank q)

isValid q@(Sq (V2 file rank)) = -- bool Nothing (Just q) $
    file >= 0 && file <= 7 &&
    rank >= 0 && rank <= 7

-- }}}

-- Total {{{
-- ------------------------------------------------------------

universe :: forall a. (Bounded a, Ix a) => [a]
universe = range (minBound,maxBound)

newtype Total a i e = Total (a i e) deriving Show

totalArray :: forall a i e. (IArray a e, Bounded i, Ix i) => [e] -> Total a i e
totalArray = Total . listArray (minBound,maxBound)

totalAt :: forall a i e. (IArray a e, Bounded i, Ix i) => i -> Lens' (Total a i e) e
totalAt i f (Total ary) = let x = unsafeIndex (minBound, maxBound) i in
    f (unsafeAt ary x) <&> \e -> Total $ unsafeReplace ary [(x,e)]

hackAt (Total ary) idx = ary!idx

-- }}}

-- Castling Rights {{{
-- ------------------------------------------------------------

newtype Castle = Castle Int
instance Show Castle where
    show x = show
        [ (c,s) | c <- universe, s <- universe, x^.castle (c,s) ]

castle :: (Color,Side) -> Lens' Castle Bool
castle (c,s) f (Castle r) =
    let i = fromEnum c * 2 + fromEnum s in
    f (testBit r i) <&> Castle . bool (clearBit r i) (setBit r i)

castleArray :: Total Array Sq [(Color,Side)]
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

-- Board {{{
-- ------------------------------------------------------------

data Board = Board
    { _bbMap       :: !(Map Sq Piece)
    , _bbKingSq    :: !(Total Array Color Sq)
    , _bbCastle    :: !Castle
    , _bbEnPassant :: !(Maybe Sq)
    } deriving Show
makeLenses ''Board

sqColor Board{..} q = _bbMap ^? ix q . pColor
sqPType Board{..} q = _bbMap ^? ix q . pType

bbAt :: Sq -> Lens' Board (Maybe Piece)
bbAt q f bb@Board{..} =
    f m <&> \m' -> bb & clr m . add m'
  where
    m = _bbMap ^? ix q
    clr Nothing  = id
    clr (Just p) = (bbMap . at q .~ Nothing) . updateCastle
    add Nothing  = id
    add j@(Just (Piece c p))
        = (bbMap . at q .~ j)
        . bool id (bbKingSq.totalAt c .~ q) (p == King)
    updateCastle bb = foldr f bb (castleArray^.totalAt q) where
        f idx = bbCastle.castle idx .~ False

bbPieces Board{..} color =
    [ (q,p) | (q, Piece c p) <- M.assocs _bbMap, c == color ]

emptyBoard = Board
    M.empty
    (totalArray $ kingSq <$> universe)
    (Castle 0)
    Nothing

startBoard = addCastle . foldl' f emptyBoard $
    zip (rankSqs rank8) (Piece Black <$> ptypes) <>
    zip (rankSqs rank1) (Piece White <$> ptypes) <>
    zip (rankSqs rank7) (replicate 8 (Piece Black Pawn)) <>
    zip (rankSqs rank2) (replicate 8 (Piece White Pawn))
  where
    ptypes = [Rook,Knight,Bishop,Queen,King,Bishop,Knight,Rook]
    f !bb (q,p) = bb & bbMap . at q .~ Just p
    addCastle bb = foldr add bb $ (,) <$> universe <*> universe where
        add idx = bbCastle.castle idx .~ True

-- }}}

-- MoveArray {{{
-- ------------------------------------------------------------

data MType
    = Jump Sq | Slide [Sq]
    | Capture Sq | EnPassant Sq
    | Advance Sq | Double Sq
    | CastleKSide [Sq] | CastleQSide [Sq]
    deriving Show
makePrisms ''MType

moveArray :: Array (Sq,Color,PType) [MType] -- {{{
moveArray = genArray (minBound,maxBound) \(q,c,p) -> mkMoves q c p

mkMoves q c Pawn =
    ( if | sqRank q == epRank c -> EnPassant <$> attacks
         | otherwise            -> Capture   <$> attacks ) <>
    ( if | sqRank q == pieceRank c       -> []
         | sqRank q == pieceRank (opp c) -> []
         | sqRank q == pawnRank c        -> [Double  advance]
         | otherwise                     -> [Advance advance] )
  where
    attacks = [ q' | s <- universe, let q' = toDiag (opp c) s q, isValid q' ]
    advance = toColor (opp c) q

mkMoves q c Knight =
    map Jump =<< gen <$> universe <*> universe
  where
    gen c s = filter isValid
        [ toColor c $ toColor c $ toSide s q
        , toColor c $ toSide  s $ toSide s q ]

mkMoves q c Bishop =
    map Slide $ gen <$> universe <*> universe
  where
    gen c s = takeWhile isValid $ tail $ iterate (toDiag c s) q

mkMoves q c Rook =
    (Slide <$> slide toColor) <>
    (Slide <$> slide toSide)
  where
    slide :: forall a. (Bounded a, Ix a) => (a -> Sq -> Sq) -> [[Sq]]
    slide f = [ takeWhile isValid $ tail $ iterate (f a) q | a <- universe ]

mkMoves q c Queen  = mkMoves q c Bishop <> mkMoves q c Rook

mkMoves q c King =
    ( Jump <$> cands ) <>
    ( q == ksq & bool []
        [ CastleQSide $ take 4 $ iterate toQSide ksq
        , CastleKSide $ take 3 $ iterate toKSide ksq ] )
  where
    ksq = kingSq c
    cands = filter isValid $ delete q $ ($ q) <$> xfms
    xfms = (.) <$> [toBlack,id,toWhite] <*> [toKSide,id,toQSide]

-- }}}
-- }}}

-- validMoves {{{
-- ------------------------------------------------------------

data Valid
    = MoveTo Sq
    | DoubleTo Sq | EPTo Sq
    | KSideTo Sq | QSideTo Sq
    deriving Show
makePrisms ''Valid

type UCIMove = Text

-- {{{
mkUCI src = \case
    MoveTo   tgt -> uci tgt
    DoubleTo tgt -> uci tgt
    EPTo     tgt -> uci tgt
    KSideTo  tgt -> uci tgt
    QSideTo  tgt -> uci tgt
  where uci tgt = pack $ show src <> show tgt

validMoves bb c m = case m of
    Slide tt ->
        let (ee,xx@(~(x:_))) = span ((==Nothing).sqColor bb) tt in
        if null xx || sqColor bb x == Just c
           then MoveTo <$> ee
           else MoveTo <$> (x:ee)
    Jump t         | sqColor bb t /= Just c       -> [MoveTo t]
    Advance t      | sqColor bb t == Nothing      -> [MoveTo t]
    Capture t      | sqColor bb t == Just (opp c) -> [MoveTo t]
    EnPassant t    | sqColor bb t == Just (opp c) -> [MoveTo t]
                   | bb^.bbEnPassant == Just t    -> [EPTo t]
    Double t       | sqColor bb t == Nothing      -> [MoveTo t] <> double t
    CastleKSide tt | canCastle KSide tt           -> [KSideTo $ tt!!2]
    CastleQSide tt | canCastle QSide tt           -> [QSideTo $ tt!!2]
    _ -> []
  where
    double t = let t' = toColor (opp c) t in
        sqColor bb t' == Nothing & bool [] [DoubleTo t']
    canCastle side tt =
        ( bb^.bbCastle.castle (c,side)) &&
        ( all ((==Nothing).sqColor bb) (tail tt) ) &&
        ( not $ any (isAttacked bb c) (take 3 tt) )
-- }}}
-- }}}

-- applyMove {{{

applyMove bb c q v =
    if inCheck then [] else [bb']
  where
    inCheck = isAttacked bb' c (bb' ^. bbKingSq.totalAt c)
    bb' = bb & set bbEnPassant Nothing . case v of
        MoveTo   t -> move q t
        DoubleTo t -> move q t . set bbEnPassant (Just $ toColor c t)
        EPTo     t -> move q t . set (bbAt $ epSq q t) Nothing
        KSideTo  t -> move q t . move (rookSq c KSide) (toQSide t)
        QSideTo  t -> move q t . move (rookSq c QSide) (toKSide t)
    move a b = let m@(Just _) = bb ^. bbAt a in
        set (bbAt a) Nothing . set (bbAt b) m

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

-- getMoves {{{
-- ------------------------------------------------------------

getValidMoves bb c =
    [ (q,valid)
    | (q,p) <- bbPieces bb c
    , raw   <- moveArray!(q,c,p)
    , valid <- validMoves bb c raw
    ]

getMoves :: Board -> Color -> [(UCIMove,Board)]
getMoves bb c =
    [ (mkUCI q valid, bb')
    | (q,valid) <- getValidMoves bb c
    , bb'       <- applyMove bb c q valid
    ]

-- }}}

type ChessResult = Result2P Color

data Chess = Chess
    { _chessPlayer :: !Color
    , _chessStatus :: !(Maybe ChessResult)
    , _chessDepth  :: !Int
    , _chessMoves  :: !(NonEmpty UCIMove)
    , _chessBoard  :: !Board
    } deriving Show
makeLenses ''Chess

instance Game Chess Color UCIMove ChessResult where
    gamePlayer   = _chessPlayer
    gameStatus   = _chessStatus
    gameMoves    = _chessMoves
    gameChildren = chessChildren

initChess = Chess White Nothing 0 (pack "init":|[]) startBoard

inCheck Chess{..} =
    isAttacked _chessBoard _chessPlayer
    $ _chessBoard^.bbKingSq.totalAt _chessPlayer

getMovesChess Chess{..} = getMoves _chessBoard _chessPlayer

chessChildren chess =
    setStatus . mk <$> getMovesChess chess
  where
    mk (move,bb) = chess
        & over chessPlayer opp
        . over chessDepth (+1)
        . over chessMoves (move<|)
        . set chessBoard bb
    setStatus g@Chess{..}
        | _chessDepth > 50 = g & set chessStatus (Just Draw)
        | not (null $ getMovesChess g) = g
        | inCheck g = g & set chessStatus (Just . Win $ g^.chessPlayer.to opp)
        | otherwise = g & set chessStatus (Just Draw)
