{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module MCT where

import Control.Lens
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Tree
import Data.Tree.Lens

import Control.Monad.Random  ( evalRand, runRand, getRandomR )
import System.Random         ( StdGen, newStdGen, split )
import System.Random.Shuffle ( shuffleM )

import Total
import Game

data MCT g = MCT
    { _mctGame   :: g
    , _mctVisits :: Double
    , _mctDraws  :: Double
    , _mctWins   :: UArray (Player g) Double
    }
makeLenses ''MCT

initMCT g = MCT g 0 0 (totalArray $ repeat 0)

mkTree g = initMCT <$> tree g where
    tree  = ap Node (map tree . nextGames)

param = sqrt 2 :: Double

eval numPlayers player logParent MCT{..} =
    exploit + param * explore
  where
    score = _mctWins^.totalAt player + _mctDraws / numPlayers
    exploit = score / _mctVisits
    explore = sqrt (logParent / _mctVisits)

addResult r = over mctVisits (+1) . case r of
    Draw  -> over mctDraws (+1)
    Win p -> over (mctWins . totalAt p) (+1)

pickOne l = getRandomR (0, length l - 1) <&> (l!!)

rollout g = gameStatus g & maybe next pure where
    next = pickOne (nextGames g) >>= rollout

search node@(Node m tt)
    | null tt = terminal
    | null uu = selection
    | null vv = expansion
  where
    isVisited (Node m _) = m^.mctVisits > 0
    (vv,uu@(~(u:uu'))) = span isVisited tt
    terminal =
        let r = m^.mctGame.to gameStatus._Just
        in (r, addResult r m)
    selection =
        let vv = view (root.to eval) <$> tt
            Just i = findIndex (==maximum vv) vv
        in 

        

-- search node =


-- search node@(Node (Right ggen) tt) =
--     rollout ggen <&> \m -> set root (Left m) node

-- search node@(Node (Left m) tt)
--     | null tt   = terminal
--     | null rr   = selection
--     | otherwise = expansion ll t rr'
--   where
--     terminal =
--         let plays = m ^. mctVisits in
--         let reward = totalArray (m ^. mctReward . to elems . to (map (/plays))) in
--         (reward, node & over (root._Left) (over mctVisits (+1) . over mctReward (addReward reward)))
--     isLeftTree (Node a _) = isLeft a
--     (ll,rr@(~(t:rr'))) = span isLeftTree tt
--     param     = sqrt 2
--     player    = m ^. mctGame . to gamePlayer
--     logParent = m ^. mctVisits . to log
--     eval (Node (Left child) _) = exploit + param * explore where
--         exploit = child ^. mctReward . totalAt player / child ^. mctVisits
--         explore = sqrt (logParent / child ^. mctVisits)
--     selection =
--         -- traceShow ("select best move for", player)
--         let vv = eval <$> tt in
--         let vmax = maximum vv in
--         let Just i = findIndex (==vmax) vv in
--         let (aa,u:bb) = splitAt i tt in
--         expansion aa u bb
--     expansion aa u bb =
--         let (reward,v) = search u in
--         let m' = m & over mctVisits (+1) . over mctReward (addReward reward) in
--         (reward, Node (Left m') (aa <> [v] <> bb))
--     addReward aa bb =
--         totalArray $ zipWith (+) (elems aa) (elems bb)

-- treeItem (Node x _) = either Left (Right . fst) x

-- pp (Right (g,_)) = showGame g
-- pp (Left MCT{..}) =
--     show (_mctVisits, pct _mctVisits <$> elems _mctReward)
--     <> " " <> showGame _mctGame

-- showGame g = show (gamePlayer g) <> " " <> show (NE.toList $ gameMoves g)

-- pct d n = round $ 100 * n / d

-- mctSearchTree n g = do
--     gen <- newStdGen
--     let t = fmap Right . randomize gen $ gameTree g
--     pure $ iterate go t !! n
--   where
--     go t = snd $ search t

-- mctSearch n g = mctSearchTree n g <&> bestMove

-- bestMove (Node (Right _) _) = error "unsearched node"
-- bestMove (Node _ tt)
--     | null cands = error "no children"
--     | otherwise = lastMove $ maximumBy (comparing _mctVisits) cands
--   where
--     cands = lefts (view root <$> tt)

-- lastMove :: Game g => MCT g -> Move g
-- lastMove = NE.head . gameMoves . _mctGame

-- mctTest g n p =
--     mctSearchTree n g >>= out
--   where
--     out = traverse_ putStrLn . trim . lines . drawTree . fmap pp . prune p
--     trim = filter (any isAlpha)
