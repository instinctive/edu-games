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

prune 0 (Node a _) = Node a []
prune n (Node a tt) = Node a (prune (n-1) <$> tt)

gameTree g = Node g (gameTree <$> nextGames g)

randomize rnd (Node a tt) =
    Node (a,r) (zipWith randomize rr tt')
  where
    r:rr = splitGens rnd
    (tt',r') = runRand (shuffleM tt) r
    splitGens rnd = let (a,b) = split rnd in a : splitGens b

type Reward g = UArray (Player g) Double

data MCT g = MCT
    { _mctGame   :: g        -- current position
    , _mctVisits :: Double   -- how many playouts from this position
    , _mctReward :: Reward g -- what have been the rewards
    }
makeLenses ''MCT

deriving instance (Show g, Show (Move g), Show (Player g), Ix (Player g)) => Show (MCT g)

pickOne l = getRandomR (0, length l - 1) <&> (l!!)

rollout (g::g,gen) = ap (,) mct $ evalRand (go g) gen where
    go g = case gameStatus g of
        Play  -> pickOne (nextGames g) >>= go
        Draw  -> pure $ totalArray (repeat draw)
        Win p -> pure $ totalArray (repeat 0) & set (totalAt p) 1
    draw = 1 / fromIntegral (numPlayers @(Player g))
    mct reward = MCT
        { _mctGame   = g
        , _mctVisits = 1
        , _mctReward = reward
        }

type SearchTree g = Tree (Either (MCT g) (g,StdGen))

search :: (Game g, PlayerId (Player g)) => SearchTree g -> (Reward g, SearchTree g)

search node@(Node (Right ggen) tt) =
    rollout ggen <&> \m -> set root (Left m) node

search node@(Node (Left m) tt)
    | null tt   = terminal
    | null rr   = selection
    | otherwise = expansion ll t rr'
  where
    terminal =
        let plays = m ^. mctVisits in
        let reward = totalArray (m ^. mctReward . to elems . to (map (/plays))) in
        (reward, node & over (root._Left) (over mctVisits (+1) . over mctReward (addReward reward)))
    isLeftTree (Node a _) = isLeft a
    (ll,rr@(~(t:rr'))) = span isLeftTree tt
    param     = sqrt 2
    player    = m ^. mctGame . to gamePlayer
    logParent = m ^. mctVisits . to log
    eval (Node (Left child) _) = exploit + param * explore where
        exploit = child ^. mctReward . totalAt player / child ^. mctVisits
        explore = sqrt (logParent / child ^. mctVisits)
    selection =
        -- traceShow ("select best move for", player)
        let vv = eval <$> tt in
        let vmax = maximum vv in
        let Just i = findIndex (==vmax) vv in
        let (aa,u:bb) = splitAt i tt in
        expansion aa u bb
    expansion aa u bb =
        let (reward,v) = search u in
        let m' = m & over mctVisits (+1) . over mctReward (addReward reward) in
        (reward, Node (Left m') (aa <> [v] <> bb))
    addReward aa bb =
        totalArray $ zipWith (+) (elems aa) (elems bb)

treeItem (Node x _) = either Left (Right . fst) x

pp (Right (g,_)) = showGame g
pp (Left MCT{..}) =
    show (_mctVisits, pct _mctVisits <$> elems _mctReward)
    <> " " <> showGame _mctGame

showGame g = show (gamePlayer g) <> " " <> show (NE.toList $ gameMoves g)

pct d n = round $ 100 * n / d

mctSearch g n p = do
    gen <- newStdGen
    let t = fmap Right . randomize gen $ gameTree g
    let go t = snd $ search t
    let t' = iterate go t !! n
    let trim = unlines . filter (not.(isInfixOf "Game")) . filter (any isAlpha) . lines
    let prt = putStrLn . trim . drawTree . fmap pp . prune p
    prt t'
