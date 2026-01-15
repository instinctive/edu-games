{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module MCT where

import Control.Lens
import Control.Monad.Random  ( evalRand, runRand, getRandomR )
import System.Random         ( StdGen, newStdGen, split )
import System.Random.Shuffle ( shuffleM )

import qualified Data.Tree as DTree

import Total
import Game
import qualified TTT

-- #define ENUM(a) Eq a, Ord a, Bounded a, Enum a, Ix a, Show a

data Tree a = Tree a [Tree a] deriving Functor
deriving instance Show a => Show (Tree a)

drawTree = DTree.drawTree . cvt where
    cvt (Tree a tt) = DTree.Node a (cvt <$> tt)

prune 0 (Tree a _) = Tree a []
prune n (Tree a tt) = Tree a (prune (n-1) <$> tt)

gameTree g = Tree g (gameTree <$> nextGames g)

randomize rnd (Tree a tt) =
    Tree (a,r) (zipWith randomize rr tt')
  where
    r:rr = splitGens rnd
    (tt',r') = runRand (shuffleM tt) r
    splitGens rnd = let (a,b) = split rnd in a : splitGens b

type Reward g = UArray (Player g) Double

data MCT g = MCT
    { _mctPlayer  :: Player g -- who made the move leading to this position
    , _mctMove    :: Move g   -- what was that move
    , _mctPlays   :: Double   -- how many playouts from this position
    , _mctRewards :: Reward g -- what have been the rewards
    }
makeLenses ''MCT

deriving instance (Show g, Show (Move g), Show (Player g), Ix (Player g)) => Show (MCT g)

pickOne l = getRandomR (0, length l - 1) <&> (l!!)

rollout (g :: g) = case gameStatus g of
    Play  -> pickOne (nextGames g) >>= rollout
    Draw  -> pure $ totalArray (repeat draw)
    Win p -> pure $ totalArray (repeat 0) & set (totalAt p) 1
  where draw = 1 / fromIntegral (numPlayers @(Player g))

expand g = rollout g <&> \reward ->
    (reward, MCT
        { _mctPlayer  = gamePlayer g
        , _mctMove    = lastMove g
        , _mctPlays   = 1
        , _mctRewards = reward
        })

type SearchTree g = Tree (Either (MCT g) (g,StdGen))

search :: (Game g, PlayerId (Player g)) => SearchTree g -> (Reward g, SearchTree g)

search (Tree (Right (g,gen)) tt) =
    (reward, Tree (Left m) tt)
  where
    (reward,m) = evalRand (expand g) gen

search (Tree (Left m) tt)
    | null tt   = error "search reached terminal node"
    | null rr   = selection
    | otherwise = expansion ll t rr'
  where
    isLeftTree (Tree a _) = isLeft a
    (ll,rr@(~(t:rr'))) = span isLeftTree tt
    param     = sqrt 2
    player    = m ^. mctPlayer
    logParent = m ^. mctPlays . to log
    eval (Tree (Left child) _) = exploit + param * explore where
        exploit = child ^. mctRewards . totalAt player / child ^. mctPlays
        explore = sqrt (logParent / child ^. mctPlays)
    selection =
        -- traceShow ("select best move for", player)
        let vv = eval <$> tt in
        let vmax = maximum vv in
        let Just i = findIndex (==vmax) vv in
        let (aa,u:bb) = splitAt i tt in
        expansion aa u bb
    expansion aa u bb =
        let (reward,v) = search u in
        let m' = m & over mctPlays (+1) . over mctRewards (addReward reward) in
        (reward, Tree (Left m') (aa <> [v] <> bb))
    addReward aa bb =
        totalArray $ zipWith (+) (elems aa) (elems bb)

treeItem (Tree x _) = either Left (Right . fst) x

pp (Right _) = "Game"
pp (Left MCT{..}) = show _mctPlayer <> " " <> show _mctMove <> " " <> show (_mctPlays, elems _mctRewards)

mctSearch g n = do
    gen <- newStdGen
    let t = fmap Right . randomize gen $ gameTree g
    let go t = snd $ search t
    let t' = iterate go t !! n
    let trim = unlines . filter (not.(isInfixOf "Game")) . filter (any isAlpha) . lines
    let prt = putStrLn . trim . drawTree . fmap pp . prune 4
    prt t'
