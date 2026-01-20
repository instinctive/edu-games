{-# LANGUAGE AllowAmbiguousTypes #-}

module MCT where

import Control.Concurrent.STM
import Control.Monad.Random.Class ( getRandomR )
import Data.Array

import Game

-- class (Game g, Eq (Player g)) => MCTS g where drawValue :: Double

data MCTSNode g = MCTSNode
    { _mGame     :: g
    , _mValue    :: TVar Double
    , _mTotal    :: TVar Double
    , _mChildren :: TVar (Array Int (MCTSNode g))
    }

initMCTS g = MCTSNode g
    <$> newTVar 0
    <*> newTVar 0
    <*> newTVar undefined

incrTotal MCTSNode{..} = atomically $ do
    t <- readTVar _mTotal
    writeTVar _mTotal (t+1)
    when (t == 0 && gameStatus _mGame == Nothing) do
        mm <- traverse initMCTS (nextGames _mGame)
        let ary = listArray (0, length mm - 1) mm
        writeTVar _mChildren ary
    pure t

search player node@(MCTSNode{..} :: MCTSNode g) = do
    total <- incrTotal node
    case gameStatus _mGame of
        Just r -> update r
        Nothing | total == 0 -> rollout _mGame >>= update
        Nothing -> explore total node >>= update
  where
    update r | isNothing player = pure r
    update r = pure r <* case r of
        Draw    -> addValue 0.5 -- (drawValue @g)
        (Win p) -> when (Just p == player) (addValue 1)
    addValue v = atomically $ modifyTVar' _mValue (+v)

explore (log -> logParent) MCTSNode{..} = do
    (ary,vv) <- atomically $ do
        ary <- readTVar _mChildren
        vv <- traverse (eval logParent) (elems ary)
        pure (ary,vv)
    let i = fst $ maximumBy (comparing snd) $ zip [0..] vv
    search (Just $ gamePlayer _mGame) (ary!i)

data Value a = Select a | Expand deriving (Eq,Ord,Show)

eval logParent (MCTSNode{..} :: MCTSNode g) = do
    total <- readTVar _mTotal
    if total == 0 then pure Expand else do
        value <- readTVar _mValue
        let exploit = value / total
        let explore = logParent / total
        pure . Select $ exploit + param * explore
  where
    param = sqrt 2

rollout g = gameStatus g & maybe next pure where
    next = pickOne (nextGames g) >>= rollout
    pickOne l = getRandomR (0, length l - 1) <&> (l!!)

mctTest g n | isJust (gameStatus g) = print $ gameStatus g
mctTest g n = do
    m <- atomically $ initMCTS g
    _ <- incrTotal m -- force children to be built
    for_ [1..n] \_ -> search Nothing m
    moves <- atomically do
        ary <- readTVar (_mChildren m)
        traverse getMove (elems ary)
    traverse_ print moves
  where
    getMove MCTSNode{..} = do
        total <- readTVar _mTotal
        if total == 0 then pure Expand else do
            value <- readTVar _mValue
            let move :| _ = gameMoves _mGame
            pure $ Select (move, value, total)
