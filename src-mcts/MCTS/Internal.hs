{-# LANGUAGE UndecidableInstances #-}

module MCTS.Internal where

import Control.Concurrent.Async ( withAsync )
import Control.Concurrent.STM
import Control.Monad.Random.Class ( getRandomR )
import Data.Array

import Game

-- | How many rollouts a node must have before being expanded.
kExpand  = 2 :: Double

-- | The exploit vs explore parameter from the UCB1 formula.
kExplore = sqrt 2 :: Double

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
    when (t == kExpand && gameStatus _mGame == Nothing) do
        mm <- traverse initMCTS (gameChildren _mGame)
        let ary = listArray (0, length mm - 1) mm
        writeTVar _mChildren ary
    pure t

search player node@MCTSNode{..} = do
    total <- incrTotal node
    case gameStatus _mGame of
        Just r -> update r
        Nothing | total < kExpand -> rollout _mGame >>= update
        Nothing -> explore total node >>= update
  where
    update r = player & maybe (pure r) (addValue r . resultValue r)
    addValue r v = do
        atomically $ modifyTVar' _mValue (+v)
        pure r

explore (log -> logParent) MCTSNode{..} = do
    (ary,vv) <- atomically $ do
        ary <- readTVar _mChildren
        vv <- traverse (eval logParent) (elems ary)
        pure (ary,vv)
    let i = fst $ maximumBy (comparing snd) $ zip [0..] vv
    search (Just $ gamePlayer _mGame) (ary!i)

data Value a = Select !Double | Expand deriving (Eq,Ord,Show)

eval logParent (MCTSNode{..} :: MCTSNode g) = do
    total <- readTVar _mTotal
    if total == 0 then pure Expand else do
        value <- readTVar _mValue
        let exploit = value / total
        let explore = logParent / total
        pure . Select $ exploit + kExplore * explore

rollout g = gameStatus g & maybe next pure where
    next = pickOne (gameChildren g) >>= rollout
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
        if total == 0 then pure Nothing else do
            value <- readTVar _mValue
            let move :| _ = gameMoves _mGame
            pure $ Just (move, value, total)

-- Search instance

data MCTSearch g = MCTSearch
    { _sRoot    :: TVar (MCTSNode g)
    , _sRunning :: TVar Bool
    }

mctStart MCTSearch{..} = do
    forever go
  where
    go = getRoot >>= search Nothing
    getRoot = atomically $ readTVar _sRunning >>= bool retry (readTVar _sRoot)

mctRunning MCTSearch{..} b = atomically $ writeTVar _sRunning b

mctCount MCTSearch{..} = atomically do
    MCTSNode{..} <- readTVar _sRoot
    readTVar _mTotal <&> floor

getMove MCTSNode{..} = let move :| _ = gameMoves _mGame in move

mctThis MCTSearch{..} move = atomically do
    MCTSNode{..} <- readTVar _sRoot
    children <- readTVar _mChildren
    case find ((==move).getMove.snd) $ zip [0..] $ elems children of
        Nothing -> pure False
        Just (i,_) -> writeTVar _sRoot (children!i) >> pure True

mctBest MCTSearch{..} = atomically do
    MCTSNode{..} <- readTVar _sRoot
    children <- readTVar _mChildren
    let getTotal MCTSNode{..} = readTVar _mTotal
    totals <- traverse getTotal $ elems children
    let (_,i) = maximum $ zip totals [0..]
    writeTVar _sRoot (children!i)
    pure $ getMove (children!i)

mctGame MCTSearch{..} = atomically $ readTVar _sRoot <&> _mGame

instance (Result r p, Game g p m r) => Search (MCTSearch g) g m where
    searchGame   = mctGame
    startSearch  = mctStart
    setIsRunning = mctRunning
    makeBestMove = mctBest
    makeThisMove = mctThis
    searchCount  = mctCount

initMCTSearch g = atomically $ do
    m <- initMCTS g
    MCTSearch <$> newTVar m <*> newTVar True

mctSearchTest :: (Game g p m r, Result r p, Show m) => g -> Int -> IO ()
mctSearchTest g n = do
    s <- initMCTSearch g
    withAsync (startSearch s) \_ -> go s
  where
    go :: (Game g p m r, Result r p, Show m) => MCTSearch g -> IO ()
    go s = do
        threadDelay 100000 -- 1/10 second
        count <- searchCount s
        if count < n then do
            putStrLn $ show count <> " searches"
            go s
          else do
            setIsRunning s False
            count <- searchCount s
            putStrLn $ show count <> " searches"
            root <- atomically $ readTVar (_sRoot s)
            children <- atomically $ readTVar (_mChildren root)
            moves <- for (elems children) \MCTSNode{..} -> do
                let m = lastMove _mGame
                value <- atomically $ readTVar _mValue
                total <- atomically $ readTVar _mTotal
                pure (m,100 * value/total)
            traverse_ print $ sortBy (comparing $ Down . snd) moves
            move <- makeBestMove s
            print move

lastMove :: Game g p m r => g -> m
lastMove g = let (m:|_) = gameMoves g in m
