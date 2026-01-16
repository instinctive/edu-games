{-# LANGUAGE TemplateHaskell      #-}

module MCT where

import Control.Lens
import Control.Monad.Random.Class ( MonadRandom, getRandomR )
import Data.List.NonEmpty qualified as NE
import Data.Tree

import Game

data MCT g = MCT
    { _mctGame   :: g
    , _mctScore  :: Double
    , _mctVisits :: Double
    }
makeLenses ''MCT

initMCT g = MCT g 0 0

mkTree g = initMCT <$> tree g where
    tree  = ap Node (map tree . nextGames)

addResult player r (m :: MCT g) = m
    & over mctScore (+score)
    . over mctVisits (+1)
  where
    score = case r of
        Draw  -> drawValue @(Player g)
        Win p -> bool 0 1 $ p == player

data Child = Select Double | Expand deriving (Eq,Ord,Show)

eval logParent MCT{..}
    | _mctVisits == 0 = Expand -- Expand > Select _
    | otherwise       = Select $ exploit + param * explore
  where
    exploit = _mctScore / _mctVisits
    explore = sqrt (logParent / _mctVisits)
    param = sqrt 2

rollout g = gameStatus g & maybe next pure where
    next = pickOne (nextGames g) >>= rollout
    pickOne l = getRandomR (0, length l - 1) <&> (l!!)

search player node@(Node m@MCT{..} tt) = case gameStatus _mctGame of
    -- Terminal node
    Just r -> pure $ update m tt r
    -- Unexplored node
    Nothing | _mctVisits == 0 -> rollout _mctGame <&> update m tt
    -- Select "best" child
    Nothing -> do
        (r,child') <- search (gamePlayer _mctGame) child
        pure $ update m (aa <> [child'] <> bb) r
      where
        vv = eval (log _mctVisits) . rootLabel <$> tt
        i = fst $ maximumBy (comparing snd) $ zip [0..] vv
        (aa,child:bb) = splitAt i tt
  where
    update m tt r = (r, Node (m & addResult player r) tt)

mctSearch player = fmap snd . search player

bestMove (Node _ tt) =
    maximumBy (comparing _mctVisits) (rootLabel <$> tt)
    & NE.head . gameMoves . _mctGame

pp MCT{..} =
    [ show $ NE.head $ gameMoves _mctGame
    , show $ round _mctVisits
    , show $ round $ 100 * _mctScore / _mctVisits
    ]

tabulate stuff =
    [ intercalate " " $ zipWith align ww ss
    | ss <- stuff ]
  where
    ww = maximum . map length <$> transpose stuff
    align w s = replicate (w - length s) ' ' <> s

mctTest p g n = do
    t <- go n (mkTree g)
    traverse_ putStrLn $ tabulate $ pp . rootLabel <$> subForest t
    print $ bestMove t
  where
    go 0 t = pure t
    go n t = mctSearch p t >>= go (n-1)
