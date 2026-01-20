{-# LANGUAGE FunctionalDependencies #-}

module Game where

data Result p = Draw | Win p deriving (Eq,Ord,Show)

class Game g p m | g -> p, g -> m where
    gamePlayer :: g -> p
    gameStatus :: g -> Maybe (Result p)
    gameMoves  :: g -> NonEmpty m
    nextGames  :: g -> [g]

class Search s m | s -> m where
    startSearch  :: s -> IO ()
    setIsRunning :: s -> Bool -> IO ()
    makeBestMove :: s -> IO m
    makeThisMove :: s -> m -> IO Bool
    searchCount  :: s -> IO Int
