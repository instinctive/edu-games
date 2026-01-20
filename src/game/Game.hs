{-# LANGUAGE FunctionalDependencies #-}

module Game where

class Eq p => Result r p | r -> p where
    resultValue :: r -> p -> Double

data Result2P p = Draw | Win p deriving (Eq,Ord,Show)
instance Eq p => Result (Result2P p) p where
    resultValue Draw _ = 0.5
    resultValue (Win a) b = bool 0 1 (a == b)

class (Eq m, Eq r) => Game g p m r | g -> p, g -> m, g -> r where
    gamePlayer   :: g -> p
    gameStatus   :: g -> Maybe r
    gameMoves    :: g -> NonEmpty m
    gameChildren :: g -> [g]

class Search s m | s -> m where
    startSearch  :: s -> IO ()
    setIsRunning :: s -> Bool -> IO ()
    makeBestMove :: s -> IO m
    makeThisMove :: s -> m -> IO Bool
    searchCount  :: s -> IO Int
