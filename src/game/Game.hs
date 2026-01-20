{-# LANGUAGE FunctionalDependencies #-}

module Game where

data Result p = Draw | Win p deriving (Eq,Ord,Show)

class Game g p m | g -> p, g -> m where
    gamePlayer :: g -> p
    gameStatus :: g -> Maybe (Result p)
    gameMoves  :: g -> NonEmpty m
    nextGames  :: g -> [g]
