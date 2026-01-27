module Main where

import GHC.Conc

import MCTS
import TTT
import Chess

main = do
    [game, read @Int -> n] <- getArgs
    numcpu <- getNumProcessors
    numcap <- getNumCapabilities
    putStrLn $ "numProcessors   = " <> show numcpu
    putStrLn $ "numCapabilities = " <> show numcap
    case game of
        "ttt"   -> mctSearchTest initTTT n
        "chess" -> mctSearchTest initChess n
