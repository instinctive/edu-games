module Main where

import GHC.Conc

import Quux

main = do
    [n] <- getArgs <&> map (read @Int)
    numcpu <- getNumProcessors
    numcap <- getNumCapabilities
    putStrLn $ "numProcessors   = " <> show numcpu
    putStrLn $ "numCapabilities = " <> show numcap
    mctSearchTest initChess n
