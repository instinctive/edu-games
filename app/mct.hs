module Main where

import Quux

main = do
    [n] <- getArgs <&> map (read @Int)
    mctSearch (initTTT :: TTT P2) n
