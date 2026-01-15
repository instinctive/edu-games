module Main where

import Quux

main = do
    [n,p] <- getArgs <&> map (read @Int)
    mctSearch (initTTT :: TTT P2) n p
