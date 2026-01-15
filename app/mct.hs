module Main where

import Quux

main = do
    [n,p] <- getArgs <&> map (read @Int)
    mctTest (initTTT :: TTT P2) n p
