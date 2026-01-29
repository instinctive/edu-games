{-# LANGUAGE FunctionalDependencies #-}

module TTT.UIText where

import Data.List.Split (chunksOf)

import Game
import TTT.Game

instance UIText TTT Int where
    showGame g    = showTTT g
    parseMove g s = readMaybe @Int s

showResult Draw = "Game is a draw."
showResult (Win p) = show p <> " has won."

showPlayer X = "\ESC[92mX\ESC[0m"
showPlayer O = "\ESC[91mO\ESC[0m"

showSq = either show showPlayer

showTTT TTT{..} =
    unlines $
    intersperse "---+---+---"
    [ intercalate "|"
      [ " " <> showSq sq <> " " | sq <- row ]
      | row <- chunksOf 3 _gBoard ]
    -- <>
    -- [ "Moves: " <> intercalate ", " (reverse $ show <$> N.toList _gMoves) ]
    <>
    [ _gStatus & maybe
      (show _gPlayer <> " to move:")
      showResult ]
