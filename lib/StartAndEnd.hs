module StartAndEnd where

import Datastructures
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import HelperFunctions
import VisualHandler


toPlaying :: Game -> Game
toPlaying game = game {status = Playing}

toEnding :: Game -> Game
toEnding game = game {status = Won}

handleLevelSelectionInput :: Event -> Game -> Game
handleLevelSelectionInput ev game | isKey '1' ev = toPlaying (changePlayerInGame game (findStart (layout (levels game !! 0))))
                                  | isKey '2' ev = toPlaying (changePlayerInGame game (findStart (layout (levels game !! 1))))
                                  | isKey '3' ev = toPlaying (changePlayerInGame game (findStart (layout (levels game !! 2))))
                                  | isKey '4' ev = toPlaying (changePlayerInGame game (findStart (layout (levels game !! 3))))
                                  | isKey '5' ev = toPlaying (changePlayerInGame game (findStart (layout (levels game !! 4))))
                                  | isKey '6' ev = toPlaying (changePlayerInGame game (findStart (layout (levels game !! 5))))
                                  | isKey '7' ev = toPlaying (changePlayerInGame game (findStart (layout (levels game !! 6))))
                                  | isKey '8' ev = toPlaying (changePlayerInGame game (findStart (layout (levels game !! 7))))
                                  | isKey '9' ev = toPlaying (changePlayerInGame game (findStart (layout (levels game !! 8))))
                                  | isKey '0' ev = toPlaying (changePlayerInGame game (findStart (layout (levels game !! 9))))
                                  | otherwise = game


