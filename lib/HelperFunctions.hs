module HelperFunctions where

import Datastructures


--write a function to find the start tile in an array of tilelines
--write a function to find the end tile in an array of tilelines

findStart :: [TileLine] -> (Int, Int)
findStart tileLines = findStart' tileLines 0 0

findStart' :: [TileLine] -> Int -> Int -> (Int, Int)
findStart' [] _ _ = error "No start tile found"
findStart' (x:xs) y z = if Start `elem` (tiles x) then (z, y) else findStart' xs (y+1) z

findEnd :: [TileLine] -> (Int, Int)
findEnd tileLines = findEnd' tileLines 0 0

findEnd' :: [TileLine] -> Int -> Int -> (Int, Int)
findEnd' [] _ _ = error "No end tile found"
findEnd' (x:xs) y z = if End `elem` (tiles x) then (z, y) else findEnd' xs (y+1) z

tiles :: TileLine -> [Tile]
tiles (TileLine a) = a

changePlayerPosition :: Player -> (Int, Int) -> Player
changePlayerPosition player (x,y) = player {px = x+1, py = y}

changePlayerInGame :: Game -> (Int, Int) -> Game
changePlayerInGame game (x,y) = game {player = changePlayerPosition (player game) (x,y)}