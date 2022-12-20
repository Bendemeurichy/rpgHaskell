module HelperFunctions where

import Datastructures

findStart :: [TileLine] -> (Int, Int)
findStart tileLines = findStart' tileLines 0 0

findStart' :: [TileLine] -> Int -> Int -> (Int, Int)
findStart' [] _ _ = error "No start tile found"
findStart' (x:xs) y z = if Start `elem` (tiles x) then (z+1, y) else findStart' xs (y+1) z

findEnd :: [TileLine] -> (Int, Int)
findEnd tileLines = findEnd' tileLines 0 0

findEnd' :: [TileLine] -> Int -> Int -> (Int, Int)
findEnd' [] _ _ = error "No end tile found"
findEnd' (x:xs) y z = if End `elem` (tiles x) then (z+1, y) else findEnd' xs (y+1) z

tiles :: TileLine -> [Tile]
tiles (TileLine a) = a

changePlayerPosition :: Player -> (Int, Int) -> Player
changePlayerPosition player (x,y) = player {px = x, py = y}

changePlayerInGame :: Game -> (Int, Int) -> Game
changePlayerInGame game (x,y) = game {player = changePlayerPosition (player game) (x,y)}

heightCurrentLevel :: Game -> Int
heightCurrentLevel game = length (layout (levels game !! currentLevel game)) - 2

calculateHealth :: Player -> [String]
calculateHealth player =[ "full" |i<-[1..(php player `div` 10)]] ++ (["half" | php player `mod` 10 == 5])

inventorySize ::Int
inventorySize = 7

detectActions :: Game -> [Action]
detectActions game = concat [  eactions ent| ent <- detectEntities game] ++ concat [actions item | item <- detectItems game]

detectEntities :: Game -> [Entity]
detectEntities game = [ent | ent <- entities (levels game !! currentLevel game) , detectCoordinates (px (player game), py (player game)) (ex ent, ey ent)]

detectItems :: Game -> [Item]
detectItems game = [item | item <- items (levels game !! currentLevel game) , detectCoordinates (px (player game) , py (player game)) (x item, y item)]

detectCoordinates :: (Int,Int) -> (Int, Int) -> Bool
detectCoordinates (x1,y1) (x2,y2) | x1 == x2 && y1 == y2 = True
                                | x1+1 == x2 && y1 == y2 = True
                                | x1-1 == x2 && y1 == y2 = True
                                | x1 == x2 && y1+1 == y2 = True
                                | x1 == x2 && y1-1 == y2 = True
                                | otherwise = False

