module HelperFunctions where

import Datastructures
import Graphics.Gloss.Interface.IO.Game
import ActionHandler (conditionTrue, findEntity)
import Data.Maybe (isNothing)

findStart :: [TileLine] -> (Int, Int)
findStart tileLines = findStart' (reverse tileLines) 0 0

findStart' :: [TileLine] -> Int -> Int -> (Int, Int)
findStart' [] _ _ = error "No start tile found"
findStart' (x:xs) y z = if Start `elem` tiles x then (z, y-1) else findStart' xs (y+1) z

findEnd :: [TileLine] -> (Int, Int)
findEnd tileLines = findEnd' (reverse tileLines) 0 0

findEnd' :: [TileLine] -> Int -> Int -> (Int, Int)
findEnd' [] _ _ = error "No end tile found"
findEnd' (x:xs) y z = if End `elem` tiles x then (z, y-1) else findEnd' xs (y+1) z

isEndingTile :: (Int, Int) -> [TileLine] -> Bool
isEndingTile (x,y) tileLines | x == fst ending && y == snd ending = True
                             | otherwise = False
                             where ending = findEnd tileLines

tiles :: TileLine -> [Tile]
tiles (TileLine a) = a

changePlayerPosition :: Player -> (Int, Int) -> Player
changePlayerPosition player (x,y) = player {px = x, py = y}

changePlayerInGame :: Game -> (Int, Int) -> Game
changePlayerInGame game (x,y) = game {player = changePlayerPosition (player game) (x,y)}

calculateHealth :: Player -> [String]
calculateHealth player =[ "full" |i<-[1..(php player `div` 10)]] ++ (["half" | php player `mod` 10 == 5])

detectAllActions :: Game -> [Action]
detectAllActions game = concat [  eactions ent| ent <- detectEntities game] ++ concat [actions item | item <- detectItems game]

detectActions :: Game -> [Action]
detectActions game = [action | action <- detectAllActions game, checkCondition game (condition action) ]

checkCondition :: Game -> [Function] -> Bool
checkCondition game conditions | null conditions = True
                            |otherwise = conditionTrue game (head conditions) && checkCondition game (tail conditions )



detectEntities :: Game -> [Entity]
detectEntities game = [ent | ent <- entities (levels game !! currentLevel game) , detectCoordinates (px (player game), py (player game)) (ex ent, ey ent)]

detectItems :: Game -> [Item]
detectItems game = [item | item <- items (levels game !! currentLevel game) , detectCoordinates (px (player game) ,py (player game)) (x item, y item)]

detectCoordinates :: (Int,Int) -> (Int, Int) -> Bool
detectCoordinates (x1,y1) (x2,y2) | x1 == x2 && y1 == y2 = True
                                | x1+1 == x2 && y1 == y2 = True
                                | x1-1 == x2 && y1 == y2 = True
                                | x1 == x2 && y1+1 == y2 = True
                                | x1 == x2 && y1-1 == y2 = True
                                | otherwise = False

isFloor :: (Int, Int) -> [TileLine] -> Bool
isFloor (x,y) tilelines | tilelinetoTiles (reverse tilelines !! y) !! x == Floor = True
                        | tilelinetoTiles (reverse tilelines !! y) !! x == Start = True
                        | tilelinetoTiles (reverse tilelines !! y) !! x == End = True
                       | otherwise = False

isValidStep :: (Int, Int) -> [TileLine] -> Game -> Bool
isValidStep (x,y) tilelines game | isFloor (x,y) tilelines && isDoorOpen (x,y) (levels game !! currentLevel game) = True
                            | otherwise = False

isDoorOpen :: (Int, Int) -> Level -> Bool
isDoorOpen (x,y) level | ex door == (x-1) && ey door == (y-1) && isNothing(evalue door)  = False
                        | otherwise = True
    where door = findEntity (ID "door") level


-- detect if key pressed is one of special keys
isSpecialKey :: SpecialKey -> Event -> Bool
isSpecialKey k1 (EventKey (SpecialKey k2) Graphics.Gloss.Interface.IO.Game.Down _ _) = k1 == k2
isSpecialKey _ _ = False

-- detect if key pressed is a normal letter
isKey :: Char -> Event -> Bool
isKey k1 (EventKey (Char k2) Graphics.Gloss.Interface.IO.Game.Down _ _) = k1 == k2
isKey _ _ = False

-- detect if key pressed is a number
isNumber :: Event -> Bool
isNumber (EventKey (Char k) Graphics.Gloss.Interface.IO.Game.Down _ _) = k `elem` ['0'..'9']
isNumber _ = False

getNumber :: Event -> String
getNumber (EventKey (Char '1') _ _ _) = "1"
getNumber (EventKey (Char '2') _ _ _) = "2"
getNumber (EventKey (Char '3') _ _ _) = "3"
getNumber (EventKey (Char '4') _ _ _) = "4"
getNumber (EventKey (Char '5') _ _ _) = "5"
getNumber (EventKey (Char '6') _ _ _) = "6"
getNumber (EventKey (Char '7') _ _ _) = "7"
getNumber (EventKey (Char '8') _ _ _) = "8"
getNumber (EventKey (Char '9') _ _ _) = "9"
getNumber (EventKey (Char '0') _ _ _) = "0"
getNumber _ = ""