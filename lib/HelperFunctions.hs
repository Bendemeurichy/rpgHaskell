module HelperFunctions where

import Datastructures
import Graphics.Gloss.Interface.IO.Game
import ActionHandler hiding (detectItems, detectAllActions, detectEntities, detectCoordinates) 
import Data.Maybe (isNothing)
import Data.List

findStart :: [TileLine] -> (Int, Int)
findStart tileLines = findCoordinate tileLines Start

findEnd :: [TileLine] -> (Int, Int)
findEnd tileLines = findCoordinate tileLines End

findCoordinate :: [TileLine] -> Tile -> (Int, Int)
findCoordinate l t = head [(x-1, y-1) | (y, line) <- zip [0 ..] (reverse l), x <- elemIndices t (tiles line)]

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
getNumber (EventKey (Char k) Graphics.Gloss.Interface.IO.Game.Down _ _ ) = [k]

