module ActionHandler where

import Datastructures
import Data.Maybe (fromJust, isNothing)


inventorySize :: Int
inventorySize = 7

inventoryFull :: Player -> Bool
inventoryFull player = length (pinventory player) == inventorySize

inventoryFull' :: Player -> Bool
inventoryFull' = inventoryFull

findItem :: Game -> ID -> Level -> Item
findItem game (ID tid) level= head [it | it <- findItem' (ID tid) level,playerInItemRange game it ]

findItem' :: ID -> Level ->[Item]
findItem' (ID tid) level= [it | it <- Datastructures.items level, Datastructures.id it == tid]

findItemInInventory :: ID -> Player -> Item
findItemInInventory (ID tid) player= head [it | it <- pinventory player, Datastructures.id it == tid]

removeItemFromInventoryIfNecessary :: Game -> Player -> ID -> Player
removeItemFromInventoryIfNecessary game player id | useTimes item == Infinite = player
                                            | amountToInt  (useTimes item)-1 == 0 = removeItemFromInventory player id
                                            | otherwise = newPlayer{pinventory = pinventory newPlayer ++ [item{useTimes = Amount (amountToInt  (useTimes item) - 1)}]}
                                            where   item =findItemInInventory id player
                                                    newPlayer = removeItemFromInventory player id

removeItemFromInventory :: Player -> ID -> Player
removeItemFromInventory player id = player {pinventory = [it | it <- pinventory player, Datastructures.id it /= idtoString  id]}

findEntity :: Game ->ID -> Level -> Entity
findEntity game (ID tid) level= head [it | it <- findEntity' tid level,playerInEntityRange game it ]

findEntity' :: String -> Level -> [Entity]
findEntity' tid level= [it | it <- Datastructures.entities level, eid it == tid]


retrieveItem :: Game -> ID -> Level -> Game
retrieveItem game id level = game {player = (player game){pinventory = pinventory (player game) ++ [findItem game id level]}, levels = removeItemFromLevel game id}

removeItemFromLevel :: Game -> ID -> [Level]
removeItemFromLevel game id = replaceNth (levels game) (currentLevel game) (removeItemFromLevel' (levels game !! currentLevel game) id)

removeItemFromLevel' :: Level -> ID -> Level
removeItemFromLevel' level id = level {items = [it | it <- Datastructures.items level, Datastructures.id it /= idtoString  id]}

removeEntityFromLevel :: Game -> ID -> [Level]
removeEntityFromLevel game id = replaceNth (levels game) (currentLevel game) (removeEntityFromLevel' (levels game !! currentLevel game) id)



removeEntityFromLevel' :: Level -> ID -> Level
removeEntityFromLevel' level id = level {entities = [it | it <- Datastructures.entities level, eid it /= idtoString  id]}

addEntityToLevel :: Entity -> Game -> Game
addEntityToLevel entity game = game {levels = replaceNth (levels game) (currentLevel game) (addEntityToLevel' entity (levels game !! currentLevel game))}

addEntityToLevel' :: Entity -> Level -> Level
addEntityToLevel' entity level = level {entities = Datastructures.entities level ++ [entity]}

replaceNth :: [a] -> Int -> a -> [a]
replaceNth xs n x =take n xs ++ [x] ++ drop (n+1) xs


--move untill player is out of range of all entities and items
leave ::Game ->Player -> Level -> Player
leave game player level | isValidStep (px player + 2, py player+1) (layout level) game &&outOfRange (px player + 1, py player) level = player {px = px player + 1}
                        | isValidStep (px player , py player+1) (layout level) game &&outOfRange (px player - 1, py player) level = player {px = px player - 1}
                        | isValidStep (px player +1 , py player+2) (layout level) game &&outOfRange (px player, py player + 1) level = player {py = py player + 1}
                        | isValidStep (px player +1, py player) (layout level) game &&outOfRange (px player, py player - 1) level = player {py = py player - 1}
                        | otherwise = player

isFloor :: (Int, Int) -> [TileLine] -> Bool
isFloor (x, y) tilelines
  | tilelinetoTiles (reverse tilelines !! y) !! x == Floor = True
  | tilelinetoTiles (reverse tilelines !! y) !! x == Start = True
  | tilelinetoTiles (reverse tilelines !! y) !! x == End = True
  | otherwise = False

isValidStep :: (Int, Int) -> [TileLine] -> Game -> Bool
isValidStep (x, y) tilelines game
  | x<0 || y<0 || x>length (tilelinetoTiles (head tilelines)) - 2 || y>length tilelines - 2 = False
  | isFloor (x, y) tilelines && isDoorOpen (x, y) (levels game !! currentLevel game) = True
  | otherwise = False

isDoorOpen :: (Int, Int) -> Level -> Bool
isDoorOpen (x, y) level
  | null door = True
  | ex (head door) == (x - 1) && ey (head door) == (y - 1) && isNothing (evalue (head door)) = False
  | otherwise = True
  where
    door = findEntity' "door" level

outOfRange:: (Int,Int) -> Level -> Bool
outOfRange loc level = not (isLocInRangeItems loc (Datastructures.items level) || isLocInRangeEntities loc (Datastructures.entities level))

isLocInRangeItems :: (Int,Int) -> [Item] -> Bool
isLocInRangeItems loc items = or [isLocInRangeI loc item | item <- items]

isLocInRangeEntities :: (Int,Int) -> [Entity] -> Bool
isLocInRangeEntities loc entities = or [isLocInRangeE loc entity | entity <- entities]

isLocInRangeE :: (Int,Int) -> Entity -> Bool
isLocInRangeE (x,y) entity | x == ex entity && y == ey entity = True
                          | x == ex entity && y == ey entity + 1 = True
                          | x == ex entity && y == ey entity - 1 = True
                          | x == ex entity + 1 && y == ey entity = True
                          | x == ex entity - 1 && y == ey entity = True
                          | otherwise = False

isLocInRangeI :: (Int,Int) -> Item -> Bool
isLocInRangeI (tx,ty) item | tx == x item && ty == y item = True
                          | tx == x item && ty == y item + 1 = True
                          | tx == x item && ty == y item - 1 = True
                          | tx == x item + 1 && ty == y item = True
                          | tx == x item - 1 && ty == y item = True
                          | otherwise = False


useItem:: Game -> ID -> Game
useItem game entity = addEntityToLevel (findEntity game entity (levels game !! currentLevel game)){evalue=Just 0} (useItem' game entity)

useItem' :: Game -> ID -> Game
useItem' game entity = game {levels = removeEntityFromLevel game entity}

inventoryContains ::Player-> ID -> Bool
inventoryContains player id = pinventory player `contains` id

contains :: [Item] -> ID -> Bool
contains [] _ = False
contains (x:xs) (ID cid) = cid == Datastructures.id x || contains xs (ID cid)

increasePlayerHp :: Game -> Player -> ID -> Player
increasePlayerHp game player id= removeItemFromInventoryIfNecessary game  (player {php = php player + Datastructures.value (findItemInInventory id player)}) id

decreasePlayerHp ::Game -> Player -> ID -> Level -> Player
decreasePlayerHp game player id level= player {php = php player - fromJust(evalue (findEntity game id level))}

decreaseHp::Game->ID -> ID -> Level -> Player -> Entity
decreaseHp game entity item level player=  (findEntity game entity level){ehp = Just ( fromJust (ehp (findEntity game entity level)) - Datastructures.value (findItemInInventory item player))}

applyFunction :: Game -> Function -> Game
applyFunction game func | functionName func == ID "leave" = game {player = leave game (player game) (levels game !! currentLevel game)}
                        | functionName func == ID "increasePlayerHp" = game {player = increasePlayerHp game(player game) (argumentToId (head (arguments func)))}
                        | functionName func == ID "decreasePlayerHp" = game {player = decreasePlayerHp game (player game) (argumentToId (head (arguments func))) (levels game !! currentLevel game)}
                        | functionName func == ID "decreaseHp" = game {player =removeItemFromInventoryIfNecessary game (player game) (argumentToId (head(tail (arguments func)))) ,
                                                                        levels = replaceNth (levels game) (currentLevel game) (levels game !! currentLevel game){entities = [decreaseHp game (argumentToId (head (arguments func))) (argumentToId (head (tail (arguments func)))) (levels game !! currentLevel game) (player game)]}}
                        | functionName func == ID "retrieveItem" = retrieveItem game (argumentToId (head (arguments func))) (levels game !! currentLevel game)
                        | functionName func == ID "useItem" = useItem (game {player = removeItemFromInventoryIfNecessary game (player game) (argumentToId (head (arguments func)))}) (ID "door")
                        | otherwise = game

conditionTrue :: Game -> Function -> Bool
conditionTrue game func | functionName func == ID "inventoryFull" = inventoryFull (player game)
                    | functionName func == ID "inventoryContains" = inventoryContains (player game) (argumentToId (head (arguments func)))
                    | functionName func == ID "not" = not (conditionTrue game (argumentToFunction (head (arguments func))))
                    | otherwise = False

playerInEntityRange :: Game -> Entity -> Bool
playerInEntityRange game entity
  | px (player game) == ex entity && py (player game) == ey entity = True
  | px (player game) == ex entity - 1 && py (player game) == ey entity = True
  | px (player game) == ex entity + 1 && py (player game) == ey entity = True
  | px (player game) == ex entity && py (player game) == ey entity - 1 = True
  | px (player game) == ex entity && py (player game) == ey entity + 1 = True
  | otherwise = False

playerInItemRange :: Game -> Item -> Bool
playerInItemRange game item
  | px (player game) == x item && py (player game) == y item = True
  | px (player game) == x item - 1 && py (player game) == y item = True
  | px (player game) == x item + 1 && py (player game) == y item = True
  | px (player game) == x item && py (player game) == y item - 1 = True
  | px (player game) == x item && py (player game) == y item + 1 = True
  | otherwise = False

