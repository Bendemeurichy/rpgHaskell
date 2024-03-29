module ActionHandler where

import Datastructures
import Data.Maybe (fromJust, isNothing)

------------------------------------------------------------------------------------------------
--module to handle functions called in the config files + helper functions for the functions
------------------------------------------------------------------------------------------------


--variable to check inventorysize
inventorySize :: Int
inventorySize = 7

--gamefunctions that match the ones in the config files

inventoryFull :: Player -> Bool
inventoryFull player = length (pinventory player) == inventorySize

--move 1 step to put player out of range of all entities and items if possible
leave ::Game ->Player -> Level -> Player
leave game player level | isValidStep (px player + 2, py player+1) (layout level) game &&outOfRange (px player + 1, py player) level = player {px = px player + 1}
                        | isValidStep (px player , py player+1) (layout level) game &&outOfRange (px player - 1, py player) level = player {px = px player - 1}
                        | isValidStep (px player +1 , py player+2) (layout level) game &&outOfRange (px player, py player + 1) level = player {py = py player + 1}
                        | isValidStep (px player +1, py player) (layout level) game &&outOfRange (px player, py player - 1) level = player {py = py player - 1}
                        | otherwise = player

useItem:: Game -> ID -> Game
useItem game entityid = if isNothing (evalue entity) then addEntityToLevel entity {evalue=Just 0} (useItem' game entityid) else game
                      where entity = findEntity game entityid (levels game !! currentLevel game)


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


--managing functions that match gamefunction ids and arguments to the correct function
applyFunction :: Game -> Function -> Game
applyFunction game func | functionName func == ID "leave" = game {player = leave game (player game) (levels game !! currentLevel game)}
                        | functionName func == ID "increasePlayerHp" = game {player = increasePlayerHp game(player game) (argumentToId (head (arguments func)))}
                        | functionName func == ID "decreasePlayerHp" = game {player = decreasePlayerHp game (player game) (argumentToId (head (arguments func))) (levels game !! currentLevel game)}
                        | functionName func == ID "decreaseHp" = game {player =removeItemFromInventoryIfNecessary game (player game) (argumentToId (head(tail (arguments func)))) ,
                                                                        levels = replaceNth (levels game) (currentLevel game) (levels game !! currentLevel game){entities = decreaseHp game (argumentToId (head (arguments func))) (argumentToId (head (tail (arguments func)))) (levels game !! currentLevel game) (player game) : [e | e <- entities (levels game !! currentLevel game), e /= findEntity game (argumentToId (head (arguments func))) (levels game !! currentLevel game)]}}
                        | functionName func == ID "retrieveItem" = retrieveItem game (argumentToId (head (arguments func))) (levels game !! currentLevel game)
                        | functionName func == ID "useItem" = useItem (game {player = removeItemFromInventoryIfNecessary game (player game) (argumentToId (head (arguments func)))}) (ID "door")
                        | otherwise = game

conditionTrue :: Game -> Function -> Bool
conditionTrue game func | functionName func == ID "inventoryFull" = inventoryFull (player game)
                    | functionName func == ID "inventoryContains" = inventoryContains (player game) (argumentToId (head (arguments func)))
                    | functionName func == ID "not" = not (conditionTrue game (argumentToFunction (head (arguments func))))
                    | otherwise = False


--helper functions to get game functions to execute properly

findItem :: Game -> ID -> Level -> Item
findItem game (ID tid) level = head [it | it <- findItem' (ID tid) level, playerInItemRange game it]

findItem' :: ID -> Level -> [Item]
findItem' (ID tid) level = [it | it <- Datastructures.items level, Datastructures.id it == tid]

findItemInInventory :: ID -> Player -> Item
findItemInInventory (ID tid) player = head [it | it <- pinventory player, Datastructures.id it == tid]

removeItemFromInventoryIfNecessary :: Game -> Player -> ID -> Player
removeItemFromInventoryIfNecessary game player id
  | useTimes item == Infinite = player
  | amountToInt (useTimes item) - 1 == 0 = removeItemFromInventory player id
  | otherwise = newPlayer {pinventory = pinventory newPlayer ++ [item {useTimes = Amount (amountToInt (useTimes item) - 1)}]}
  where
    item = findItemInInventory id player
    newPlayer = removeItemFromInventory player id

removeItemFromInventory :: Player -> ID -> Player
removeItemFromInventory player id = player {pinventory = [it | it <- pinventory player, Datastructures.id it /= idtoString id]}

findEntity :: Game -> ID -> Level -> Entity
findEntity game (ID tid) level = head [it | it <- findEntity' (ID tid) level, playerInEntityRange game it]

findEntity' :: ID -> Level -> [Entity]
findEntity' (ID tid) level = [it | it <- Datastructures.entities level, eid it == tid]

retrieveItem :: Game -> ID -> Level -> Game
retrieveItem game id level = game {player = (player game) {pinventory = pinventory (player game) ++ [findItem game id level]}, levels = removeItemFromLevel game id}

removeItemFromLevel :: Game -> ID -> [Level]
removeItemFromLevel game id = replaceNth (levels game) (currentLevel game) (removeItemFromLevel' (levels game !! currentLevel game) (findItem game id (levels game !! currentLevel game)))

removeItemFromLevel' :: Level -> Item -> Level
removeItemFromLevel' level item = level {items = [it | it <- Datastructures.items level, it /= item]}

removeEntityFromLevel :: Game -> ID -> [Level]
removeEntityFromLevel game id = replaceNth (levels game) (currentLevel game) (removeEntityFromLevel' (levels game !! currentLevel game) (findEntity game id (levels game !! currentLevel game)))

removeEntityFromLevel' :: Level -> Entity -> Level
removeEntityFromLevel' level entity = level {entities = [it | it <- Datastructures.entities level, ename it /= ename entity]}

addEntityToLevel :: Entity -> Game -> Game
addEntityToLevel entity game = game {levels = replaceNth (levels game) (currentLevel game) (addEntityToLevel' entity (levels game !! currentLevel game))}

addEntityToLevel' :: Entity -> Level -> Level
addEntityToLevel' entity level = level {entities = Datastructures.entities level ++ [entity]}

--replace the nth element of a list with a new element
replaceNth :: [a] -> Int -> a -> [a]
replaceNth xs n x = take n xs ++ [x] ++ drop (n + 1) xs

useItem' :: Game -> ID -> Game
useItem' game entity = game {levels = removeEntityFromLevel game entity}

--helper functions to determine if location can be moved to

isFloor :: (Int, Int) -> [TileLine] -> Bool
isFloor (x, y) tilelines
  | tilelinetoTiles (reverse tilelines !! y) !! x == Floor = True
  | tilelinetoTiles (reverse tilelines !! y) !! x == Start = True
  | tilelinetoTiles (reverse tilelines !! y) !! x == End = True
  | otherwise = False

isValidStep :: (Int, Int) -> [TileLine] -> Game -> Bool
isValidStep (x, y) tilelines game
  | x < 0 || y < 0 || x > length (tilelinetoTiles (head tilelines)) - 2 || y > length tilelines - 2 = False
  | isFloor (x, y) tilelines && isDoorOpen (x, y) (levels game !! currentLevel game) = True
  | otherwise = False

isDoorOpen :: (Int, Int) -> Level -> Bool
isDoorOpen (x, y) level
  | null door = True
  | ex (head door) == (x - 1) && ey (head door) == (y - 1) && isNothing (evalue (head door)) = False
  | otherwise = True
  where
    door = findEntity' (ID "door") level

-- helper functions to check if player is in range of an item or entity or the other way around

--is location in range of item
isLocInRangeI :: (Int, Int) -> Item -> Bool
isLocInRangeI (tx, ty) item
  | tx == x item && ty == y item = True
  | tx == x item && ty == y item + 1 = True
  | tx == x item && ty == y item - 1 = True
  | tx == x item + 1 && ty == y item = True
  | tx == x item - 1 && ty == y item = True
  | otherwise = False

--is location out of range of both all items and all entities
outOfRange :: (Int, Int) -> Level -> Bool
outOfRange loc level = not (isLocInRangeItems loc (Datastructures.items level) || isLocInRangeEntities loc (Datastructures.entities level))

isLocInRangeItems :: (Int, Int) -> [Item] -> Bool
isLocInRangeItems loc items = or [isLocInRangeI loc item | item <- items]

isLocInRangeEntities :: (Int, Int) -> [Entity] -> Bool
isLocInRangeEntities loc entities = or [isLocInRangeE loc entity | entity <- entities]

-- is location in range of entity
isLocInRangeE :: (Int, Int) -> Entity -> Bool
isLocInRangeE (x, y) entity
  | x == ex entity && y == ey entity = True
  | x == ex entity && y == ey entity + 1 = True
  | x == ex entity && y == ey entity - 1 = True
  | x == ex entity + 1 && y == ey entity = True
  | x == ex entity - 1 && y == ey entity = True
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