module ActionHandler where

import Datastructures
import Data.Maybe (fromJust)

inventorySize :: Int
inventorySize = 7

inventoryFull :: Player -> Bool
inventoryFull player = length (pinventory player) == inventorySize

inventoryFull' :: Player -> Bool
inventoryFull' = inventoryFull

findItem :: ID -> Level ->Item
findItem (ID tid) level= head [it | it <- Datastructures.items level, Datastructures.id it == tid]

findItemInInventory :: ID -> Player -> Item
findItemInInventory (ID tid) player= head [it | it <- pinventory player, Datastructures.id it == tid]

removeItemFromInventoryIfNecessary :: Game -> Player -> ID -> Player
removeItemFromInventoryIfNecessary game player id | useTimes item == Infinite = player
                                            | amountToInt  (useTimes item) == 0 = removeItemFromInventory player id
                                            | otherwise = newPlayer{pinventory = pinventory newPlayer ++ [item{useTimes = Amount (amountToInt  (useTimes item) - 1)}]}
                                            where   item =findItem id (levels game !! currentLevel game)
                                                    newPlayer = removeItemFromInventory player id

removeItemFromInventory :: Player -> ID -> Player
removeItemFromInventory player id = player {pinventory = [it | it <- pinventory player, Datastructures.id it /= idtoString  id]}

findEntity :: ID -> Level -> [Entity]
findEntity (ID tid) level= [it | it <- Datastructures.entities level, eid it == tid]



retrieveItem :: Game -> ID -> Level -> Game
retrieveItem game id level = game {player = (player game){pinventory = pinventory (player game) ++ [findItem id level]}, levels = removeItemFromLevel game id}

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

leave :: Player -> Player
leave player = player {php =0}

useItem:: Game -> ID -> Game
useItem game entity = addEntityToLevel (head (findEntity entity (levels game !! currentLevel game))){evalue=Just 0} (useItem' game entity)

useItem' :: Game -> ID -> Game
useItem' game entity = game {levels = removeEntityFromLevel game entity}

inventoryContains ::Player-> ID -> Bool
inventoryContains player id = pinventory player `contains` id

contains :: [Item] -> ID -> Bool
contains [] _ = False
contains (x:xs) (ID cid) = cid == Datastructures.id x || contains xs (ID cid)

increasePlayerHp :: Player -> ID -> Player
increasePlayerHp player id= removeItemFromInventory (player {php = php player + Datastructures.value (findItemInInventory id player)}) id

decreasePlayerHp :: Player -> ID -> Level -> Player
decreasePlayerHp player id level= player {php = php player - fromJust(evalue (head (findEntity id level)))}

decreaseHp::ID -> ID -> Level -> Player -> Entity
decreaseHp entity item level player= (head (findEntity entity level)){ehp = Just  (fromJust (ehp(head (findEntity entity level))) - Datastructures.value (findItemInInventory item player))}

applyFunction :: Game -> Function -> Game
applyFunction game func | functionName func == ID "leave" = game {player = leave (player game)}
                        | functionName func == ID "increasePlayerHp" = game {player = increasePlayerHp (player game) (argumentToId (head (arguments func)))}
                        | functionName func == ID "decreasePlayerHp" = game {player = decreasePlayerHp (player game) (argumentToId (head (arguments func))) (levels game !! currentLevel game)}
                        | functionName func == ID "decreaseHp" = game {levels = replaceNth (levels game) (currentLevel game) (levels game !! currentLevel game){entities = [decreaseHp (argumentToId (head (arguments func))) (argumentToId (head (tail (arguments func)))) (levels game !! currentLevel game) (player game)]}}
                        | functionName func == ID "retrieveItem" = retrieveItem game (argumentToId (head (arguments func))) (levels game !! currentLevel game)
                        | functionName func == ID "useItem" = useItem (game {player = removeItemFromInventory (player game) (argumentToId (head (arguments func)))}) (ID "door")
                        | otherwise = game

conditionTrue :: Game -> Function -> Bool
conditionTrue game func | functionName func == ID "inventoryFull" = inventoryFull (player game)
                    | functionName func == ID "inventoryContains" = inventoryContains (player game) (argumentToId (head (arguments func)))
                    | functionName func == ID "not" = not (conditionTrue game (argumentToFunction (head (arguments func))))
                    | otherwise = False