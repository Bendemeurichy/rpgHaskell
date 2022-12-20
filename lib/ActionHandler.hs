module ActionHandler where

import Datastructures
import Data.Maybe (fromJust)
import HelperFunctions

inventoryFull :: Player -> Bool
inventoryFull player = length (pinventory player) == inventorySize

inventoryFull' :: Player -> Bool
inventoryFull' = inventoryFull

findItem :: ID -> Level ->Item
findItem (ID tid) level= head [it | it <- Datastructures.items level, Datastructures.id it == tid]

findEntity :: ID -> Level -> Entity
findEntity (ID tid) level= head [it | it <- Datastructures.entities level, eid it == tid]

retrieveItem :: Player -> ID -> Level -> Player
retrieveItem player id level = player {pinventory = pinventory player ++ [findItem id level]}

leave :: Player -> Player
leave player = player {php =0}

inventoryContains ::Player-> ID -> Bool
inventoryContains player id = pinventory player `contains` id

contains :: [Item] -> ID -> Bool
contains [] _ = False
contains (x:xs) (ID cid) = cid == Datastructures.id x || contains xs (ID cid)

increasePlayerHp :: Player -> ID -> Level -> Player
increasePlayerHp player id level = player {php = php player + Datastructures.value (findItem id level)}

decreasePlayerHp :: Player -> ID -> Level -> Player
decreasePlayerHp player id level= player {php = php player - fromJust(evalue (findEntity id level))}

decreaseHp::ID -> ID -> Level -> Entity
decreaseHp entity item level = (findEntity entity level){ehp = Just  (fromJust (ehp (findEntity entity level)) - Datastructures.value (findItem item level))}

applyFunction :: Game -> Function -> Game
applyFunction game func = game