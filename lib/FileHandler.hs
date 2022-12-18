{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module FileHandler where

import Datastructures
import JsonParser
import Text.Parsec
import System.IO
import Control.Exception (throw)
import GHC.IO (unsafePerformIO)
import Data.Maybe (fromJust, isNothing)
import GHC.TopHandler (runIO)
import System.Posix.Internals (withFilePath)

constructLevel :: String -> IO Game
constructLevel levelname = do
    file <- readFile ("levels/" ++ levelname ++ ".txt")
    let json = parse parseJSON "" file
    case
        json of
            Prelude.Left err -> error $ show err
            Prelude.Right json -> do
                print json
                let game = constructGameFromJSON initGame json
                return game

startGame :: String -> Game
{-# NOINLINE startGame #-}
startGame levelname = unsafePerformIO (constructLevel levelname)

findMaybePair :: ID -> [Pair] -> Maybe Pair
findMaybePair _ [] = Nothing
findMaybePair id (x:xs) = if id == fstp x then Just x else findMaybePair id xs

findPair :: ID -> [Pair] -> Pair
findPair id pairs = fromJust (findMaybePair id pairs)

constructGameFromJSON :: Game -> JSON -> Game
constructGameFromJSON initGame (Object pairs) = initGame{ levels = constructLevelsFromJSON initLevels (sndp(findPair (ID "levels") pairs))}

constructLevelsFromJSON :: [Level] -> JSON -> [Level]
constructLevelsFromJSON initLevels (Object pairs) = [constructLevelFromJSON initLevel level | level <- levels]
    where levels = jsonToArray (sndp (findPair (ID "levels") pairs))

constructLevelFromJSON :: Level -> JSON -> Level
constructLevelFromJSON initLevel (Object pairs) = initLevel{ player = constructPlayerFromJSON initPlayer (findPair (ID "player") pairs)
                                                        , entities = constructEntitiesFromJSON initEntities(findPair (ID "entities") pairs)
                                                        , items = constructItemsFromJSON initItems (findPair (ID "items") pairs)
                                                        , layout = jsonToLayout  (sndp (findPair (ID "layout") pairs))
                                                        }


constructPlayerFromJSON :: Player -> Pair -> Player
constructPlayerFromJSON initplayer (Pair _ (Object pairs)) = initplayer{ px = jsonToInt (sndp (findPair (ID "x") pairs))
                                                                      , py =  jsonToInt (sndp (findPair (ID "y") pairs))
                                                                      , php = jsonToInt (sndp (findPair (ID "hp") pairs))
                                                                      , pinventory = constructItemsFromJSON initItems (findPair (ID "inventory") pairs)
                                                                      }

constructItemsFromJSON :: [Item] -> Pair -> [Item]
constructItemsFromJSON inititems (Pair _ (Array items)) = [constructItemFromJSON item | item <- items]

constructItemFromJSON :: JSON -> Item
constructItemFromJSON (Object item) = Item{ Datastructures.id = jsonToString (sndp (findPair (ID "id") item))
                                         , x = jsonToInt (sndp (findPair (ID "x") item))
                                         , y = jsonToInt (sndp (findPair (ID "y") item))
                                         , name = jsonToString (sndp (findPair (ID "name") item))
                                         , description = jsonToString (sndp (findPair (ID "description") item))
                                         , useTimes = jsonToUseTimes (sndp (findPair (ID "useTimes") item))
                                         , value = jsonToInt (sndp (findPair (ID "value") item))
                                         , actions = jsonToActions (sndp (findPair (ID "actions") item))
                                         }

constructEntitiesFromJSON :: [Entity] -> Pair -> [Entity]
constructEntitiesFromJSON initentities (Pair _ (Array entities)) = [constructEntityFromJSON entity | entity <- entities]

constructEntityFromJSON :: JSON -> Entity
constructEntityFromJSON (Object entity) = Entity{ eid = jsonToString (sndp (findPair (ID "id") entity))
                                       , ex = jsonToInt (sndp (findPair (ID "x") entity))
                                       , ey = jsonToInt (sndp (findPair (ID "y") entity))
                                       , ename = jsonToString (sndp (findPair (ID "name") entity))
                                       , edescription = jsonToString (sndp (findPair (ID "description") entity))
                                       , eDirection = if isNothing (findMaybePair (ID "direction") entity) then Nothing else Just (jsonToDirection  (sndp (findPair (ID "direction") entity)))
                                       , ehp = if isNothing (findMaybePair (ID "hp") entity) then Nothing else Just (jsonToInt (sndp  (findPair (ID "hp") entity)))
                                       , evalue = if isNothing (findMaybePair (ID "value") entity) then Nothing else Just (jsonToInt (sndp (findPair (ID "value") entity)))
                                       , eactions =jsonToActions ( sndp (findPair (ID "actions") entity))
                                       }


initGame = Game{
    levels = initLevels,
    currentLevel = 0,
    status = initStatus
}

initLevels = [initLevel]

initLevel = Level 1 (Player 0 0 50 []) [] [] []

initPlayer = Player 0 0 50 []

initEntities = []

initItems = []

initLayout = []

initStatus = Levelselection