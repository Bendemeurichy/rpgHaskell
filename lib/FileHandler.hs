module FileHandler where

import System.Directory
import Datastructures
import JsonParser
import Text.Parsec
import System.IO
import Control.Exception (throw)
import GHC.IO (unsafePerformIO)
import Data.Maybe (fromJust, isNothing)
import GHC.TopHandler (runIO)
import System.Posix.Internals (withFilePath)
import HelperFunctions
import Data.List (sort)

------------------------------------------------------------------------------------------------------------------------
-- module to read files and initialize games and levels
------------------------------------------------------------------------------------------------------------------------


-- functions to load levels from files and initialize the game

constructLevel :: String -> Game -> IO Game
constructLevel levelname game = do
    file <- readFile ("levels/" ++ levelname ++ ".txt")
    let json = parse parseJSON "" file
    case
        json of
            Prelude.Left err -> error $ show err
            Prelude.Right json -> do
                let constructedgame = constructGameFromJSON game json
                return constructedgame

showLevel :: String -> IO ()
showLevel levelname = do
  withFile
    levelname
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        let parsed = parse parseJSON "error" contents
        case parsed of
          Prelude.Left err -> error $ show err
          Prelude.Right json -> putStr (show json)
    )

startLevel :: String ->Game -> Game
startLevel levelname game = (changePlayerInGame constructedgame (findStart  (layout (levels constructedgame!!currentLevel constructedgame)))){levelName = levelname}
    where constructedgame =unsafePerformIO (constructLevel levelname game)

restartLevel :: Game -> Game
restartLevel game= startLevel (levelName game) game

-- functions to convert JSON to game datastructures

findMaybePair :: ID -> [Pair] -> Maybe Pair
findMaybePair _ [] = Nothing
findMaybePair id (x:xs) = if id == fstp x then Just x else findMaybePair id xs

findPair :: ID -> [Pair] -> Pair
findPair id pairs = fromJust (findMaybePair id pairs)

constructGameFromJSON :: Game -> JSON -> Game
constructGameFromJSON initGame (Object pairs) = initGame {player = constructPlayerFromJSON initPlayer (findPair (ID "player") pairs),
                                                        levels = constructLevelsFromJSON initLevels pairs}

constructLevelsFromJSON :: [Level] -> [Pair] -> [Level]
constructLevelsFromJSON initLevels pairs = [constructLevelFromJSON initLevel level | level <- levels]
    where levels = jsonToArray (sndp (findPair (ID "levels") pairs))

constructLevelFromJSON :: Level -> JSON -> Level
constructLevelFromJSON initLevel (Object pairs) = initLevel{ entities = constructEntitiesFromJSON initEntities(findPair (ID "entities") pairs)
                                                        , items = constructItemsFromJSON initItems (findPair (ID "items") pairs)
                                                        , layout = jsonToLayout  (sndp (findPair (ID "layout") pairs))
                                                        }


constructPlayerFromJSON :: Player -> Pair -> Player
constructPlayerFromJSON initplayer (Pair _ (Object pairs)) = initplayer{ php = jsonToInt (sndp (findPair (ID "hp") pairs))
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


--initial values for a game and its components

initGame = Game{
    levelName ="",
    player =initPlayer,
    levels = initLevels,
    currentLevel = 0,
    status = initStatus,
    damageTicks = 0,
    selectionScreen  = initSelectionScreen
}

initLevels = [initLevel]

initLevel = Level 1 initEntities initItems initLayout

initPlayer = Player Datastructures.Right 0 0 50 []

initEntities = []

initItems = []

initLayout = []

initStatus = Levelselection

initSelector :: Selector
initSelector = Selector {selected = 0}

initSelectionScreen :: SelectionScreen
initSelectionScreen = SelectionScreen {selector = initSelector, levelfiles = initLevelfiles}

initLevelfiles :: [String]
initLevelfiles = sort findLevelFiles

-- functions to detect all files in the level folder

findLevelFiles :: [String]
{-# NOINLINE findLevelFiles #-}
findLevelFiles = unsafePerformIO $ do
    levels <- getDirectoryContents "levels"
    return $ map (reverse . drop 4 . reverse) $ filter (\x -> getExtension x == "txt") levels

getExtension :: String -> String
getExtension [] = []
getExtension (x:xs)
    | x == '.' = xs
    | otherwise = getExtension xs