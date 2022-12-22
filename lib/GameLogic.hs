module GameLogic where
import Graphics.Gloss.Interface.IO.Game
import Datastructures
import HelperFunctions
import qualified Data.Bifunctor
import ActionHandler
import FileHandler (restartLevel)
import StartAndEnd
import qualified Data.Maybe
import Data.Maybe (fromJust)

update :: Float -> Game -> Game
update _ game | status game == Levelselection = game 
               | status game ==Playing = enemyTurn (gameChecks game)
               | otherwise = game

--bestaat using namespace Datastructures of iets gelijkaardig ??
moveDirection :: Direction -> (Int, Int)
moveDirection dir | dir == Datastructures.Up = (0, 1)
                  | dir == Datastructures.Down = (0, -1)
                  | dir == Datastructures.Left = (-1, 0)
                  | dir == Datastructures.Right = (1, 0)

canmove :: Game -> Direction -> Bool
canmove game dir | isValidStep (Data.Bifunctor.bimap
   ((px (player game)+1) +) ((py (player game)+1) +)
   (moveDirection dir)) (layout (levels game !! currentLevel game)) game= True
                 | otherwise = False

move :: Game -> Direction -> Game
move game dir | canmove game dir = game {player =changeDirection (changePlayerPosition (player game) (Data.Bifunctor.bimap (px (player game) +) (py (player game) +) (moveDirection dir))) dir}
              | otherwise = game

changeDirection :: Player -> Direction -> Player
changeDirection player dir | dir == Datastructures.Left = player {playerdirection = Datastructures.Left}
                           | dir == Datastructures.Right = player {playerdirection = Datastructures.Right}
                           | otherwise = player


handleInput :: Event -> Game -> Game
handleInput ev game | status game == Levelselection = handleLevelSelectionInput ev game
                  | status game == Datastructures.Playing = handleGameInput ev game
                  | status game == Won = handleEndingInput ev game
                  | otherwise = game

handleGameInput :: Event -> Game -> Game
handleGameInput ev game | isKey 'w' ev = move game Datastructures.Up
                    | isKey 's' ev = move game Datastructures.Down
                    | isKey 'a' ev = move game Datastructures.Left
                    | isKey 'd' ev = move game Datastructures.Right
                    | isNumber ev && read (getNumber ev)<length (detectActions  game) = applyFunction game (action (detectActions game !!(read (getNumber ev) :: Int)))
                    | otherwise = game

restartIfDead :: Game -> Game
restartIfDead game | php (player game) <=0 = restartLevel game
                   | otherwise = game

ifEndingTile :: Game -> Game
ifEndingTile game | isEndingTile (px (player game), py (player game)) (layout (levels game !! currentLevel game)) && currentLevel game == length (levels game) -1  = game {status = Won}
                  | isEndingTile (px (player game), py (player game)) (layout (levels game !! currentLevel game)) = changePlayerInGame (game {currentLevel = currentLevel game +1}) (findStart (layout (levels game !! (currentLevel game +1))))
                  | otherwise = game

enemyTurn :: Game -> Game
enemyTurn game = enemiesDamage (dissapearIfDead game)

gameChecks :: Game -> Game
gameChecks game = ifEndingTile (restartIfDead game)

enemiesDamage :: Game -> Game
enemiesDamage game = foldl dealDamage game (entities (levels game !! currentLevel game))

dealDamage :: Game -> Entity -> Game
dealDamage game entity | playerInEntityRange game entity && Data.Maybe.isJust (evalue entity) && damageTicks game `mod` 60 == 0 = game {player = decreasePlayerHp game (player game) (ID (eid entity)) (levels game !! currentLevel game),damageTicks = damageTicks game +1}
                       | otherwise = game{damageTicks = damageTicks game +1}

dissapearIfDead :: Game -> Game
dissapearIfDead game = foldl dissapear game (entities (levels game !! currentLevel game))

dissapear :: Game -> Entity -> Game
dissapear game entity | shouldDissapear game entity = game{levels = removeEntityFromLevel  game (ID (eid entity))}
                        | otherwise = game


shouldDissapear :: Game -> Entity -> Bool
shouldDissapear game entity | Data.Maybe.isJust (ehp  entity) && fromJust (ehp entity) <= 0 = True
                            | otherwise = False
