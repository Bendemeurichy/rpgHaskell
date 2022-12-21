module GameLogic where
import Graphics.Gloss.Interface.IO.Game
import Datastructures
import HelperFunctions
import qualified Data.Bifunctor
import ActionHandler (applyFunction)
import FileHandler (restartLevel)
import VisualHandler (gameUiMap)
import StartAndEnd

update :: Float -> Game -> Game
update _ game = game

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
move game dir | canmove game dir = game {player = changePlayerPosition (player game) (Data.Bifunctor.bimap (px (player game) +) (py (player game) +) (moveDirection dir))}
              | otherwise = game

handleInput :: Event -> Game -> Game
handleInput ev game | status game == Levelselection = handleLevelSelectionInput ev game
                  | status game == Datastructures.Playing = gameChecks(handleGameInput ev game)
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

gameChecks :: Game -> Game
gameChecks game = ifEndingTile (restartIfDead game)