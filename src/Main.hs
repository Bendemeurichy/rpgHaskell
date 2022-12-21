import FileHandler
import Graphics.Gloss
import VisualHandler
import GameLogic

main :: IO ()
main = play window black fps (startLevel "level2") render handleInput update