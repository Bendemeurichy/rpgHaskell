import JsonParser
import Text.Parsec
import FileHandler
import Graphics.Gloss
import VisualHandler
import GameLogic

main :: IO ()
main = play window black fps initGame render handleInput update