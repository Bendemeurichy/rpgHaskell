module VisualHandler where
import Datastructures
import JsonParser
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (fromMaybe, fromJust)
import GHC.IO (unsafePerformIO)
import Graphics.Gloss.Juicy (loadJuicyPNG)

-- maps of txt names to gloss pictures

characterMap :: [(String, Picture)]
characterMap = [("player", loadPicture "characters" "oldman"), ("enemy", loadPicture "characters" "leftCat"),
    ("doorClosed", loadPicture "characters" "doorClosed"), ("doorOpen", loadPicture "characters" "doorOpen")]

gameUiMap :: [(String, Picture)]
gameUiMap = [("actionbar", loadPicture "gameUi" "actionbar"),("background",loadPicture "gameUi" "background"),
    ("heartEmpty",loadPicture "gameUi" "heartEmpty"),("heartFull",loadPicture "gameUi" "heartFull"),
    ("heartHalf",loadPicture "gameUi" "heartHalf"),("inventoryBox",loadPicture "gameUi" "inventoryBox"),
    ("inventorySelector",loadPicture "gameUi" "inventorySelector"),("inventorySlot",loadPicture "gameUi" "inventorySlot")]

itemMap :: [(String, Picture)]
itemMap = [("dagger", loadPicture "items" "dagger"), ("potion", loadPicture "items" "potion"),
    ("sword", loadPicture "items" "sword"), ("key", loadPicture "items" "key")]

levelMap :: [(Tile, Picture)]
levelMap = [(Wall, loadPicture "level" "levelWall"), (Floor, loadPicture "level" "levelFloor"),
    (End, loadPicture "level" "levelExit"), (Start, loadPicture "level" "levelStart")]

startMap :: [(String, Picture)]
startMap = [("endScreenTextBar", loadPicture "startAndEnding" "endScreenTextBar"),("levelNameBar",loadPicture "startAndEnding" "levelNameBar"),
    ("levelSelector",loadPicture "startAndEnding" "levelSelector")]

-- helper function to load the pictures from files

loadPicture :: String -> String -> Picture
loadPicture folder name = fromMaybe Blank (unsafePerformIO (loadJuicyPNG ("lib/assets/" ++ folder ++ "/" ++ name ++ ".png")))

-- helper function to get the picture from a map
lookupPicture :: [(String, Picture)] -> String -> Picture
lookupPicture map name = fromJust (lookup name map)

lookupTile :: [(Tile, Picture)] -> Tile -> Picture
lookupTile map tile = fromJust (lookup tile map)

--functions to render the game correctly
windowposition :: (Int, Int)
windowposition = (200, 200)

windowSize :: (Int, Int)
windowSize = (1000, 700)

fps :: Int
fps = 60

window :: Display
window = InWindow "RPG" windowSize windowposition

renderBackground :: Picture
renderBackground = pictures [translate (-230.0) 0.0 (scale 20.0 18.0 (lookupPicture gameUiMap "background")), (rotate 180.0 (translate (-230.0) 0.0 (scale 20.0 18.0 (lookupPicture gameUiMap "background"))))]

render :: Game -> Picture
render game = pictures [renderBackground,renderplayer]

renderplayer :: Picture
renderplayer = lookupPicture characterMap "player"

convertx :: Int -> Float
convertx x = fromIntegral x * 40.0 - 200.0


