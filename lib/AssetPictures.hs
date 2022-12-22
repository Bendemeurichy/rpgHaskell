module AssetPictures where
import Datastructures
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import GHC.IO
import Graphics.Gloss.Juicy
-- maps of txt names to gloss pictures

characterMap :: [(String, Picture)]
characterMap =
  [ ("player", loadPicture "characters" "oldman"),
    ("devil", loadPicture "characters" "leftCat"),
    ("doorClosed", loadPicture "characters" "doorClosed"),
    ("doorOpen", loadPicture "characters" "doorOpen")
  ]

gameUiMap :: [(String, Picture)]
gameUiMap =
  [ ("actionbar", loadPicture "gameUi" "actionbar"),
    ("background", loadPicture "gameUi" "background"),
    ("heartEmpty", loadPicture "gameUi" "heartEmpty"),
    ("heartFull", loadPicture "gameUi" "heartFull"),
    ("heartHalf", loadPicture "gameUi" "heartHalf"),
    ("inventoryBox", loadPicture "gameUi" "inventoryBox"),
    ("inventorySelector", loadPicture "gameUi" "inventorySelector"),
    ("inventorySlot", loadPicture "gameUi" "inventorySlot")
  ]

itemMap :: [(String, Picture)]
itemMap =
  [ ("dagger", loadPicture "items" "dagger"),
    ("potion", loadPicture "items" "potion"),
    ("sword", loadPicture "items" "sword"),
    ("key", loadPicture "items" "key")
  ]

levelMap :: [(Tile, Picture)]
levelMap =
  [ (Wall, loadPicture "level" "levelWall"),
    (Floor, loadPicture "level" "levelFloor"),
    (End, loadPicture "level" "levelExit"),
    (Start, loadPicture "level" "levelStart")
  ]

startMap :: [(String, Picture)]
startMap =
  [ ("endScreenTextBar", loadPicture "startAndEnding" "endScreenTextBar"),
    ("levelNameBar", loadPicture "startAndEnding" "levelNameBar"),
    ("levelSelector", loadPicture "startAndEnding" "levelSelector")
  ]

inputMap :: [(String, Picture)]
inputMap =
  [ ("w", loadPicture "input" "W_Key_Light"),
    ("a", loadPicture "input" "A_Key_Light"),
    ("s", loadPicture "input" "S_Key_Light"),
    ("d", loadPicture "input" "D_Key_Light"),
    ("enter", loadPicture "input" "Enter_Key_Light"),
    ("esc", loadPicture "input" "Esc_Key_Light")
  ]

-- helper function to load the pictures from files

loadPicture :: String -> String -> Picture
loadPicture folder name = fromMaybe Blank (unsafePerformIO (loadJuicyPNG ("lib/assets/" ++ folder ++ "/" ++ name ++ ".png")))

-- helper function to get the picture from a map
lookupPicture :: [(String, Picture)] -> String -> Picture
lookupPicture map name = fromJust (lookup name map)

lookupTile :: [(Tile, Picture)] -> Tile -> Picture
lookupTile map tile = fromJust (lookup tile map)

-- constants because background doensn't need to change easily
renderBackground :: Picture
renderBackground = pictures [translate (-230.0) 0.0 (scale 20.0 18.0 (lookupPicture gameUiMap "background")), rotate 180.0 (translate (-230.0) 0.0 (scale 20.0 18.0 (lookupPicture gameUiMap "background")))]