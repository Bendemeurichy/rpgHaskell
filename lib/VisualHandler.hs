module VisualHandler where
import Datastructures
import JsonParser
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (fromMaybe, fromJust, isNothing)
import GHC.IO (unsafePerformIO)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import HelperFunctions
import Text.Printf (IsChar(toChar))
import Data.String (IsString(fromString))
import ActionHandler

-- maps of txt names to gloss pictures

characterMap :: [(String, Picture)]
characterMap = [("player", loadPicture "characters" "oldman"), ("devil", loadPicture "characters" "leftCat"),
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
windowposition = (150, 0)

windowSize :: (Int, Int)
windowSize = (1000, 700)

fps :: Int
fps = 60

window :: Display
window = InWindow "RPG" windowSize windowposition

--constants because background doensn't need to change easily
renderBackground :: Picture
renderBackground = pictures [translate (-230.0) 0.0 (scale 20.0 18.0 (lookupPicture gameUiMap "background")), rotate 180.0 (translate (-230.0) 0.0 (scale 20.0 18.0 (lookupPicture gameUiMap "background")))]

render :: Game -> Picture 
render game | status game == Levelselection = renderStart game
            | status game == Playing = renderPlaying game
            | status game == Won = renderEnding game

renderPlaying :: Game -> Picture
renderPlaying game = pictures [renderBackground,renderLayout (layout (levels game!! currentLevel game)),renderItems (items (levels game !! currentLevel game)) ,
    renderEntities (entities (levels game !! currentLevel game)), renderUI game,renderplayer (player game)]

renderplayer :: Player -> Picture
renderplayer player = translate (convertx (px player + 1)) (converty (py player + 1)) (scale scaleSize scaleSize (lookupPicture characterMap "player"))

renderTile :: Tile -> Picture
renderTile tile = scale scaleSurface scaleSurface (lookupTile levelMap tile)

renderTileLine :: TileLine -> Int -> Picture
renderTileLine (TileLine tiles) y = pictures [translate  (convertx i) (converty y) (renderTile (tiles !! i)) | i <- [0..length tiles - 1]]

renderLayout :: [TileLine] -> Picture
renderLayout layout = pictures [renderTileLine (reverse layout !! y) y  | y <- [(length layout - 1),length layout -2 ..0]]

renderItem :: Item -> Picture
renderItem item  = translate (convertx (1+x item))(converty (1 + y  item)) (scale scaleSize scaleSize (lookupPicture itemMap (Datastructures.id item)))

renderItems :: [Item]   -> Picture
renderItems items = pictures [renderItem item | item <- items]

translateEntity :: Entity-> Picture
translateEntity entity = translate (convertx (1 + ex entity)) (converty (1 + ey entity)) (rotateEntity entity)

renderEntity :: Entity -> Picture
renderEntity entity | eid entity == "door" =  scale scaleSize scaleSize (lookupDoor entity)
                    | otherwise =  scale scaleSize scaleSize (lookupPicture characterMap (eid entity))

rotateEntity :: Entity  -> Picture
rotateEntity entity | isNothing (eDirection entity) = renderEntity entity
                    | fromJust (eDirection entity) == Datastructures.Left = rotate 90.0 (renderEntity entity )
                    | fromJust (eDirection entity) == Datastructures.Down = rotate 180.0 (renderEntity entity)
                    | fromJust (eDirection entity) == Datastructures.Right = rotate 270.0 (renderEntity entity )
                    | otherwise = renderEntity entity

renderEntities :: [Entity]  -> Picture
renderEntities entities  = pictures [translateEntity entity | entity <- entities]

lookupDoor :: Entity -> Picture
lookupDoor entity | isNothing (evalue entity) = lookupPicture characterMap "doorClosed"
                  | otherwise = lookupPicture characterMap "doorOpen"

renderUI :: Game -> Picture
renderUI game = pictures [renderActionsfield game, renderInventory (player game)]

renderActionsfield :: Game -> Picture
renderActionsfield game = pictures [renderActionbar , renderActions game]

renderActions :: Game -> Picture
renderActions game =color white (translate (-480.0) 0.0 (pictures [ translate 0.0 250.0 (scale 0.2 0.2 (text "Actions")),pictures [renderAction (actions !! i) i | i <- [0..length actions -1 ]]]))
    where actions = detectActions  game

renderAction :: Action -> Int-> Picture
renderAction act i  | functionName (action act) == ID "decreaseHp" = translate 0.0 (convertyActions (i+1)) (scale 0.1 0.1 (text ( show i ++ ") " ++idtoString (functionName  (action act)) ++ " " ++ idtoString  (argumentToId  (arguments  (action act) !! 1)))))
                    |otherwise = translate 0.0 (convertyActions (i+1)) (scale 0.1 0.1 (text ( show i ++ ") " ++idtoString (functionName  (action act)))))


renderActionbar :: Picture
renderActionbar = translate (-400.0) 0.0 (scale 9.4 15.0 (rotate 180.0 (lookupPicture gameUiMap "actionbar")))

renderInventory :: Player -> Picture
renderInventory player = pictures [renderInventoryBox,renderHearts player,renderInventorySlots,renderInventoryContents player]

renderInventoryBox ::  Picture
renderInventoryBox  = translate 180.0 (-260.0) (scale 20.0 8.5 (rotate 90.0 (lookupPicture gameUiMap "inventoryBox")))

renderInventorySlots ::  Picture
renderInventorySlots = pictures [renderInventorySlot i | i <- [0..inventorySize-1]]

renderInventorySlot :: Int -> Picture
renderInventorySlot slot = translate (convertxItems slot) (-225.0) (scale 5.0 5.0 (lookupPicture gameUiMap "inventorySlot"))

renderInventoryContents :: Player -> Picture
renderInventoryContents player = pictures [renderInventoryItem i player | i <- [0..length (pinventory player)-1]]

renderInventoryItem :: Int -> Player -> Picture
renderInventoryItem item player = translate (convertxItems item) (-225.0) (scale 3.6 3.6 (lookupPicture itemMap (Datastructures.id  (pinventory  player !!item))))

renderHearts :: Player -> Picture
renderHearts player = pictures [translate (convertxHp i) (-320.0) (scale 4.0 4.0(renderHeart (bar !! i)))|i<-[0..length bar-1]]
    where bar = calculateHealth player

renderHeart :: String -> Picture
renderHeart heart | heart == "full" = lookupPicture gameUiMap "heartFull"
                |heart == "half" = lookupPicture gameUiMap "heartHalf"
                | otherwise = lookupPicture gameUiMap "heartEmpty"

renderEnding :: Game -> Picture
renderEnding game = Pictures [renderBackground, color white (translate (-170) 0 (scale 0.5 0.5 (text "You won!")))]

renderStart :: Game -> Picture
renderStart game = Pictures [renderBackground,color white ( translate (-170) 0 (scale 0.5 0.5 (text "Press space to start")))]

convertx :: Int -> Float
convertx x = fromIntegral x * 40.0 + 75.0

converty :: Int -> Float
converty y = fromIntegral y * 40.0

convertxItems :: Int -> Float
convertxItems x = (fromIntegral x * 80.0) - 100.0


convertxHp :: Int -> Float
convertxHp x = fromIntegral x * 40.0 -100.0

convertyActions :: Int -> Float
convertyActions y = - fromIntegral y * 40.0 +250.0

scaleSurface :: Float
scaleSurface = 2.0

scaleSize :: Float
scaleSize = 1.3
