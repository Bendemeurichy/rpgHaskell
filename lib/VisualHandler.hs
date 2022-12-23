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
import AssetPictures
import StartAndEnd

--------------------------------------------------------------------------------
--module to handle the rendering of the game
--------------------------------------------------------------------------------



--functions to handle the gloss window
windowposition :: (Int, Int)
windowposition = (150, 0)

windowSize :: (Int, Int)
windowSize = (1000, 700)

fps :: Int
fps = 60

window :: Display
window = InWindow "RPG" windowSize windowposition

render :: Game -> Picture 
render game | status game == Levelselection = renderStart game
            | status game == Playing = renderPlaying game
            | status game == Won = renderEnding game


-- render the game while playing--

--render the correct level and the player
renderPlaying :: Game -> Picture
renderPlaying game = pictures [renderBackground,renderLayout (layout (levels game!! currentLevel game)),renderItems (items (levels game !! currentLevel game)) ,
    renderEntities (entities (levels game !! currentLevel game)), renderUI game,renderplayer (player game)]

renderplayer :: Player -> Picture
renderplayer player | playerdirection player == Datastructures.Left = translate (convertx (px player + 1)) (converty (py player + 1)) (scale (-scaleSize) scaleSize (lookupPicture characterMap "player"))
                    |otherwise = translate (convertx (px player + 1)) (converty (py player + 1)) (scale scaleSize scaleSize (lookupPicture characterMap "player"))

renderTile :: Tile -> Picture
renderTile tile | tile == Empty = blank
                | otherwise = scale scaleSurface scaleSurface (lookupTile levelMap tile)

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

-- render the actionbar and the actions

renderActionsfield :: Game -> Picture
renderActionsfield game = pictures [renderActionbar , renderActions game,renderMovementPrompt]

renderActions :: Game -> Picture
renderActions game =color white (translate (-480.0) 0.0 (pictures [ translate 0.0 250.0 (scale 0.2 0.2 (text "Actions")),pictures [renderAction (actions !! i) i | i <- [0..length actions -1 ]]]))
    where actions = detectActions  game

renderAction :: Action -> Int-> Picture
renderAction act i  | functionName (action act) == ID "decreaseHp" = translate 0.0 (convertyActions (i+1)) (scale 0.1 0.1 (text ( show i ++ ") " ++idtoString (functionName  (action act)) ++ " " ++ idtoString  (argumentToId  (arguments  (action act) !! 1)))))
                    |otherwise = translate 0.0 (convertyActions (i+1)) (scale 0.1 0.1 (text ( show i ++ ") " ++idtoString (functionName  (action act)))))


renderActionbar :: Picture
renderActionbar = translate (-400.0) 0.0 (scale 9.4 15.0 (rotate 180.0 (lookupPicture gameUiMap "actionbar")))

-- render inventory

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

--render extrar ui

renderMovementPrompt :: Picture
renderMovementPrompt = pictures [renderW,renderS,renderA,renderD]

renderW :: Picture
renderW = translate (-380.0) (-180.0) (scale 0.6 0.6 (lookupPicture inputMap "w"))

renderS :: Picture
renderS = translate (-380.0) (-220.0) (scale 0.6 0.6 (lookupPicture inputMap "s"))

renderA :: Picture
renderA = translate (-420.0) (-220.0) (scale 0.6 0.6 (lookupPicture inputMap "a"))

renderD :: Picture
renderD = translate (-340.0) (-220.0) (scale 0.6 0.6 (lookupPicture inputMap "d"))


--convert the coordinates of the tiles to the coordinates of the window

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


--scale the tiles
scaleSurface :: Float
scaleSurface = 2.0

--scale the items and characters
scaleSize :: Float
scaleSize = 1.3
