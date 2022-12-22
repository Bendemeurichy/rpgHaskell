module StartAndEnd where

import Datastructures
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import HelperFunctions
import FileHandler (startLevel)
import AssetPictures


toPlaying :: Game -> Game
toPlaying game = game {status = Playing}

toEnding :: Game -> Game
toEnding game = game {status = Won}

toLevelSelection :: Game -> Game
toLevelSelection game = game {status = Levelselection}

handleLevelSelectionInput :: Event -> Game -> Game
handleLevelSelectionInput ev game | isKey 'w' ev = moveSelector game Datastructures.Up
                                  | isKey 's' ev = moveSelector game Datastructures.Down
                                  | isSpecialKey KeyEnter ev = initiateGame game
                                  | otherwise = game

handleEndingInput :: Event -> Game -> Game
handleEndingInput ev game | isSpecialKey KeyEnter ev = toLevelSelection game
                          | otherwise = game

moveSelector :: Game -> Direction -> Game
moveSelector game dir | canmoveSelector game dir && dir == Datastructures.Up = game { selectionScreen = decreaseSelector screen}
                      | canmoveSelector game dir && dir == Datastructures.Down = game { selectionScreen = increaseSelector screen}
                      | otherwise = game
                      where screen = selectionScreen game

canmoveSelector :: Game -> Direction -> Bool
canmoveSelector game dir | dir == Datastructures.Down && selected (selector screen) < (length (levelfiles screen) - 1)  = True
                            | dir == Datastructures.Up && selected (selector screen) > 0  = True
                            | otherwise = False
                            where screen = selectionScreen game

increaseSelector :: SelectionScreen -> SelectionScreen
increaseSelector screen = screen {selector = (selector screen) {selected = selected (selector screen) +1}}

decreaseSelector :: SelectionScreen -> SelectionScreen
decreaseSelector screen = screen {selector = (selector screen) {selected = selected (selector screen) -1}}

initiateGame :: Game -> Game
initiateGame game = startLevel (getSelectedLevel game) (toPlaying game)

getSelectedLevel :: Game -> String
getSelectedLevel game = levelfiles (selectionScreen game) !! selected (selector (selectionScreen game))

convertLevelPosition :: Int -> Float
convertLevelPosition pos = - fromIntegral (pos * 110) +140.0

renderLevelBars :: SelectionScreen -> Picture
renderLevelBars screen =pictures [ renderLevelBar pos | pos <- [0..length (levelfiles screen) -1]]

renderLevelBar :: Int -> Picture
renderLevelBar pos = translate 0 (convertLevelPosition pos)(scale 10.0 7.5 (lookupPicture startMap "levelNameBar"))

renderLevelText :: Int -> String -> Picture
renderLevelText pos name = translate (-30) (convertLevelPosition pos -10)(scale 0.2 0.2 (text name))

renderLevelNames :: SelectionScreen -> Picture
renderLevelNames screen = pictures [renderLevelText pos (levelfiles screen !! pos) | pos <- [0..length (levelfiles screen) -1]]

renderEndingBar :: Picture
renderEndingBar = translate 0 0 (scale 16.0 16.0 (lookupPicture startMap "endScreenTextBar"))

renderEnding :: Game -> Picture
renderEnding game = Pictures [renderBackground,renderEndingBar,renderEndingBar,renderEndingtext,renderEscapePrompt,renderReturnToMenu]

renderEndingtext :: Picture
renderEndingtext = translate (-170) 10.0 (scale 0.5 0.5 (text "You won!"))

renderEscapePrompt :: Picture
renderEscapePrompt = translate (-390) 270(scale 0.6 0.6(lookupPicture inputMap "esc"))

renderStart :: Game -> Picture
renderStart game = Pictures [renderBackground,renderLevelBars (selectionScreen game),renderUpAndDownPrompt, renderLevelNames (selectionScreen game),renderSelection (selectionScreen game),renderEscapePrompt]

renderSelector :: SelectionScreen -> Picture
renderSelector screen = translate (-180)(convertLevelPosition (selected (selector screen)))(scale 5.0 5.0 (lookupPicture startMap "levelSelector"))

renderSelection :: SelectionScreen -> Picture
renderSelection screen = Pictures [renderSelector screen,renderenterPrompt screen]

renderenterPrompt :: SelectionScreen -> Picture
renderenterPrompt screen = translate 160 (convertLevelPosition (selected (selector screen)))(scale 0.6 0.6(lookupPicture inputMap "enter"))

renderUpAndDownPrompt :: Picture
renderUpAndDownPrompt = pictures [renderUp,renderDown]

renderUp :: Picture
renderUp = translate 300 (-170)(scale 0.6 0.6(lookupPicture inputMap "w"))

renderDown :: Picture
renderDown = translate 300 (-220)(scale 0.6 0.6(lookupPicture inputMap "s"))

renderReturnToMenu :: Picture
renderReturnToMenu = pictures [renderEndingEnterPrompt,renderReturnText]

renderReturnText :: Picture
renderReturnText = color white (translate (-280) (-270) (scale 0.4 0.4(text "Return to level selection")))

renderEndingEnterPrompt :: Picture
renderEndingEnterPrompt = translate (-300) (-250 )(scale 0.7 0.7 (lookupPicture inputMap "enter"))