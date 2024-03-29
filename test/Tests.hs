import Test.Hspec

import JsonParser
import Datastructures
import FileHandler
import HelperFunctions (findStart, calculateHealth, getNumber, detectItems, changePlayerInGame)
import Graphics.Gloss.Interface.IO.Game (Event(EventKey))
import Graphics.Gloss.Interface.IO.Interact
import ActionHandler (inventoryContains)
import GameLogic (canmove)
import Text.Parsec

main :: IO ()
main = hspec $ do
    it "player hp after reading from file3 is 46" $ do
        let cplayer = player (startLevel "level3" initGame)
        php cplayer `shouldBe` 45

    it "start location of level 2 is (0,0)" $ do
        let cgame = startLevel "level2" initGame
        findStart (layout (levels cgame !! currentLevel cgame)) `shouldBe` (0,0)

    it "player hp is calculated correctly for conversion to hp bar" $ do
        let cplayer = player (startLevel "level3" initGame)
        calculateHealth cplayer `shouldBe` ["full","full","full","full","half"]

    it "detectItems works correctly" $ do
        let cgame = startLevel "level3" initGame
        let citems = detectItems cgame
        length citems `shouldBe` 0
    
    it "inventory is empty after reading from file2" $ do
        let cgame = startLevel "level2" initGame
        inventoryContains (player cgame) (ID "dagger") `shouldBe` False
    
    it "can't move to wall" $ do
        let cgame = startLevel "level2" initGame
        let cplayer = player cgame
        let moved = canmove cgame Datastructures.Left
        moved `shouldBe` False

    it "can move to empty space" $ do
        let cgame = startLevel "level2" initGame
        let cplayer = player cgame
        let moved = canmove cgame Datastructures.Up
        moved `shouldBe` True
    
    it "parseJson works correctly" $ do
        let parsed = parse parseJSON "" "direction: left,"
        show parsed `shouldBe` "Right (Object [Pair (ID \"direction\") (Direction Left)])"