module Datastructures where


data JSON
    = Number Int
    | String String
    | Actions [Action]
    | Array [JSON]
    | Object [Pair]
    | Layout [TileLine]
    | UseTimes UseTimes
    |Direction Direction
    deriving (Show, Eq)

newtype ID = ID String
    deriving (Show, Eq)

data Pair = Pair ID JSON
    deriving (Show, Eq)

jsonToInt :: JSON -> Int
jsonToInt (Number a) = a

jsonToString :: JSON -> String
jsonToString (String a) = a

jsonToActions :: JSON -> [Action]
jsonToActions (Actions a) = a

jsonToArray :: JSON -> [JSON]
jsonToArray (Array a) = a

jsonToObject :: JSON -> [Pair]
jsonToObject (Object a) = a

jsonToLayout :: JSON -> [TileLine]
jsonToLayout (Layout a) = a

jsonToUseTimes :: JSON -> UseTimes
jsonToUseTimes (UseTimes a) = a

jsonToDirection :: JSON -> Direction
jsonToDirection (Direction a) = a

idtoString :: ID -> String
idtoString (ID a) = a

sndp :: Pair -> JSON
sndp (Pair _ b) = b

fstp :: Pair -> ID
fstp (Pair a _) = a

lengthTileLine :: TileLine -> Int
lengthTileLine (TileLine a) = length a

data TileLine = TileLine [Tile]
    deriving (Show, Eq)

data Tile = Wall | Floor | Start | End | Empty deriving (Show, Eq) 
data UseTimes = Amount Int
    |Infinite 
    deriving (Show, Eq)

data Function = Function{
    functionName::ID,
    arguments::[Argument]
} deriving (Show, Eq)

data Argument = TargetID ID | ArgumentFunction Function
    deriving (Show, Eq)

data Item = Item{
    id :: String,
    x::Int,
    y::Int,
    name::String,
    description::String,
    useTimes::UseTimes,
    value::Int,
    actions::[Action]
    
} deriving (Show, Eq)

data Player = Player{
    px::Int,
    py::Int,
    php::Int,
    pinventory::[Item]
} deriving (Show, Eq)

data Entity = Entity{
    eid::String,
    ex::Int,
    ey::Int,
    ename::String,
    edescription::String,
    eDirection:: Maybe Direction,
    ehp :: Maybe Int,
    evalue::Maybe Int,
    eactions::[Action]
} deriving (Show, Eq)

data Action = Action{
    condition::[Function],
    action::Function
} deriving (Show, Eq)

data Direction = Up | Down | Left | Right
    deriving (Show, Eq)

data Game = Game{
    player :: Player,
    levels :: [Level],
    currentLevel :: Int,
    status :: Status
} deriving (Show, Eq)

data Level = Level{
    level :: Int,
    entities :: [Entity],
    items :: [Item],
    layout :: [TileLine]
} deriving (Show, Eq)

data Status = Levelselection | Playing | Won | Lost deriving (Show, Eq)