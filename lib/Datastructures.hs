module Datastructures where


data JSON
    = Number Int
    | String String
    | Actions Action
    | Array [JSON]
    | Object [Pair]
    | Layout [TileLine]
    | UseTimes UseTimes
    deriving (Show, Eq)

newtype ID = ID String
    deriving (Show, Eq)
data Pair = Pair ID JSON
    deriving (Show, Eq)

data TileLine = TileLine [Tile]
    deriving (Show, Eq)

data Tile = Wall | Floor | Start | End deriving (Show, Eq) 
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
    id :: ID,
    x::Int,
    y::Int,
    name::String,
    description::String,
    useTimes::UseTimes,
    value::Int,
    actions::[Action]
    
} 

data Player = Player{
    px::Int,
    py::Int,
    php::Int,
    pinventory::[Item]
}

data Entity = Entity{
    eid::ID,
    ex::Int,
    ey::Int,
    ename::String,
    edescription::String,
    eDirection:: Maybe Direction,
    ehp :: Maybe Int,
    evalue::Maybe Int,
    eactions::[Action]
}

data Action = Action{
    condition::[Function],
    action::Function
} deriving (Show, Eq)

data Direction = Up | Down | Left | Right
    deriving (Show, Eq)

