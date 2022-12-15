module Datastructures where


data JSON
    = Number Int
    | String String
    | Actions Function
    | Array [JSON]
    | Object [Pair]
    deriving (Show, Eq)

newtype ID = ID String
    deriving (Show, Eq)
data Pair = Pair ID JSON
    deriving (Show, Eq)

data UseTimes = Int Int
    |Unlimited 
    deriving (Show, Eq)

data Function = Function{
    functionName::String,
    arguments::[ID]
} deriving (Show, Eq)

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
    condition::ID->Bool,
    action::Function
}

data Direction = Up | Down | Left | Right
    deriving (Show, Eq)