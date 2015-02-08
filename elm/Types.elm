module Types where

import Dict

type alias Position = (Int, Int)

type Block = EmptyBlock | StationaryBlock Int  | MovingBlock Int Position Bool | NoBlock

type alias GameField = Dict.Dict Position Block

type Direction = Up | Down | Left | Right

type Event = Initialization | Move Direction | Useless
