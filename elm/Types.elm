module Types where

import Dict

type alias Position = (Int, Int)

type alias Block = List Position

type alias GameField = Dict.Dict Position Block

type alias Moves = {up : GameField,
                    down : GameField,
                    left : GameField,
                    right : GameField}

type Direction = Up | Down | Left | Right

type Event = Initialization | Move Direction | Useless
