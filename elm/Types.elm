module Types where

type alias Position = (Int, Int)

type alias Block = List Position

type alias GameField = List (List Block)

type alias Moves = {up : GameField,
                    down : GameField,
                    left : GameField,
                    right : GameField}

type Direction = Up | Down | Left | Right

