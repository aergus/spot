module Types where

import Dict
import Random
import Time

type alias Position = (Int, Int)

type Block =
  EmptyBlock | StationaryBlock Int  | MovingBlock Int Position Bool | NoBlock

type alias GameField = Dict.Dict Position Block

type Direction = Up | Down | Left | Right

type Event = Initialization Int | Move Direction | Animation Time.Time | Useless

type alias Moves = {up: GameField,
                    down : GameField,
                    left: GameField,
                    right: GameField}

type alias GameState =
  {field : GameField,
   seed : Random.Seed,
   animation : Maybe Time.Time,
   moves : Moves}
