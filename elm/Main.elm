module Main where

import Dict
import Graphics.Element
import Keyboard
import Maybe
import List
import Random
import Signal
import Time

import Auxiliary (..)
import Drawing (..)
import Parameters (..)
import Transitions (..)
import Types (..)

port initialization : Signal ()

main : Signal.Signal Graphics.Element.Element
main = Signal.map (\ (f, s) -> if f == move Up f && f == move Down f
                                                 && f == move Left f
                                                 && f == move Right f
                               then toScene f True
                               else toScene f False)
                  (Signal.foldp update (initField dimension, Nothing) signal)

update : (Time.Time, Event) -> (GameField, Maybe Random.Seed) -> (GameField, Maybe Random.Seed)
update (t, e) (f , y) = case e of
  Initialization -> (f, y)
  Move x -> let seed = Maybe.withDefault ((Random.initialSeed << round
                                                              << Time.inSeconds) t) y
            in Maybe.withDefault (f, y) (Maybe.map
    (\ d -> let f' = move d f in if f == f'
                                 then (f, Just seed)
                                 else let (f'', newSeed) = addRandomBlock f' seed
                                      in (f'', Just newSeed)) x)

signal : Signal.Signal (Time.Time, Event)
signal = Time.timestamp (Signal.mergeMany
  [Signal.map (always Initialization) initialization,
   Signal.map (\ v -> if v == {x = 0, y = 1}
                      then Move (Just Up)
                      else if v == {x = 0, y = -1}
                           then Move (Just Down)
                           else if v == {x = -1, y = 0}
                                then Move (Just Left)
                                else if v == {x = 1, y = 0}
                                     then Move (Just Right)
                                     else Move Nothing)
                Keyboard.arrows])

score : GameField -> Int
score f = List.sum (List.map (\ x -> if List.isEmpty x then 0 else 2 ^ (List.length x - 1)) (Dict.values f))

moves : GameField -> Moves
moves f = {up = move Up f, down = move Down f, left = move Left f, right = move Right f}
