module Main where

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

main : Signal.Signal Graphics.Element.Element
main = Signal.map (toScene << fst)
  (Signal.foldp (\ (t, x) (f, y) -> let seed = Maybe.withDefault ((Random.initialSeed << round << Time.inSeconds) t) y
                                    in Maybe.withDefault (f, y) (Maybe.map
    (\ d -> let (f', newSeed) = addRandomBlock (move d f) seed in (f', Just newSeed)) x))
                (initField dimension, Nothing)
                (Time.timestamp (Signal.map (\ v -> if v == {x = 0, y = 1}
                                                    then Just Up
                                                    else if v == {x = 0, y = -1}
                                                         then Just Down
                                                         else if v == {x = -1, y = 0}
                                                              then Just Left
                                                              else if v == {x = 1, y = 0}
                                                                   then Just Right
                                                                   else Nothing)
                                             Keyboard.arrows)))


score : GameField -> Int
score f = List.sum (List.map (\ x -> if List.isEmpty x then 0 else 2 ^ (List.length x - 1)) (List.concat f))

moves : GameField -> Moves
moves f = {up = move Up f, down = move Down f, left = move Left f, right = move Right f}
