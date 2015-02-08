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
main = Signal.map (\x -> Maybe.withDefault Graphics.Element.empty
                                          (Maybe.map
  (\ (f, s) -> toScene f (f == move Up f && f == move Down f
                                         && f == move Left f
                                         && f == move Right f))
  x))
                  (Signal.foldp update Nothing signal)

update : (Time.Time, Event) -> Maybe (GameField, Random.Seed) -> Maybe (GameField, Random.Seed)
update (t, e) x = case e of
  Initialization -> let f = emptyField dimension
                        (f', s') = addRandomBlock f ((Random.initialSeed << round
                                                                         << Time.inSeconds) t)
                    in Just (addRandomBlock f' s')
  Move d -> Maybe.map (\ (f, s) -> let f' = move d f
                                   in if f == f'
                                      then (f, s)
                                      else addRandomBlock f' s)
                      x
  _ -> x

signal : Signal.Signal (Time.Time, Event)
signal = Time.timestamp (Signal.mergeMany
  [Signal.map (always Initialization) initialization,
   Signal.map (\ v -> if v == {x = 0, y = 1}
                      then Move Up
                      else if v == {x = 0, y = -1}
                           then Move Down
                           else if v == {x = -1, y = 0}
                                then Move Left
                                else if v == {x = 1, y = 0}
                                     then Move Right
                                     else Useless)
                Keyboard.arrows])
