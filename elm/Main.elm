module Main where

import Debug

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

port initialization : Signal Int

main : Signal.Signal Graphics.Element.Element
main = Signal.map (\x -> Maybe.withDefault Graphics.Element.empty
                                          (Maybe.map
  (\ (f, s, v) -> toScene f (f == move Up f && f == move Down f
                                            && f == move Left f
                                            && f == move Right f))
  x))
                  (Signal.foldp update Nothing signal)

update : Event -> Maybe (GameField, Random.Seed, Maybe Time.Time) -> Maybe (GameField, Random.Seed, Maybe Time.Time)
update e x = case e of
  Initialization t -> let f = emptyField dimension
                          (f', s') = addRandomBlock f (Random.initialSeed t)
                      in (Just << addThird Nothing) (addRandomBlock f' s')
  Move d -> Maybe.map (\ (f, s, v) -> if v /= Nothing
                                      then (f, s ,v)
                                      else let f' = move d f
                                           in if f == f'
                                           then (f, s, Nothing)
                                           else addThird (Just 0) (addRandomBlock f' s))
                      x
  Animation t -> Maybe.map
    (\ (f, s, v) -> Maybe.withDefault (f, s, v)
                                      (Maybe.map (\ k -> let k' = Debug.log "k'"  (k + t)
                                                         in if k' >= animationDuration
                                                         then (f, s, Nothing)
                                                         else (f, s, Just k'))
                                                 v))
    x
  _ -> x

signal : Signal.Signal Event
signal = let relevantArrows = Signal.keepIf (\ v -> v.x == 0 || v.y == 0)
                                            {x = 0, y = 0}
                                            Keyboard.arrows
         in Signal.mergeMany
  [Signal.map Initialization initialization,
   Signal.map (\ v -> if v == {x = 0, y = 1}
                      then Move Up
                      else if v == {x = 0, y = -1}
                           then Move Down
                           else if v == {x = -1, y = 0}
                                then Move Left
                                else if v == {x = 1, y = 0}
                                     then Move Right
                                     else Useless)
              relevantArrows,
   Signal.map Animation (Time.fpsWhen 20 (Time.since animationDuration
                                                     relevantArrows))]
