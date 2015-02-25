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

port initialization : Signal Int

main : Signal.Signal Graphics.Element.Element
main = Signal.map (\x -> Maybe.withDefault Graphics.Element.empty
                                          (Maybe.map toScene x))
                  (Signal.foldp update Nothing signal)

update : Event -> Maybe GameState -> Maybe GameState
update e x = case e of
  Initialization t -> let fd = emptyField dimension
                          (fd', sd') = addRandomBlock fd (Random.initialSeed t)
                          (fd'', sd'') = addRandomBlock fd' sd'
                      in Just {field = fd'',
                               seed = sd'',
                               animation = Nothing,
                               moves = nextMoves fd''}
  Move d -> Maybe.map (\ s -> if s.animation /= Nothing
                              then s
                              else let fd = moveAt s.moves d
                                   in if s.field == fd
                                      then s
                                      else {s | field <- fd,
                                                animation <- Just 0})
                      x
  Animation t -> Maybe.map
    (\ s -> Maybe.withDefault s
      (Maybe.map (\ k -> let k' = k + t
                         in if k' >= animationDuration
                            then let fd = (Dict.map (\ p b -> stop b) s.field)
                                     (fd', sd') = addRandomBlock fd s.seed
                                 in {s | field <- fd',
                                         seed <- sd',
                                         animation <- Nothing,
                                         moves <- nextMoves fd'}
                            else {s | animation <- Just k'})
      s.animation))
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
   Signal.map Animation (Time.fpsWhen 60 (Time.since animationDuration
                                                     relevantArrows))]
