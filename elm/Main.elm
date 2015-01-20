module Main where

import Color
import Graphics.Element
import Graphics.Collage
import Keyboard
import Maybe
import List
import Random
import Signal
import Time

import Auxiliary (..)
import Parameters (..)
import Transitions (..)
import Types (..)

colorOf : Block -> Color.Color
colorOf l = if List.isEmpty l
            then blockBg
            else let n = List.length l - 1
                 --in elemAt [Color.blue, Color.yellow, Color.red, Color.green, Color.brown, Color.orange] n
                 --in Color.rgb (150 - 10 * n) (100 - 5 * n) (250 - 15 * n)
                 in Color.rgb ((211 * n) % 255) ((223 * n) % 255) ((227 * n) % 255)


toForms : GameField -> List Graphics.Collage.Form
toForms f = let toOffset k = -sceneSize / 2 + blockSize / 2 + marginSize +
                             (blockSize + marginSize) * toFloat k in
  List.concat (indexedMatrixMap
    (\ (i, j) x -> Graphics.Collage.move (toOffset j, -(toOffset i))
                                         (Graphics.Collage.filled (colorOf x)
                                                                  (Graphics.Collage.rect blockSize blockSize))
    )
    f)

toScene : GameField -> Graphics.Element.Element
toScene f = let size = round sceneSize in
  Graphics.Collage.collage size
                           size
                           ([Graphics.Collage.filled bg (Graphics.Collage.rect sceneSize sceneSize)] ++
                             (toForms << emptyField) dimension ++
                             (toForms f))

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
