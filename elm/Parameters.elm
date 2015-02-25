module Parameters where

import Color
import Dict
import List
import Time

import Auxiliary (..)
import Types (..)

dimension : Int
dimension = 4

blockSize : Float
blockSize = 100

marginSize : Float
marginSize = 5

sceneSize : Float
sceneSize = let d = toFloat dimension in d * blockSize + (d + 1) * marginSize

bg : Color.Color
bg = Color.rgb 50 50 50

blockBg : Color.Color
blockBg = Color.rgb 225 225 225

mg : Color.Color
mg = Color.black

animationDuration : Time.Time
animationDuration = 100 * Time.millisecond

emptyField : Int -> GameField
emptyField n = Dict.fromList (List.concatMap (\ i -> (List.map (\ k -> ((i, k), EmptyBlock)) [0 .. (n - 1)])) [0 .. (n - 1)])
