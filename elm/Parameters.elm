module Parameters where

import Color
import Dict
import List

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
bg = Color.black

blockBg : Color.Color
blockBg = Color.rgb 200 200 200

emptyField : Int -> GameField
emptyField n = Dict.fromList (List.concatMap (\ i -> (List.map (\ k -> ((i, k), EmptyBlock)) [0 .. (n - 1)])) [0 .. (n - 1)])
