module Parameters where

import Color
import List

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

initField : Int -> GameField
initField n = [[[], [], [], [(2, 1)]],
               [[], [], [], []],
               [[], [], [], []],
               [[], [(2, 1)], [], []]]

emptyField : Int -> GameField
emptyField n = List.repeat n (List.repeat n [])

