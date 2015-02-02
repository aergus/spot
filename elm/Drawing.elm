module Drawing where

import Color
import Dict
import Graphics.Element
import Graphics.Collage
import List

import Auxiliary (..)
import Parameters (..)
import Types (..)

colorOf : Block -> Color.Color
colorOf l = if List.isEmpty l
            then blockBg
            else let n = List.length l - 1
                 in Color.rgb ((211 * n) % 255) ((223 * n) % 255) 50


toForms : GameField -> List Graphics.Collage.Form
toForms f = let toOffset k = -sceneSize / 2 + blockSize / 2 + marginSize +
                             (blockSize + marginSize) * toFloat k in
  Dict.values (Dict.map
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
