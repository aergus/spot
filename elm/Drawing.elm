module Drawing where

import Color
import Dict
import Graphics.Element
import Graphics.Collage
import List
import Text

import Auxiliary (..)
import Parameters (..)
import Types (..)

colorOf : Int -> Color.Color
colorOf l = if l == 0
            then blockBg
            else let n = l - 1
                 in Color.rgb ((221 * n) % 255) ((25 * n) % 255) 250


toForms : GameField -> List Graphics.Collage.Form
toForms f = let toOffset k = -sceneSize / 2 + blockSize / 2 + marginSize +
                             (blockSize + marginSize) * toFloat k in
  Dict.values (Dict.map
    (\ (i, j) x ->
      let l = List.length x
          c = colorOf l
          form = Graphics.Collage.filled c (Graphics.Collage.rect blockSize blockSize)
      in Graphics.Collage.move (toOffset j, -(toOffset i))
                               (if l == 0
                                then form
                                else Graphics.Collage.group
        [form,
         (Graphics.Collage.toForm << Text.centered
                                  << Text.height (blockSize * 0.5)
                                  << Text.color Color.black
                                  << Text.bold
                                  << Text.fromString
                                  << toString) (2 * l)])
    )
    f)

toScene : GameField -> Graphics.Element.Element
toScene f = let size = round sceneSize in
  Graphics.Collage.collage size
                           size
                           ([Graphics.Collage.filled bg (Graphics.Collage.rect sceneSize sceneSize)] ++
                             (toForms << emptyField) dimension ++
                             (toForms f))
