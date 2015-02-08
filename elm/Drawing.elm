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
                 in Color.rgb ((201 * n) % 256) ((223 * n) % 256) 255


toForms : GameField -> List Graphics.Collage.Form
toForms f = let toOffset k = sceneSize / 2 - blockSize / 2 - marginSize -
                             (blockSize + marginSize) * toFloat k in
  Dict.values (Dict.map
    (\ (i, j) x ->
      let l = valueOf x
          c = colorOf l
          form = Graphics.Collage.filled c (Graphics.Collage.rect blockSize blockSize)
      in Graphics.Collage.move (-(toOffset i), toOffset j)
                               (if l == 0
                                then form
                                else Graphics.Collage.group
        [form,
         (Graphics.Collage.toForm << Text.centered
                                  << Text.height (blockSize * 0.5)
                                  << Text.color Color.black
                                  << Text.bold
                                  << Text.fromString
                                  << toString) (2 ^ l)])
    )
    f)

toScene : GameField -> Bool -> Graphics.Element.Element
toScene f s = let size = round sceneSize in
  Graphics.Collage.collage size
                           size
                           ([Graphics.Collage.filled bg (Graphics.Collage.rect sceneSize sceneSize)] ++
                             (toForms << emptyField) dimension ++
                             (toForms f) ++
                             (if s
                              then [Graphics.Collage.filled (Color.rgba 0 0 0 0.95)
                                                            (Graphics.Collage.rect sceneSize sceneSize),
                                   (Graphics.Collage.toForm << Text.centered
                                                            << Text.height blockSize
                                                            << Text.color Color.white
                                                            << Text.bold
                                                            << Text.fromString) "Game\nOver"]
                              else []))
