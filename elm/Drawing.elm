module Drawing where

import Color
import Dict
import Graphics.Element
import Graphics.Collage
import List
import Maybe
import Text
import Time

import Auxiliary (..)
import Parameters (..)
import Transitions (..) -- to be removed
import Types (..)

blockRect : Graphics.Collage.Shape
blockRect = Graphics.Collage.rect blockSize blockSize

emptyForm : Graphics.Collage.Form
emptyForm = Graphics.Collage.toForm Graphics.Element.empty

colorOf : Int -> Color.Color
colorOf l = let n = l - 1
            in Color.rgb ((201 * n) % 256) ((223 * n) % 256) 255

toOffset : Int -> Float
toOffset k = sceneSize / 2 - blockSize / 2 - marginSize - (blockSize + marginSize) * toFloat k

moveByOffset : (Int, Int) -> Graphics.Collage.Form -> Graphics.Collage.Form
moveByOffset (i, j) = Graphics.Collage.move (-(toOffset i), toOffset j)

blockForm : Int -> Graphics.Collage.Form
blockForm l = (Graphics.Collage.group [Graphics.Collage.filled (colorOf l)
                                                                blockRect,
  (Graphics.Collage.toForm << Text.centered
                           << Text.height (blockSize * 0.4)
                           << Text.typeface ["sans-serif"]
                           << Text.color Color.black
                           << Text.bold
                           << Text.fromString
                           << toString) (2 ^ l)])

stationaryForms : GameField -> List Graphics.Collage.Form
stationaryForms f = List.map
  (\ (p, b) ->
    let drawBlock l = moveByOffset p (blockForm l)
    in case b of StationaryBlock n -> drawBlock n
                 MovingBlock n p True -> drawBlock (n - 1)
                 _ -> emptyForm)
  (List.filter (isFixed << snd) (Dict.toList f))

movingForms : Maybe Time.Time -> GameField -> List Graphics.Collage.Form
movingForms x f = Maybe.withDefault [] (Maybe.map
  (\ t -> List.map
    (\ ((i, j), b) ->
      case b of MovingBlock n (i', j') v -> let x = -(toOffset i)
                                                y = toOffset j
                                                x' = -(toOffset i')
                                                y' = toOffset j'
                                                scale = t / animationDuration
                                             in Graphics.Collage.move  (x' + scale * (x - x'),
                                                                        y' + scale * (y - y'))
                                                                       (blockForm (if v
                                                                                   then (n - 1)
                                                                                   else n))
                _ -> emptyForm)
    (List.filter (isAnimated << snd) (Dict.toList f)))
  x)

backgroundForms : List Graphics.Collage.Form
backgroundForms = List.map (\ p -> moveByOffset p (Graphics.Collage.filled blockBg blockRect))
                           (Dict.keys (emptyField dimension))

toScene : GameState -> Graphics.Element.Element
toScene s = let size = round sceneSize in
  Graphics.Collage.collage size
                           size
                           ([Graphics.Collage.filled bg (Graphics.Collage.rect sceneSize sceneSize)] ++
                             backgroundForms ++
                             (stationaryForms s.field) ++
                             (movingForms s.animation s.field) ++
                             (if let f = s.field
                                 in f == move Up f && f == move Down f
                                                   && f == move Left f
                                                   && f == move Right f
                              then [Graphics.Collage.filled (Color.rgba 0 0 0 0.95)
                                                            (Graphics.Collage.rect sceneSize sceneSize),
                                   (Graphics.Collage.toForm << Text.centered
                                                            << Text.height blockSize
                                                            << Text.typeface ["sans-serif"]
                                                            << Text.color Color.white
                                                            << Text.bold
                                                            << Text.fromString) "Game\nOver"]
                              else []))
