module Transitions where

import Dict
import List
import Maybe
import Random

import Auxiliary (..)
import Parameters (..)
import Types (..)

addRandomBlock : GameField -> Random.Seed -> (GameField, Random.Seed)
addRandomBlock f s =
  let frees = (Dict.keys << Dict.filter (\ p b -> isEmpty b)) f
      l = List.length frees
  in if l == 0
     then (f, s)
     else let range = dimension // 2
              (i, s') = Random.generate (Random.int 0 ((l - 1) * range)) s
          in (Dict.insert (elemAt frees (i // range))
                          (StationaryBlock (1 + i % range))
                          f,
              s')

nextPosByDir : Direction -> Position -> Position
nextPosByDir d (i, j) = case d of Up -> (i, j + 1)
                                  Down -> (i, j - 1)
                                  Left -> (i - 1, j)
                                  Right -> (i + 1, j)

atToWhole : (Direction -> Position -> GameField -> GameField) -> Direction -> GameField -> GameField
atToWhole fct d f =  List.foldr (if d == Up || d == Right then (<<) else (>>))
                                identity
                                (List.map (fct d) (Dict.keys f))
                                f

shift : Direction -> GameField -> GameField
shift = atToWhole shiftAt

shiftAt : Direction -> Position -> GameField -> GameField
shiftAt d p f = let x = entryAt f p
                in if isEmpty x
                   then f
                   else let s = scan d p f
                        in if s == p
                           then f
                           else Dict.insert s
                                            (MovingBlock (valueOf x)
                                                         (Maybe.withDefault p (originOf x))
                                                         (isMerged x))
                                            (Dict.insert p EmptyBlock f)

scan : Direction -> Position -> GameField -> Position
scan d p f = let p' = nextPosByDir d p
                 next = Dict.get p' f
             in if isEmpty (Maybe.withDefault NoBlock next)
                then scan d p' f
                else p

merge : Direction -> GameField -> GameField
merge = atToWhole mergeAt

mergeAt : Direction -> Position -> GameField -> GameField
mergeAt d p f = let x = entryAt f p
                    p' = nextPosByDir d p
                    next = entryAt f p'
                    v = valueOf x
                    v' = valueOf next
                in if isEmpty x || v /= v'
                   then f
                   else Dict.insert p'
                                    (MovingBlock (v + 1)
                                                 (Maybe.withDefault p (originOf x))
                                                 True)
                                    (Dict.insert p EmptyBlock f)

move : Direction -> GameField -> GameField
move d f = (shift d << merge d << shift d) f

nextMoves : GameField -> Moves
nextMoves f = {up = move Up f,
               down = move Down f,
               left = move Left f,
               right = move Right f}
