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
  let frees = (List.concat << Dict.values) (Dict.map (\ p x -> if List.isEmpty x then [p] else []) f)
      l = List.length frees
  in if l == 0
     then (f, s)
     else let range = dimension // 2
              (i, s') = Random.generate (Random.int 0 ((l - 1) * range)) s
          in (Dict.map (\ p x -> if p == elemAt frees (i // range)
                                 then List.repeat (2 ^ (i % range)) p
                                 else x) f,
              s')

nextPosByDir : Direction -> Position -> Position
nextPosByDir d (i, j) = case d of Up -> (i, j - 1)
                                  Down -> (i, j + 1)
                                  Left -> (i - 1, j)
                                  Right -> (i + 1, j)

atToWhole : (Direction -> Position -> GameField -> GameField) -> Direction -> GameField -> GameField
atToWhole fct d f =  List.foldr (if d == Down || d == Right then (<<) else (>>))
                                identity
                                (List.map (fct d) (Dict.keys f))
                                f

shift : Direction -> GameField -> GameField
shift = atToWhole shiftAt

shiftAt : Direction -> Position -> GameField -> GameField
shiftAt d p f = let x = entryAt f p
                in if List.isEmpty x
                   then f
                   else Dict.insert (scan d p f) x (Dict.insert p [] f)

scan : Direction -> Position -> GameField -> Position
scan d p f = let p' = nextPosByDir d p
                 next = Dict.get p' f
             in if List.isEmpty (Maybe.withDefault [(-1, -1)] next)
                then scan d p' f
                else p

merge : Direction -> GameField -> GameField
merge = atToWhole mergeAt

mergeAt : Direction -> Position -> GameField -> GameField
mergeAt d p f = let x = entryAt f p
                    p' = nextPosByDir d p
                    next = entryAt f p'
                in if List.isEmpty x || List.length x /= List.length next
                   then f
                   else Dict.insert p' (x ++ next) (Dict.insert p [] f)

move : Direction -> GameField -> GameField
move d f = (shift d << merge d << shift d) f
