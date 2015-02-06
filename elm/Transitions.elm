module Transitions where

import Dict
import List
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
                                 then List.repeat (1 + (i % range)) p
                                 else x) f,
              s')

getVars : Direction -> Position -> (Position, Int, Int)
getVars d (i,j) = let i' = if d == Up then i - 1 else if d == Down then i + 1 else i
                      j' = if d == Left then j - 1 else if d == Right then j + 1 else j
                      coord = if d == Up || d == Down then i else j
                      bound = if d == Up || d == Left then 0 else dimension - 1
                  in ((i', j'), coord, bound)


shift : Direction -> GameField -> GameField
shift d f = let f' = shiftOnce d f in if f' == f then f' else shift d f'

atToWhole : (Direction -> Position -> GameField -> GameField) -> Direction -> GameField -> GameField
atToWhole fct d f =  List.foldr (if d == Down || d == Right then (<<) else (>>))
                                identity
                                (List.map (fct d) (Dict.keys f))
                                f

shiftOnce : Direction -> GameField -> GameField
shiftOnce = atToWhole shiftOnceAt

shiftOnceAt : Direction -> Position -> GameField -> GameField
shiftOnceAt d q f = let (q', coord, bound) = getVars d q in Dict.map
  (if coord /= bound && List.isEmpty (entryAt f q')
   then \ p x -> if p == q'
                 then entryAt f q
                 else if p == q
                      then []
                      else x
   else \ p x -> x)
  f

merge : Direction -> GameField -> GameField
merge = atToWhole mergeOnceAt

mergeOnceAt : Direction -> Position -> GameField -> GameField
mergeOnceAt d q f = let (q', coord, bound) = getVars d q
                        entry = (entryAt f q)
                    in Dict.map
  (if coord /= bound && (not << List.isEmpty) entry && List.length (entryAt f q') == List.length entry
   then \ p x -> if p == q'
                 then entry ++ x
                 else if p == q
                      then []
                      else x
   else \ p x -> x)
  f

move : Direction -> GameField -> GameField
move d f = (shift d << merge d << shift d) f
