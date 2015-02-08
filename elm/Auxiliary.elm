module Auxiliary where

import Dict
import List
import Maybe

import Types (..)

isEmpty : Block -> Bool
isEmpty b = case b of EmptyBlock -> True
                      _ -> False

valueOf : Block -> Int
valueOf b = case b of StationaryBlock i -> i
                      MovingBlock i p -> i
                      _ -> 0

elemAt : List a -> Int -> a
elemAt l n = if n == 0 then List.head l else elemAt (List.tail l) (n - 1)

entryAt : GameField -> Position -> Block
entryAt d k = Maybe.withDefault NoBlock (Dict.get k d)

log2 : Int -> Int
log2 n = case n of 0 -> -1
                   1 -> 0
                   k -> 1 + (log2 (n // 2))
