module Auxiliary where

import Dict
import List
import Maybe

import Types (..)

elemAt : List a -> Int -> a
elemAt l n = if n == 0 then List.head l else elemAt (List.tail l) (n - 1)

entryAt : Dict.Dict comparable (List v) -> comparable -> List v
entryAt d k = Maybe.withDefault [] (Dict.get k d)

log2 : Int -> Int
log2 n = case n of 0 -> -1
                   1 -> 0
                   k -> 1 + (log2 (n // 2))
