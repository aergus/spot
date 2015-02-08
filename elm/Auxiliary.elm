module Auxiliary where

import Dict
import List
import Maybe

import Types (..)

isEmpty : Block -> Bool
isEmpty b = case b of EmptyBlock -> True
                      _ -> False

isMerged : Block -> Bool
isMerged b = case b of MovingBlock i p True -> True
                       _ -> False

valueOf : Block -> Int
valueOf b = case b of StationaryBlock i -> i
                      MovingBlock i p v -> i
                      _ -> 0

originOf : Block -> Maybe Position
originOf b = case b of MovingBlock i p v -> Just p
                       _ -> Nothing


elemAt : List a -> Int -> a
elemAt l n = if n == 0 then List.head l else elemAt (List.tail l) (n - 1)

entryAt : GameField -> Position -> Block
entryAt d k = Maybe.withDefault NoBlock (Dict.get k d)

addThird : c -> (a, b) -> (a, b, c)
addThird z (x, y) = (x, y, z)

log2 : Int -> Int
log2 n = case n of 0 -> -1
                   1 -> 0
                   k -> 1 + (log2 (n // 2))
