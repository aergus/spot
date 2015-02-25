module Auxiliary where

import Dict
import List
import Maybe

import Types (..)

isEmpty : Block -> Bool
isEmpty b = case b of EmptyBlock -> True
                      _ -> False

isFixed : Block -> Bool
isFixed b = case b of StationaryBlock _ -> True
                      MovingBlock _ _ True -> True
                      _ -> False

isAnimated : Block -> Bool
isAnimated b = case b of MovingBlock _ _ _ -> True
                         _ -> False

isMerged : Block -> Bool
isMerged b = case b of MovingBlock _ _ True -> True
                       _ -> False

valueOf : Block -> Int
valueOf b = case b of StationaryBlock i -> i
                      MovingBlock i _ _ -> i
                      _ -> 0

originOf : Block -> Maybe Position
originOf b = case b of MovingBlock _ p _ -> Just p
                       _ -> Nothing

stop : Block -> Block
stop b = case b of MovingBlock i _ _ -> StationaryBlock i
                   b' -> b'

moveAt : Moves -> Direction -> GameField
moveAt m d = case d of Up -> m.up
                       Down -> m.down
                       Left -> m.left
                       Right -> m.right

elemAt : List a -> Int -> a
elemAt l n = if n == 0 then List.head l else elemAt (List.tail l) (n - 1)

entryAt : GameField -> Position -> Block
entryAt d k = Maybe.withDefault NoBlock (Dict.get k d)
