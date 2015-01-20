module Auxiliary where

import List

import Types (..)

elemAt : List a -> Int -> a
elemAt l n = if n == 0 then List.head l else elemAt (List.tail l) (n - 1)

entryAt : (List (List a)) -> Position -> a
entryAt m (k, l) = elemAt (elemAt m k) l

indexedMatrixMap : (Position -> a -> b) -> (List (List a)) -> (List (List b))
indexedMatrixMap f m =  List.indexedMap (\ i l -> List.indexedMap (\ j x -> f (i, j) x) l) m
