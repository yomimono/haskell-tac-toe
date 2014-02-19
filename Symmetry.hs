module Symmetry where

import Board
import Data.List as List
import Data.Map as Map

validPlays :: Board -> [(Integer, Integer)]
validPlays board
        | noMoreMoves board = []
        | otherwise =
                keys $ Map.filter (== Unclaimed) board

--take advantage of board symmetry
nonDuplicatePlays :: Integer -> Board -> [ (Integer, Integer) ]
nonDuplicatePlays size board 
        | noMoreMoves board = []
        | hasSymmetry size board l (verticallySymmetricalPoint size) = List.intersect l valid
        | hasSymmetry size board t (horizontallySymmetricalPoint size) = List.intersect t valid
        | otherwise = valid
        where 
        	l = leftHalf size
        	t = topHalf size
        	valid = validPlays board

horizontallySymmetricalPoint :: Integer -> (Integer, Integer) -> (Integer, Integer) 
horizontallySymmetricalPoint 1 _ = (1, 1)
horizontallySymmetricalPoint size (x, y) = (size - x + 1, y)

verticallySymmetricalPoint :: Integer -> (Integer, Integer) -> (Integer, Integer)
verticallySymmetricalPoint 1 _ = (1, 1)
verticallySymmetricalPoint size (x, y) = (x, size - y + 1)

half :: Integer -> Integer
half x = ceiling ((fromIntegral x) / 2.0 :: Float)

leftHalf :: Integer -> [ (Integer, Integer)]
leftHalf 1 = [(1, 1)]
leftHalf boardSize = [ (x,y) | x <- [1..limit], y <- [1..boardSize] ]
        where limit = half boardSize

topHalf :: Integer -> [ (Integer, Integer)]
topHalf 1 = [(1, 1)]
topHalf boardSize = [ (x,y) | x <- [1..boardSize], y <- [1..limit] ]
        where limit = half boardSize

hasSymmetry :: Integer -> Board -> [(Integer, Integer) ] -> ( (Integer, Integer) -> (Integer, Integer) ) -> Bool
hasSymmetry size board generator symmetryFun
        | size == 0 = True
        | size == 1 = True
        | otherwise =
                and [ Map.lookup point board == Map.lookup (symmetryFun point) board | point <- generator ]
        