module Symmetry where

import Board
import Data.List as List
import Data.Map as Map

validPlays :: Board -> [(Integer, Integer)]
validPlays board =
         keys $ Map.filter (== Unclaimed) board

--take advantage of board symmetry
nonDuplicatePlays :: Integer -> Board -> [ (Integer, Integer) ]
nonDuplicatePlays size board =
    List.intersect (symmetricHalf size board) valid
        	where valid = validPlays board

--if the board has symmetry, return a half which should be considered, otherwise return everything
symmetricHalf :: Integer -> Board -> [ (Integer, Integer)]
symmetricHalf size board 
	| symmetry (leftHalf size) vsp && symmetry (topHalf size) hsp =
		List.intersect (topHalf size) (leftHalf size)
	| symmetry (tlHalf size) rdsp && symmetry (trHalf size) ldsp =
		List.intersect (tlHalf size) (trHalf size)
    | symmetry (topHalf size) hsp = (topHalf size)
    | symmetry (tlHalf size) rdsp = (tlHalf size)
    | symmetry (trHalf size) ldsp = (trHalf size)
    | otherwise = valid
    where 
    	vsp = verticallySymmetricalPoint size
    	hsp = horizontallySymmetricalPoint size
    	rdsp = rightDiagSymmetricalPoint size
    	ldsp = leftDiagSymmetricalPoint size
    	valid = validPlays board
    	symmetry = hasSymmetry size board

--kill these; make them inline
horizontallySymmetricalPoint :: Integer -> (Integer, Integer) -> (Integer, Integer) 
horizontallySymmetricalPoint size (x, y) = (size - x + 1, y)

verticallySymmetricalPoint :: Integer -> (Integer, Integer) -> (Integer, Integer)
verticallySymmetricalPoint size (x, y) = (x, size - y + 1)

leftDiagSymmetricalPoint :: Integer -> (Integer, Integer) -> (Integer, Integer)
leftDiagSymmetricalPoint _ (x, y) = (y, x)

rightDiagSymmetricalPoint :: Integer -> (Integer, Integer) -> (Integer, Integer)
rightDiagSymmetricalPoint size (x, y) 
	| x == y = (size - x + 1, size - x + 1) --left diags are easy
	| x == size - y + 1 = (x, y) -- right diags too
	| otherwise = transpose (leftDiagSymmetricalPoint size (y, x))
		where transpose(x, y) = (y, x)


half :: Integer -> Integer
half x = ((x+1) `div` 2)

leftHalf :: Integer -> [ (Integer, Integer)]
leftHalf 1 = [(1, 1)]
leftHalf boardSize = [ (x,y) | x <- [1..limit], y <- [1..boardSize] ]
        where limit = half boardSize

topHalf :: Integer -> [ (Integer, Integer)]
topHalf 1 = [(1, 1)]
topHalf boardSize = [ (x,y) | x <- [1..boardSize], y <- [1..limit] ]
        where limit = half boardSize

tlHalf :: Integer -> [ (Integer, Integer)]
tlHalf 1 = [(1, 1)]
tlHalf boardSize = [ (x, y) | x <- [1..boardSize], y <- [1..boardSize], x + y <= boardSize + 1]

trHalf :: Integer -> [ (Integer, Integer)]
trHalf 1 = [(1, 1)]
trHalf boardSize = [ (x, y) | x <- [1..boardSize], y <- [1..boardSize], (y >= x) ]

hasSymmetry :: Integer -> Board -> [(Integer, Integer) ] -> ( (Integer, Integer) -> (Integer, Integer) ) -> Bool
hasSymmetry size board generator symmetryFun
        | size == 1 = True
        | otherwise =
                and [ Map.lookup point board == Map.lookup (symmetryFun point) board | point <- generator ]
        