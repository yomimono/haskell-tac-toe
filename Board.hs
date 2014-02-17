module Board where

import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe
import System.Random as Random

data Player = Unclaimed | Player1 | Player2 deriving (Eq, Ord)
type Board = Map (Integer, Integer) Player

instance Show Player where 
	show Unclaimed = " "
	show Player1 = "X"
	show Player2 = "O"

newBoard :: Integer -> Board
newBoard x  
	| x <= 0 = (Map.empty)
	| otherwise =
		Map.fromList [ ((p, y), Unclaimed ) | p <- [1..x], y <- [1..x] ]

showBoardRow :: Integer -> Integer -> Board -> String
showBoardRow row limit board 
	| row <= 0 || limit <= 0 || Map.null board = ""
	| otherwise =
		List.intercalate "|" [ show (Maybe.fromJust $ Map.lookup (row, column) board) | column <- [1..limit] ]

showBoardState :: Integer -> Board -> String
showBoardState limit board
	| Map.null board = ""
	| otherwise = 
		List.intercalate separator [ showBoardRow x limit board | x <- [1..limit ] ]
			where separator = "\n" ++ ((take (fromInteger (limit + limit - 1))) (repeat '-')) ++ "\n"

noMoreMoves :: Board -> Bool
noMoreMoves board
	| Map.null board = True
	| otherwise =
		Map.null $ Map.filter (== Unclaimed) board 
			
isWinningSet :: Integer -> [(Integer, Integer)] -> Bool
isWinningSet _ [] = False
isWinningSet 1 points = True
isWinningSet limit points 
	| (toInteger (length points)) < limit = False
	| otherwise = 
		or rows || or columns || (toInteger (length ldiags) >= limit) || (toInteger (length rdiags) >= limit)
		where 	rows = [ toInteger (length (List.filter ( == p) $ fst $ unzip points)) >= limit | p <- [1..limit] ]
			columns = [ toInteger (length (List.filter ( == p) $ snd $ unzip points)) >= limit | p <- [1..limit] ]
			ldiags = [ (x,y) | (x, y) <- points, x == y ]
			rdiags = [ (x, y) | (x, y) <- points, x == limit - y + 1 ]

getPlayerMoves :: Player -> Board -> [(Integer, Integer)]
getPlayerMoves player board
	| Map.null board = []
	| otherwise = 
		Map.keys $ fst $ Map.partition (== player) board 
	
winner :: Board -> Integer -> Maybe Player
winner board size
	| Map.null board = Nothing	
	| isWinningSet size (getPlayerMoves Player1 board) = Just Player1
	| isWinningSet size (getPlayerMoves Player2 board) = Just Player2
	| otherwise = Just Unclaimed

score :: Board -> Integer -> Integer
score board size 
	| Map.null board = 0
	| otherwise =
		case (winner board size) of
			Just Player1 -> 1
			Just Player2 -> -1
			Just Unclaimed -> 0

gameOver :: Board -> Integer -> Bool
gameOver board boardsize = ((winner board boardsize /= Just Unclaimed) || (noMoreMoves board))

