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

getBoardState :: Integer -> Integer -> Board -> Maybe Player
getBoardState x y map = Map.lookup (x, y) map 

advancePlay :: (Integer, Integer) -> Player -> Board -> Board
advancePlay (x, y) player map = 
	Map.insert (x, y) player map

showBoardRow :: Integer -> Integer -> Board -> String
showBoardRow row limit board 
	| row <= 0 || limit <= 0 || Map.null board = ""
	| otherwise =
		List.intercalate "|" [ show (Maybe.fromJust $ getBoardState row column board) | column <- [1..limit] ]

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
			
nextPlayer :: Player -> Player
nextPlayer Player1 = Player2			
nextPlayer Player2 = Player1

validPlay :: (Integer, Integer) -> Board -> Bool
validPlay (x, y) board 
	| noMoreMoves board = False
	| Map.lookup (x, y) board /= Just Unclaimed = False
	| otherwise = True

validPlays :: Board -> [(Integer, Integer)]
validPlays board
	| noMoreMoves board = []
	| otherwise =
		keys $ Map.filter (== Unclaimed) board

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
		

gameOver :: Board -> Integer -> Bool
gameOver board boardsize = ((winner board boardsize /= Just Unclaimed) || (noMoreMoves board))

winningPlays :: Integer -> Player -> Board -> [(Integer, Integer)]
winningPlays size player board 
	| Map.null board = []
	| otherwise =
		[ p | p <- (validPlays board), isWinningSet size ((getPlayerMoves player board) ++ [p]) ]

--try to win; failing that, try to keep the other player from winning; failing that, do something valid
chooseNextPlay :: Board -> Player -> Integer -> (Integer, Integer) 
chooseNextPlay board player size =
	head ( winningPlays size player board  ++  winningPlays size (nextPlayer player) board  ++ validPlays board)

getHumanPlay :: IO (Integer, Integer)
getHumanPlay = do
        putStrLn $ "Please choose your move by inputting a pair, e.g. (1, 1)."
	moveChosen <- getLine
	let verifiedMoveChosen = r moveChosen
		where r = read :: String -> (Integer, Integer) 
	return verifiedMoveChosen

humanPlay :: Board -> IO (Integer, Integer)
humanPlay board = do
	verifiedMoveChosen <- getHumanPlay
	if verifiedMoveChosen `elem` (validPlays board) then
		return verifiedMoveChosen
	else humanPlay board

--TODO: human player (player 1) should get interaction instead
nextGameState :: (Player, Board, Integer) -> (Player, Board, Integer)
nextGameState (player, board, size)
	| player == Unclaimed || noMoreMoves board = (Unclaimed, board, size)
	| otherwise =
		(nextPlayer player, advancePlay (chooseNextPlay board player size) player board, size)
