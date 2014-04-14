module Plays where

import Board
import Symmetry
import Data.Array.IArray as Array
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe

data BoardState = BoardState Player Board deriving (Ord, Show)
data Score = Score Integer deriving (Ord, Eq, Show)

instance Eq BoardState where
  (BoardState player1 board1) == (BoardState player2 board2) = 
					player1 == player2 && board1 == board2

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

playerEval :: Ord a => Player -> [a] -> a
playerEval Player1 = List.maximum
playerEval Player2 = List.minimum

playerBound :: Player -> Score
playerBound Player1 = Score (-100000)
playerBound Player2 = Score 100000

pairPlayerEval :: Player -> Ordering
pairPlayerEval Player1 = GT
pairPlayerEval Player2 = LT

pairEval :: Ordering -> (Score, b) -> (Score, b) -> (Score, b)
pairEval ordering (pair,x) (other,y) = 
        if compare pair other == ordering then (pair, x) else (other, y)

validPlay :: (Integer, Integer) -> Board -> Bool
validPlay point (Board board size) =
        board Array.! point == Unclaimed

advancePlay :: (Integer, Integer) -> Player -> Board -> Board
advancePlay point player (Board board size) =
        Board (board Array.// [ (point, player) ] ) size

intermediateScore :: Map BoardState Score -> Integer -> Player -> Board -> (Score, Map BoardState Score)
intermediateScore memoTable plies player board =
				--map of the tuple (player, board) to integer - scores are dependent on who's to play next
				case Map.lookup (BoardState player board) memoTable of 
								Just p -> (p, memoTable)
								Nothing -> intermediateScore' memoTable plies player board

whichScore :: Map BoardState Score -> Integer -> Player -> Board -> (Score, Map BoardState Score)
whichScore memoTable plies player board =
  if gameOver board || List.null possibilities || plies <= 0 then 
				case score board of
								Just x -> (Score x, memoTable)
								Nothing -> (Score 0, memoTable)
	else playerEval adversary (List.map (intermediateScore memoTable (plies - 1) adversary) possibilities)
				where 
				adversary = nextPlayer player
				possibilities = nextPlays adversary board

intermediateScore' :: Map BoardState Score -> Integer -> Player -> Board -> (Score, Map BoardState Score)
intermediateScore' memoTable plies player board =
				(score, Map.insert (BoardState player board) score newMap)
				where (score, newMap) = whichScore memoTable plies player board

nextPlays :: Player -> Board -> [ Board ]
nextPlays nextPlayer board =
				[ advancePlay p nextPlayer board | p <- nonDuplicatePlays board ]

bestMove :: Player -> Board -> (Integer, Integer)
bestMove player board =
				case (winningPlays player board, winningPlays (nextPlayer player) board) of
								(x : [], _) -> x
								([], x : []) -> x
								_ ->
												snd (List.foldl' fitness acc possibleBoards) 
												where   
												openSpots = nonDuplicatePlays board
												depthLimit = 1000 --if List.length openSpots > 5 then 1 else 5
												possibleBoards = [ (fst $ intermediateScore (Map.fromList []) depthLimit player (advancePlay p player board), p) | p <- openSpots ] 
												fitness = pairEval (pairPlayerEval player)
												acc = (playerBound player, undefined)

nextGameState :: (Player, Board) -> (Player, Board)
nextGameState (player, board)
        | player == Unclaimed || noMoreMoves board = (Unclaimed, board)
        | otherwise =
                (nextPlayer player, advancePlay (bestMove player board) player board)

winningPlays :: Player -> Board -> [(Integer, Integer)]
winningPlays player board =
        [ p | p <- validPlays board, isWinningSet board (p : getPlayerMoves player board)]
