module Plays where

import Board
import Symmetry
import Data.Array.IArray as Array
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe

data BoardState = BoardState Player Board deriving Ord

instance Eq BoardState where
  (BoardState player1 board1) == (BoardState player2 board2) = 
					player1 == player2 && board1 == board2

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

playerEval :: Ord a => Player -> [a] -> a
playerEval Player1 = List.maximum
playerEval Player2 = List.minimum

playerBound :: Player -> Integer
playerBound Player1 = -100000
playerBound Player2 =  100000

pairPlayerEval :: Player -> Ordering
pairPlayerEval Player1 = GT
pairPlayerEval Player2 = LT

pairEval :: Ord a => Ordering -> (a, b) -> (a, b) -> (a, b)
pairEval ordering pair other =
        if compare (fst pair) (fst other) == ordering then
                pair
        else other

validPlay :: (Integer, Integer) -> Board -> Bool
validPlay point (Board board size) =
        board Array.! point == Unclaimed

advancePlay :: (Integer, Integer) -> Player -> Board -> Board
advancePlay point player (Board board size) =
        Board (board Array.// [ (point, player) ] ) size

intermediateScore :: Map BoardState Integer -> Integer -> Player -> Board -> (Map BoardState Integer, Integer)
intermediateScore memoTable plies player board =
				--map of the tuple (player, board) to integer - scores are dependent on who's to play next
				case Map.lookup (BoardState player board) memoTable of 
								Just p -> (memoTable, p)
								Nothing -> intermediateScore' memoTable plies player board

whichScore :: Map BoardState Integer -> Integer -> Player -> Board -> (Map BoardState Integer, Integer)
whichScore memoTable plies player board =
  if gameOver board || List.null possibilities || plies <= 0 then 
				case score board of
								Just x -> (memoTable, x)
								Nothing -> (memoTable, 0)
	else playerEval adversary (List.map (intermediateScore memoTable (plies - 1) adversary) possibilities) 
				where 
				adversary = nextPlayer player
				possibilities = nextPlays adversary board

intermediateScore' :: Map BoardState Integer -> Integer -> Player -> Board -> (Map BoardState Integer, Integer)
intermediateScore' memoTable plies player board =
				(Map.insert (BoardState player board) score newMap, score)
				where (newMap, score) = whichScore memoTable plies player board

nextPlays :: Player -> Board -> [ Board ]
nextPlays nextPlayer board =
				[ advancePlay p nextPlayer board | p <- nonDuplicatePlays board ]

bestMove :: Player -> Board -> (Integer, Integer)
bestMove player board =
				snd (List.foldl' fitness acc possibleBoards) 
                where   
								openSpots = nonDuplicatePlays board
								depthLimit = if List.length openSpots > 3 then 2 else 5
								possibleBoards = [ (snd $ intermediateScore (Map.fromList []) depthLimit player (advancePlay p player board), p) | p <- openSpots ] 
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
