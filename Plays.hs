module Plays where

import Board
import Symmetry
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe

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
validPlay (x, y) (Board board size) =
        Map.lookup (x, y) board == Just Unclaimed

advancePlay :: (Integer, Integer) -> Player -> Board -> Board
advancePlay (x, y) player (Board board size) =
        Board (Map.insert (x, y) player board) size

intermediateScore :: Integer -> Player -> Board -> Integer
intermediateScore morePlies player board
        | gameOver board = Maybe.fromJust (score board)
	| morePlies <= 0 = 
		if List.null (winningPlays player board) then 0
		else if player == Player1 then 1
		else -1
        | otherwise = --make a list of boards with the next play in them, map the list to intermediate scores, return the maximum or minimum as approproate
                playerEval (nextPlayer player) (List.map (intermediateScore (morePlies - 1) (nextPlayer player)) nextPlays)
                where   nextPlays = [ advancePlay p (nextPlayer player) board | p <- nonDuplicatePlays board ]

bestMove :: Player -> Board -> (Integer, Integer)
bestMove player board =
        snd (List.foldl' fitness acc possibleBoards) 
                where   possibleBoards = 
                                [ (intermediateScore 5 player (advancePlay p player board), p) | p <- nonDuplicatePlays board ] 
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
