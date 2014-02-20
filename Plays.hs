module Plays where

import Board
import Symmetry
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

playerEval :: Ord a => Player -> ([a] -> a)
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
validPlay (x, y) board =
        Map.lookup (x, y) board == Just Unclaimed

advancePlay :: (Integer, Integer) -> Player -> Board -> Board
advancePlay (x, y) player map =
        Map.insert (x, y) player map

intermediateScore :: Integer -> Integer -> Player -> Board -> Integer
intermediateScore morePlies size player board
        | gameOver board size = Maybe.fromJust (score board size)
	| morePlies <= 0 = 
		if length (winningPlays size player board) == 0 then 0
		else if player == Player1 then 1
		else -1
        | otherwise = --make a list of boards with the next play in them, map the list to intermediate scores, return the maximum or minimum as approproate
                (playerEval (nextPlayer player)) (List.map (intermediateScore (morePlies - 1) size (nextPlayer player)) nextPlays)
                where nextPlays = [ (advancePlay p (nextPlayer player) board) | p <- validPlays board ]

bestMove :: Integer -> Player -> Board -> (Integer, Integer)
bestMove size player board =
        snd (List.foldl' fitness acc possibleBoards) 
                where   possibleBoards = [ ((intermediateScore 5 size player (advancePlay p player board)), p) | p <- nonDuplicatePlays size board ] --break this up
                        fitness = (pairEval (pairPlayerEval player))
                        acc = (playerBound player, undefined)

nextGameState :: (Player, Board, Integer) -> (Player, Board, Integer)
nextGameState (player, board, size)
        | player == Unclaimed || noMoreMoves board = (Unclaimed, board, size)
        | otherwise =
                (nextPlayer player, advancePlay (bestMove size player board) player board, size)

winningPlays :: Integer -> Player -> Board -> [(Integer, Integer)]
winningPlays size player board
        | Map.null board = []
        | otherwise =
                [ p | p <- (validPlays board), isWinningSet size ((getPlayerMoves player board) ++ [p]) ]

