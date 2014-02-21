import Board
import Plays
import Symmetry
import System.Environment
import System.IO.Error
import Data.Maybe as Maybe

getHumanPlay :: IO (Integer, Integer)
getHumanPlay = do  
    putStrLn "Please choose your move by inputting a pair, e.g. (1, 1)."
    moveChosen <- getLine
    return (read moveChosen)

humanPlay :: Board -> IO (Integer, Integer)
humanPlay board = do
    verifiedMoveChosen <- getHumanPlay
    if verifiedMoveChosen `elem` validPlays board then
		return verifiedMoveChosen
    else humanPlay board

explainEndgame :: Player -> IO ()
explainEndgame Unclaimed = putStrLn "The game was a draw."
explainEndgame x = putStrLn ("The winner is player "  ++ (show x ++ "."))

mainLoop :: (Board, Player) -> IO (Board, Player) 
mainLoop (board, player) = do
	putStrLn ""
	putStrLn $ showBoardState board
	case winner board of
		Just x -> do
			explainEndgame x
			return (board, player)
		Nothing -> 
			if player == Player1 then do
				nextHumanPlay <- humanPlay board
				mainLoop (advancePlay nextHumanPlay player board, nextPlayer player)
			else 
				mainLoop (snd (nextGameState(player, board)), nextPlayer player)

main = do
	(boardsize:_) <- getArgs
	let board = read boardsize
	mainLoop (newBoard board, Player1)