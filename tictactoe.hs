import Board
import System.Environment
import Control.Monad.Loops
import Data.Maybe as Maybe

mainLoop :: (Board, Player, Integer) -> IO (Board, Player, Integer) 
mainLoop (board, player, size) = do
	putStrLn ""
	putStrLn $ showBoardState size board
	if gameOver board size
		then do
			putStrLn "GAME OVER"
			if (winner board size /= Just Unclaimed)
				then do
					putStrLn ("The winner is player "  ++ (show $ Maybe.fromJust (winner board size)) ++ ".")
				else do
					putStrLn "The game was a draw."
			return (board, player, size)
		else 
			mainLoop (nextBoard, nextPlayer(player), size)
				where 	nextBoard = mid (nextGameState(player, board, size))
					mid (x, y, z) = y

main = do
        (boardsize:_) <- getArgs
        let board = newBoard (read boardsize)
	let player = Player1
	mainLoop (board, player, (read boardsize))
