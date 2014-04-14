import Board
import Plays
import Symmetry
import System.Environment
import System.IO.Error
import Data.Maybe as Maybe

data PlayerType = Internal | External deriving Eq --or if you prefer, AI | Human

playerVector :: Int -> ((Player, PlayerType), (Player, PlayerType))
playerVector 1 = ((Player1, External), (Player2, Internal))
playerVector 2 = ((Player1, External), (Player2, External))
playerVector _ = ((Player1, Internal), (Player2, Internal))

getExternalPlay :: IO (Integer, Integer)
getExternalPlay = do  
    putStrLn "Please choose your move by inputting a pair, e.g. (1, 1)."
    moveChosen <- getLine
    return (read moveChosen)

externalPlay :: Board -> IO (Integer, Integer)
externalPlay board = do
    verifiedMoveChosen <- getExternalPlay
    if verifiedMoveChosen `elem` validPlays board then
		return verifiedMoveChosen
    else externalPlay board

explainEndgame :: Player -> IO ()
explainEndgame Unclaimed = putStrLn "A strange game.  The only winning move is not to play."
explainEndgame x = putStrLn ("The winner is player "  ++ (show x ++ "."))

mainLoop :: (Board, (Player, PlayerType), (Player, PlayerType)) -> IO (Board, Player) 
mainLoop (board, (player, playerType), (nextPlayer, nextPlayerType)) = do
	putStrLn ""
	print board
	case winner board of
		Just x -> do
			explainEndgame x
			return (board, player)
		Nothing -> 
			if playerType == External then do
				nextExternalPlay <- externalPlay board
				mainLoop (advancePlay nextExternalPlay player board, (nextPlayer, nextPlayerType), (player, playerType))
			else 
				mainLoop (snd (nextGameState(player, board)), (nextPlayer, nextPlayerType), (player, playerType))

main = do
	(boardsize:hPlayers:_) <- getArgs
	let board = read boardsize
	let players = playerVector (read hPlayers)  
	mainLoop (newBoard board, fst players, snd players)
