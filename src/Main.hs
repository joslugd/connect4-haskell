
{- Connect4 implementation in Haskell.
 - NOTE: I know the comments in this file are a bit much extense. This has been
 - on purpose, so in case someone stumbles upon this source code and wants
 - to learn Haskell, they can somewhat understand what is going on (while
 - helping myself remember the concepts I have learned about Haskell).
-}

import Control.Lens
import Control.Monad.State
import System.Random
import Safe (readMay)

import Board
import GameState
import Strategy.Human

-- |Data type that represents the state of a match.
data MatchState = NotEnd | Win Piece | Tie

-- |Type alias for the state monad used in this file.
-- We will be using the state monad transformer so we can have a mutable game
-- state as well as we can use the capabilities of the IO monad (access to the
-- real world, i.e: reading from stdin and outputting to stdout) 
type GameMonad a = StateT GameState IO a

-- |Define player 1 strategy.
player1Strat :: GameStrategy
player1Strat = humanStrategy

-- |Define player 2 strategy.
player2Strat :: GameStrategy
player2Strat = humanStrategy

-- |Prints whose turn is it.
printTurn :: GameMonad ()
printTurn = do
    cur <- use gsCurrentPlayer
    liftIO . putStrLn $ "It is the turn for " ++ show cur

-- |Play a turn for a given player (i.e: read column and place piece).
playTurn :: GameMonad ()
playTurn = do
    -- The 'lens' package provides some functions analogous to the ones defined
    -- in the state monad. For example: 'use' is like 'gets', but the former
    -- lets us use a lens as a getter, while the later needs a explicit
    -- function over the state.
    currentPlayer <- use gsCurrentPlayer
    curBoard <- use gsBoard
    strat <- use (if currentPlayer == X then gsPlayer1Strat else gsPlayer2Strat)

    -- Obtain column from the strategy.
    col <- liftIO . runStrategy strat =<< get

    let mboard = putPiece currentPlayer col curBoard

    -- Check validity of the board, returning it if it's valid, or asking
    -- again for the column if it's not the case.
    verifyBoard mboard
    where verifyBoard Nothing = playTurn  -- If board is not valid, run again.
          verifyBoard (Just newBoard) = do
            -- Update state using lens functions.
            assign gsBoard newBoard
            gsCurrentPlayer %= nextPlayer

-- |Function called when the game ends. Prints final board and winner.
gameOver :: Maybe Piece -> GameMonad ()
gameOver winner = do
    finalBoard <- use gsBoard
    liftIO $ do
        putStrLn ""
        printBoard finalBoard
        putStrLn $ "The game is over! " ++
            maybe "Tie!" (\w -> show w ++ " wins!") winner

checkEnd :: GameMonad MatchState
checkEnd = do
    board <- use gsBoard
    return $ case checkWinner board of
        Nothing     -> if isFull board then Tie else NotEnd
        Just winner -> Win winner

-- |Loop of the game. This function repeats itself until there is a winner
-- (or no more plays are possible).
mainLoop :: GameMonad ()
mainLoop = do
    board <- use gsBoard
    liftIO $ do 
        putStrLn ""
        printBoard board
    printTurn
    playTurn
    matchSt <- checkEnd
    case matchSt of
        NotEnd -> mainLoop
        Win w  -> gameOver $ Just w
        Tie    -> gameOver Nothing

-- |Main function, entry point to the application.
main :: IO ()
main = do
    stdGen <- getStdGen
    let (initialPlayer, newGen) = random stdGen
        initialState = GameState emptyBoard newGen initialPlayer player1Strat player2Strat
    mainLoop `runStateT` initialState
    return ()
