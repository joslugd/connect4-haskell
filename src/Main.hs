
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

-- |Reads a column number from stdin.
readCol :: IO (Maybe Int)
readCol = liftM readMay getLine

-- |Prints whose turn is it.
printTurn :: StateT GameState IO ()
printTurn = do
    cur <- use gsCurrentPlayer
    liftIO . putStrLn $ "It is the turn for " ++ show cur

-- |Play a turn for a given player (i.e: read column and place piece).
playTurn :: StateT GameState IO ()
playTurn = do
    liftIO . putStrLn $ "Enter a valid column number (0.." ++ show (boardCols-1)
                                                           ++ ")"
    -- The 'lens' package provides some functions analogous to the ones defined
    -- in the state monad. For example: 'use' is like 'gets', but the former
    -- lets us use a lens as a getter, while the later needs a explicit
    -- function over the state.
    currentPlayer <- use gsCurrentPlayer
    curBoard <- use gsBoard

    -- Read column.
    mcol <- liftIO readCol
    -- Nested do block. Needed because we have to run several actions
    -- corresponding to the Maybe monad and not to the external one (StateT).
    -- Here, we check the existence of the column and thst the resulting board
    -- is valid (returning the resulting board if that's the case, or Nothing
    -- otherwise).
    let mboard = do
            acol <- mcol
            putPiece currentPlayer acol curBoard
    -- Check validity of the board, returning it if it's valid, or asking
    -- again for the column if it's not the case.
    verifyBoard mboard
    where verifyBoard Nothing = playTurn  -- If board is not valid, ask again
          verifyBoard (Just newBoard) = do
            assign gsBoard newBoard
            gsCurrentPlayer %= nextPlayer

-- |Function called when the game ends. Prints final board and winner.
gameOver :: Maybe Piece -> StateT GameState IO ()
gameOver winner = do
    finalBoard <- use gsBoard
    liftIO $ do
        putStrLn ""
        printBoard finalBoard
        putStrLn $ "The game is over! " ++
            maybe "Tie!" (\w -> show w ++ " wins!") winner

-- |Data type that represents the state of a match.
data MatchState = NotEnd | Win Piece | Tie
checkEnd :: StateT GameState IO MatchState
checkEnd = do
    board <- use gsBoard
    return $ case checkWinner board of
        Nothing     -> if isFull board then Tie else NotEnd
        Just winner -> Win winner

-- |Loop of the game. This function repeats itself until there is a winner
-- (or no more plays are possible).
mainLoop :: StateT GameState IO ()
mainLoop = do
    board <- use gsBoard
    liftIO $ putStrLn ""
    liftIO $ printBoard board
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
        initialState = GameState emptyBoard newGen initialPlayer
    mainLoop `runStateT` initialState
    return ()
