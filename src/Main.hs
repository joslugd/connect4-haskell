{-# LANGUAGE OverloadedStrings #-}

{- Connect4 implementation in Haskell.
 - NOTE: I know the comments in this file are a bit much extense. This has been
 - on purpose, so in case someone stumbles upon this source code and wants
 - to learn Haskell, they can somewhat understand what is going on (while
 - helping myself remember the concepts I have learned about Haskell).
-}

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import System.Random
import Safe (readMay)

import Board
import GameState
import qualified UI.Graphics as UI
import Strategy.Greedy

-- |Data type that represents the state of a match.
data MatchState = NotEnd | Win Piece | Tie

-- |Type alias for the state monad used in this file.
-- We will be using the state monad transformer so we can have a mutable game
-- state as well as we can use the capabilities of the IO monad (access to the
-- real world, i.e: reading from stdin and outputting to stdout) 
-- Update: Also we add the "ReaderT" transformer so we can access the
-- GraphicsHandler.
type GameMonad = ReaderT UI.GraphicsHandle (StateT GameState IO)

-- |The 'runGame' function evaluates the whole stack of a GameMonad monad.
-- We pass it the reader configuration and the initial state.
runGame :: GameMonad a -> UI.GraphicsHandle -> GameState -> IO a
runGame gm uiHandle initState = 
    (gm `runReaderT` uiHandle) `evalStateT` initState

-- |Returns the strategy for the given player ('Nothing' = human controlled).
playerStrategy :: Player -> Maybe GameStrategy
playerStrategy pl = pick strategies
    where pick | pl == X = fst
               | pl == O = snd
          -- Define player strategies.
          strategies = (Nothing, Just computerStrategy)

playerInput :: GameState -> Maybe GameStrategy -> GameMonad (Maybe Int)
playerInput _ Nothing = do
    input <- UI.getInput
    return $ case input of
        UI.Exit -> Nothing
        UI.ColSelected col -> Just col
playerInput state (Just strat) = liftIO . fmap Just $ strat `runStrategy` state

-- |Play a turn for a given player (i.e: read column and place piece).
-- Returns False if user wants to close the application. True otherwise.
playTurn :: GameMonad Bool
playTurn = do
    -- The 'lens' package provides some functions analogous to the ones defined
    -- in the state monad. For example: 'use' is like 'gets', but the former
    -- lets us use a lens as a getter, while the later needs a explicit
    -- function over the state.
    currentPlayer <- use gsCurrentPlayer
    curBoard <- use gsBoard
    let mStrat = playerStrategy currentPlayer

    -- Obtain column from the strategy or human player.
    state <- get
    mcol  <- playerInput state mStrat

    if isNothing mcol then
        return False
    else
        let col    = fromJust mcol
            mboard = putPiece currentPlayer col curBoard
        -- Check validity of the board, returning it if it's valid, or asking
        -- again for the column if it's not the case.
        in  verifyBoard mboard
    where verifyBoard Nothing = playTurn  -- If board is not valid, run again.
          verifyBoard (Just newBoard) = do
            -- Update state using lens functions.
            assign gsBoard newBoard
            gsCurrentPlayer %= nextPlayer
            return True

-- |Function called when the game ends. Prints final board and winner.
gameOver :: Maybe Player -> GameMonad ()
gameOver mwinner = do
    finalBoard <- use gsBoard
    uiHandle <- ask
    UI.render finalBoard uiHandle
    liftIO $ do
        UI.showMessageWindow "Game over"
                (maybe "The game ended in a draw"
                       (T.append "The winner of the game is "
                             . T.pack . toColor )
                       mwinner)
                uiHandle
        UI.cleanup uiHandle

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
    uiHandle <- ask

    UI.render board uiHandle
    notQuitted <- playTurn
    if notQuitted then do
        matchSt <- checkEnd
        case matchSt of
            NotEnd -> mainLoop
            Win w  -> gameOver $ Just w
            Tie    -> gameOver Nothing
    else
        UI.cleanup uiHandle

-- |Main function, entry point to the application.
main :: IO ()
main = do
    stdGen <- getStdGen
    let (initialPlayer, newGen) = random stdGen
        initialState = GameState emptyBoard newGen initialPlayer
    uiHandle <- UI.initUI
    runGame mainLoop uiHandle initialState
