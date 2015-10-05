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
import Data.Functor (($>))
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import System.Random
import Safe (readMay)

import Board
import GameState
import qualified UI.Graphics as UI
import Strategy.Greedy
import Strategy.Negamax

-- |Type alias for the state monad used in this file.
-- We will be using the state monad transformer so we can have a mutable game
-- state as well as we can use the capabilities of the IO monad (access to the
-- real world, i.e: reading from stdin and outputting to stdout)
-- Update: Also we add the "ReaderT" transformer so we can access the
-- GraphicsHandler.
type GameMonad = ReaderT UI.GraphicsHandle (StateT GameState IO)


-- Algebraic data structure that may contain an (exceptional) action to
-- do about the game.
data GameAction = Exit | Restart

-- Takes an UI.Action (from the UI.Graphics module) and converts it to either
-- a GameAction or a column number.
fromUIInput :: UI.Input -> Either GameAction Int
fromUIInput uiAct =
    case uiAct of
        UI.Exit    -> Left Exit
        UI.Restart -> Left Restart
        UI.ColSelected col -> Right col


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
          strategies = (Nothing, Just $ negamaxStrategy 5)


-- |Reads player input using a 'GameStrategy' or reading from the user.
playerInput :: Maybe GameStrategy -> GameMonad (Either GameAction Int)
playerInput Nothing = do   -- No strategy -> human user.
    uiHandle <- ask
    board <- use gsBoard
    fmap fromUIInput $ UI.getInput board uiHandle
playerInput (Just strat) = do -- Run the strategy.
    state <- get
    liftIO . fmap Right $ strat `runStrategy` state

-- |Play a turn for a given player (i.e: read column and place piece).
-- Returns the input received from the player.
playTurn :: GameMonad (Either GameAction ())
playTurn = do
    -- The 'lens' package provides some functions analogous to the ones defined
    -- in the state monad. For example: 'use' is like 'gets', but the former
    -- lets us use a lens as a getter, while the later needs a explicit
    -- function over the state.
    currentPlayer <- use gsCurrentPlayer
    curBoard <- use gsBoard
    let mStrat = playerStrategy currentPlayer

    -- Obtain input from the strategy or human player.
    input <- playerInput mStrat

    case input of
        Left  _   -> return (input $> ())
        Right col ->
            let mboard = putPiece currentPlayer col curBoard
            -- Check validity of the board, returning it if it's valid, or
            -- asking again for the column if it's not the case.
            in  case mboard of
                    Nothing       -> playTurn
                    Just newBoard -> do
                         assign gsBoard newBoard
                         gsCurrentPlayer %= nextPlayer
                         return $ Right ()

-- |Function called when the game ends. Shows final board and winner.
gameOver :: Maybe Player -> GameMonad ()
gameOver mwinner = do
    finalBoard <- use gsBoard
    uiHandle <- ask
    UI.render finalBoard uiHandle
    -- Indicates that the game has ended, disabling interaction with the board.
    assign gsGameEnded True
    liftIO $ do
        UI.showMessageWindow "Game over"
                (maybe "The game ended in a draw"
                       (T.append "The winner of the game is "
                             . T.pack . toColor )
                       mwinner)
                uiHandle


-- |Loop of the game. This function repeats itself until there is a winner
-- (or no more plays are possible).
mainLoop :: GameMonad ()
mainLoop = do
    -- Display current state of the board.
    render

    -- Check if game ended.
    hasMatchEnded <- use gsGameEnded

    if hasMatchEnded then do
        board <- use gsBoard
        uiHandle <- ask
        action <- UI.getPassiveInput board uiHandle
        case fromUIInput action of
            Left action -> runAction action
            Right _ -> error "ColSelected event is not expected now"
    else do
        eInput <- playTurn
        either runAction continueGame eInput

    where render = do
              board <- use gsBoard
              uiHandle <- ask
              UI.render board uiHandle
          runAction Exit    = do
              uiHandle <- ask
              UI.cleanup uiHandle
          runAction Restart = do
              stdGen <- use gsStdGen
              initState <- liftIO $ initialState (Just stdGen)
              put initState
              render
              mainLoop
          continueGame () = do
              newBoard <- use gsBoard
              let matchSt = getMatchState newBoard
              case matchSt of
                  NotEnd -> return ()
                  Win w  -> gameOver $ Just w
                  Tie    -> gameOver Nothing
              mainLoop


-- |Returns the initial state.
initialState :: Maybe StdGen -> IO GameState
initialState prevStdGen = do
    stdGen <- maybe getStdGen return prevStdGen
    let (initialPlayer, newGen) = random stdGen
    return $ GameState emptyBoard newGen initialPlayer False


-- |Main function, entry point to the application.
main :: IO ()
main = do
    initState <- initialState Nothing
    uiHandle <- UI.initUI
    runGame mainLoop uiHandle initState
