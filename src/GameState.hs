{-# LANGUAGE TemplateHaskell #-}

module GameState
(
    GameState (..),
    gsBoard, gsStdGen, gsCurrentPlayer,
    gsPlayer1Strat, gsPlayer2Strat,
    Column, GameStrategy, runStrategy
) where

import Control.Lens
import Control.Monad.Reader
import System.Random

import Board

{- Type definitions for strategies. -}
-- | 'Column' is an alias for 'Int', for readability purposes.
type Column = Int

-- | A Strategy is a monadic computation that reads from the game state and
-- produces a column (which is the selected column). Also we should provide
-- access to the external world in case we need to read some data.
type GameStrategy = ReaderT GameState IO Column

-- |Run a strategy.
runStrategy :: GameStrategy -> GameState -> IO Column
runStrategy = runReaderT

-- |Represents the state of the program at a particular time.
data GameState = GameState { _gsBoard :: Board, _gsStdGen :: StdGen,
                             _gsCurrentPlayer :: Piece,
                             _gsPlayer1Strat :: GameStrategy, 
                             _gsPlayer2Strat :: GameStrategy }
-- Construct the lenses for the previously defined data structure. In layman
-- terms, lenses are abstractions used in functional programming to simulate
-- getters and setters in a composable, functional and convenient way. The
-- following line makes use of the "TemplateHaskell" extension, which acts
-- like some kind of preprocessor, thus generating the code for the lenses at
-- compile time (as opposed to at runtime). This way we can use the lenses
-- like any other defined object in the rest of our program.
-- The created lenses will have the same name as the corresponding field,
-- but without the leading underscore.
makeLenses ''GameState

