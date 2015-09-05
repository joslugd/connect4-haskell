{-# LANGUAGE TemplateHaskell #-}

module GameState
(
    GameState (..),
    gsBoard, gsStdGen, gsCurrentPlayer
) where

import Board
import Control.Lens
import System.Random

-- Game input/output functions
-- |Represents the state of the program at a particular time.
data GameState = GameState { _gsBoard :: Board, _gsStdGen :: StdGen,
                             _gsCurrentPlayer :: Piece }
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

