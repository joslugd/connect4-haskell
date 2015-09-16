{-
 - This file defines the strategy for a player controller by the computer.
 - -}

module Strategy.Greedy
(
    computerStrategy
) where

import Control.Lens (view)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V

import Board
import GameState
import Strategy.Evaluation

-- |Data type that contains information about each possible move to make.
-- This info is: 1. the column we inserted the piece into, 2. the resulting
-- board and 3. the rating given by the evaluation/heuristic function.
data Move = Move { mvColumn :: Int, mvResult :: Board, mvRating :: Int }

-- |The exported strategy. Determines (greedily) the best move to make.
computerStrategy :: GameStrategy
computerStrategy = do
    player <- view gsCurrentPlayer
    curBoard <- view gsBoard

    -- For each column, we try putting a piece (we discard invalid movements
    -- thanks to the 'mapMaybe' function), and then get the column for which
    -- we got the maximum rating.
    return . mvColumn . maximumBy (compare `on` mvRating) .
        mapMaybe (move player curBoard) $ [0..boardCols-1]
    where -- Creates the 'Move' data structure for a given column.
          move player board col = 
            case putPiece player col board of
                Nothing -> Nothing
                Just resultBoard -> Just $ Move col resultBoard
                    (evaluate player playerCoef opponentCoef resultBoard)
          -- Coefficients for the linear combination of winning rows. We
          -- temporarily use a greater weight for the opponent's so we can get
          -- a more aggresive AI that doesn't lose so easily. This may change
          -- later.
          playerCoef   = 1
          opponentCoef = -3
