{-
 - This file defines the strategy for a player controller by the computer.
 - -}

module Strategy.Computer
(
    computerStrategy
) where

import Control.Lens (view)
import Control.Monad (join)
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V

import Board
import GameState

-- |Auxiliary data type to represent the result of the evaluation function 
newtype NumPair a = NumPair { tuple :: (a, a) }

plus :: Num a => NumPair a -> NumPair a -> NumPair a
(NumPair (a, b)) `plus` (NumPair (c, d)) = NumPair (a+c, b+d)

zeroPair :: Num a => NumPair a
zeroPair = NumPair (0, 0)

-- |Evaluates the "goodness" of a board for a given player.
-- This function can be paraphrased as "the number of combinations of 4 adjacent
-- squares* that have at least one piece of the player, and none of the
-- adversary."
-- * Here, "adjacent" means that both squares are next to each other
--   horizontally, vertically or diagonally.
-- Returns a pair containing the evaluations for the given player and their
-- adversary, respectively.
winningRows :: Player -> Board -> (Int, Int)
winningRows pl board = tuple . winningRows' $ combs
    where combs = join $ (V.toList.) <$> [id, extractCols, extractDiagonals]
                                     <*> pure board
          winningRows' (row:rows) = countRow row `plus` winningRows' rows
          winningRows' [] = zeroPair
          countRow row
            | V.length row < 4 = zeroPair
            | otherwise = countFour (V.take 4 row) `plus` countRow (V.tail row)
          countFour f =
            let myPieceCount       = howMany pl f
                opponentPieceCount = howMany (nextPlayer pl) f
            in  case (myPieceCount, opponentPieceCount) of
                    (4, 0) -> NumPair (10000, 0)
                    (0, 4) -> NumPair (0, 10000)
                    (0, 0) -> zeroPair
                    (a, 0) -> NumPair (a, 0)
                    (0, b) -> NumPair (0, b)
                    (_, _) -> zeroPair
          howMany pi = V.length . V.filter (== (Just pi))

-- |The heuristic function, as pointed in the paper from
-- http://www.ics.uci.edu/~jlam2/connectk.pdf,
-- should be a linear combination of the 'winningRows' function calculated
-- for each of the players.
-- The 'Int' parameters to this function are the coefficients for said linear
-- combination.
evaluate :: Piece -> Int -> Int -> Board -> Int
evaluate pl plCoef opCoef board =
    case winningRows pl board of
        (winPl, winOp) -> plCoef * winPl + opCoef * winOp

data Move = Move { mvColumn :: Int, mvResult :: Board, mvRating :: Int }
computerStrategy :: GameStrategy
computerStrategy = do
    player <- view gsCurrentPlayer
    curBoard <- view gsBoard
    return . mvColumn . maximumBy ratingComparison . catMaybes .
        map (move player curBoard) $ [0..boardCols-1]
    where move player board col = 
            case putPiece player col board of
                Nothing -> Nothing
                Just resultBoard -> Just $ Move col resultBoard
                    (evaluate player playerCoef opponentCoef resultBoard)
          ratingComparison move1 move2 = compare (mvRating move1)
                                                 (mvRating move2)
          playerCoef   = 1
          opponentCoef = -2
