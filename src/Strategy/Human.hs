{-
 - This file defines the strategy for a human player. This strategy consists
 - simply in reading the column number from the keyboard.
 - -}

module Strategy.Human
(
    humanStrategy
) where

import Control.Monad.Reader
import Control.Lens (view)
import Safe (readMay)

import Board
import GameState

-- |Reads a column number from stdin.
readCol :: IO (Maybe Int)
readCol = liftM readMay getLine

humanStrategy :: GameStrategy
humanStrategy = do
    liftIO . putStrLn $ "Enter a valid column number (0.." ++ show (boardCols-1)
                                                           ++ ")"
    -- The 'lens' package provides some functions analogous to the ones defined
    -- in the reader monad. For example: 'view' is like 'asks', but the former
    -- lets us use a lens as a getter, while the later needs a explicit
    -- function over the state.
    curBoard <- view gsBoard

    -- Read column.
    mcol <- liftIO readCol

    -- Check that the input is correct. If it is not, ask again.
    validateCol mcol
    where validateCol (Just col) = return col
          validateCol Nothing    = humanStrategy
