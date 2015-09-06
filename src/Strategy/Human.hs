
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
    currentPlayer <- view gsCurrentPlayer
    curBoard <- view gsBoard

    -- Read column.
    mcol <- liftIO readCol

    validateCol mcol
    where validateCol (Just col) = return col
          validateCol Nothing    = humanStrategy
