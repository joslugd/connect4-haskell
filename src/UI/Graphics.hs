{-# LANGUAGE OverloadedStrings #-}

module UI.Graphics
(
    GraphicsHandle,
    Input(..),
    initUI,
    render,
    getInput,
    showMessageWindow,
    cleanup
) where

import Control.Monad (join, unless, forM_)
import Control.Monad.IO.Class
import qualified Data.Text   as T
import qualified Data.Vector as V
import SDL
import Linear (V2(..))
import Linear.Affine (Point(..))

import Board

-- |Data type containing SDL data structures.
data GraphicsHandle = GraphicsHandle
    {
        ghWindow   :: Window,
        ghRenderer :: Renderer,
        ghTexture  :: Texture
    }

-- |Data type that represents a input from the user.
data Input = Exit | ColSelected Int

-- |Width and height (in pixels) of each square in the window.
squareSize :: Num a => a
squareSize    = 64
-- |Width and height (in pixels) of each square in the texture.
srcSquareSize :: Num a => a
srcSquareSize = 64

-- |Configuration for the SDL window.
windowConfig :: WindowConfig
windowConfig = WindowConfig
        True                -- Window border?
        False               -- High DPI?
        False               -- Window input grabbed?
        Windowed            -- Window mode.
        Nothing             -- OpenGL config.
        Wherever            -- Window pos.
        False               -- Resizable?
        (V2 (squareSize*boardCols) (squareSize*boardRows))  -- Window size.

-- |Initialize the SDL subsystem and return a handle
initUI :: MonadIO m => m GraphicsHandle
initUI = do
    -- Typical SDL initialization.
    initialize [InitEverything]
    window <- createWindow "Connect 4 Haskell" windowConfig
    renderer <- createRenderer window (-1) defaultRenderer

    -- Create texture from BMP.
    surfaceTexture <- loadBMP "assets/balls.bmp"
    texture <- createTextureFromSurface renderer surfaceTexture
    freeSurface surfaceTexture

    return $ GraphicsHandle window renderer texture

-- |Present the contents of the board to the window.
render :: MonadIO m => Board -> GraphicsHandle -> m ()
render board uiHandle = do
    let renderer = ghRenderer uiHandle
        texture  = ghTexture uiHandle
    clear renderer
    let twoDim       = V.fromList [(i,j) | j <- [0..boardRows-1],
                                           i <- [0..boardCols-1]]
        indexedBoard = V.zip twoDim (join board)
    forM_ indexedBoard $ \((col, row), sq) -> do
        let dstRec = Rectangle (P (V2 (col * squareSize) (row * squareSize)))
                               (V2 squareSize squareSize)
            srcRec = Rectangle (P (V2 (textureXCoord sq) 0))
                               (V2 srcSquareSize srcSquareSize)
        copy renderer texture (Just srcRec) (Just dstRec)
    present renderer
    where textureXCoord (Just X) = 0 * srcSquareSize
          textureXCoord (Just O) = 1 * srcSquareSize
          textureXCoord Nothing  = 2 * srcSquareSize

-- |Gets input from user.
getInput :: MonadIO m => m Input
getInput = do
    event <- waitEvent
    let mInput = handleEvent event
    maybe getInput return mInput
    where
        isQPressed kbdEvent =
            keyboardEventKeyMotion kbdEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym kbdEvent) == KeycodeQ
        isLeftClickPressed mouseEvent =
            mouseButtonEventButton mouseEvent == ButtonLeft &&
            mouseButtonEventMotion mouseEvent == Pressed &&
            withinBounds mouseEvent
        withinBounds mouseEvent =
            case mouseButtonEventPos mouseEvent of
                P (V2 _ y) -> y > 0
        getPressedCol mouseEvent =
            case mouseButtonEventPos mouseEvent of
                P (V2 x _) -> x `div` squareSize
        handleEvent ev = case eventPayload ev of
            -- 'Exit application' events.
            KeyboardEvent keyboardEvent ->
                if isQPressed keyboardEvent then Just Exit
                                            else Nothing
            WindowClosedEvent _ -> Just Exit
            QuitEvent -> Just Exit
            -- 'Select column' events.
            MouseButtonEvent mouseEvent ->
                if isLeftClickPressed mouseEvent then
                    Just . ColSelected . fromIntegral . getPressedCol
                        $ mouseEvent
                else
                    Nothing
            _ -> Nothing

-- |Shows a simple message in a dialog.
showMessageWindow :: MonadIO m => T.Text -> T.Text -> GraphicsHandle -> m ()
showMessageWindow title msg uiHandle =
    showSimpleMessageBox (Just $ ghWindow uiHandle) Information title msg

-- |Free the resources created by SDL.
cleanup :: MonadIO m => GraphicsHandle -> m ()
cleanup uiHandle = do
    destroyTexture $ ghTexture uiHandle
    destroyRenderer $ ghRenderer uiHandle
    destroyWindow $ ghWindow uiHandle
    quit
