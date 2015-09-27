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
import Data.List (zip3)
import qualified Data.Text   as T
import qualified Data.Vector as V
import Foreign.C.Types (CInt)
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
data Input = Exit | ColSelected Int deriving (Show)

-- |Width and height (in pixels) of each square in the window.
squareSize :: Num a => a
squareSize    = 64
-- |Width and height (in pixels) of each square in the texture.
srcSquareSize :: Num a => a
srcSquareSize = 64

-- |Width/Height of border of the board
borderSize :: Num a => a
borderSize = 16

-- |Board width.
boardWidth :: Num a => a
boardWidth = boardCols * squareSize

-- |Board height.
boardHeight :: Num a => a
boardHeight = boardRows * squareSize

-- |Width of contents of the window.
windowWidth :: Num a => a
windowWidth = squareSize * boardCols + 2*borderSize

-- |Height of contents of the window.
windowHeight :: Num a => a
windowHeight = squareSize * boardRows + 2*borderSize

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
        (V2 windowWidth windowHeight)  -- Window size.

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


renderSame :: MonadIO m
              => GraphicsHandle
              -> Rectangle Int -> Rectangle Int
              -> Int -> Int
              -> m ()
renderSame uih _       _       0 0    = return ()
renderSame uih srcRect dstRect 0 cols = do
    copy (ghRenderer uih) (ghTexture uih) (Just . fmap fromIntegral $ srcRect) 
                                          (Just . fmap fromIntegral $ dstRect)
    renderSame uih srcRect updatedDstRect 0 (cols-1)
    where updatedDstRect = case dstRect of
            Rectangle (P (V2 x y)) (V2 width height) ->
                Rectangle (P (V2 (x+width) y)) (V2 width height)
renderSame uih srcRect dstRect rows cols = do
    copy (ghRenderer uih) (ghTexture uih) (Just . fmap fromIntegral $ srcRect) 
                                          (Just . fmap fromIntegral $ dstRect)
    renderSame uih srcRect updatedDstRect (rows-1) cols
    where updatedDstRect = case dstRect of
            Rectangle (P (V2 x y)) (V2 width height) ->
                Rectangle (P (V2 x (y+height))) (V2 width height)


renderBorder :: MonadIO m => GraphicsHandle -> m ()
renderBorder uiHandle = do
    let renderer = ghRenderer uiHandle
        texture  = ghTexture uiHandle
    -- Border.
    forM_ (zip3 borderSrcTiles dstInitialRects dstHowMany) $
        \(src, dst, counts) -> do
            let (howManyRows, howManyCols) = counts
            renderSame uiHandle src dst howManyRows howManyCols
    -- Corners.
    forM_ (cornerSrcTiles `zip` cornerDstRects) $ \(src, dst) ->
        renderSame uiHandle src dst 1 0
    where           -- tl, bl, tr, br
          cornerSrcTiles = map (toRectangle . toTextureCoords) $
                               (,) <$> [0, 2] <*> [0, 2]
          cornerDstRects = map toRectangle $
                               (,) <$> [0, borderSize+boardWidth]
                                   <*> [0, borderSize+boardHeight]
          -- l, t, b, r
          borderSrcTiles = map (toRectangle . toTextureCoords)
                               [(0, 1), (1, 0), (1, 2), (2, 1)]
          dstInitialRects = map toRectangle 
                                [ (0, borderSize)
                                , (borderSize, 0)
                                , (borderSize, borderSize+boardHeight)
                                , (borderSize+boardWidth, borderSize) ]
          dstHowMany = [ (boardHeight `div` borderSize, 0)
                       , (0, boardWidth `div` borderSize)
                       , (0, boardWidth `div` borderSize)
                       , (boardHeight `div` borderSize, 0) ]
          toRectangle (x, y) = Rectangle (P (V2 x y)) (V2 borderSize borderSize)
          toTextureCoords (tileX, tileY) =
            (borderSize*tileX, srcSquareSize + borderSize*tileY)

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
        let dstRec = Rectangle (P (V2 (col * squareSize + borderSize) 
                                      (row * squareSize + borderSize)))
                               (V2 squareSize squareSize)
            srcRec = Rectangle (P (V2 (textureXCoord sq) 0))
                               (V2 srcSquareSize srcSquareSize)
        copy renderer texture (Just srcRec) (Just dstRec)
    renderBorder uiHandle
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
                P (V2 x y) -> 
                    and [ x > borderSize, x <= (borderSize+boardWidth)
                        , y > borderSize, y <= (borderSize+boardHeight) ]
        getPressedCol mouseEvent =
            case mouseButtonEventPos mouseEvent of
                P (V2 x _) -> (x - borderSize) `div` squareSize
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
