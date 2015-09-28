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

import Control.Monad (join, forM_)
import Control.Monad.IO.Class
import Data.List (zip3)
import qualified Data.Text   as T
import qualified Data.Vector as V
import Foreign.C.Types (CInt)
import SDL
import Linear (V2(..), V4(..))
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


-- Define dimensions of the various GUI elements:
squareSize, srcSquareSize, borderSize, boardWidth, boardHeight, buttonWidth,
    buttonHeight, windowWidth, windowHeight :: Num a => a

-- |Width and height (in pixels) of each square in the window.
squareSize    = 64
-- |Width and height (in pixels) of each square in the texture.
srcSquareSize = 64
-- |Width/Height of border of the board
borderSize = 16
-- |Board width.
boardWidth = boardCols * squareSize
-- |Board height.
boardHeight = boardRows * squareSize
-- |Button width.
buttonWidth = 192
-- |Button height.
buttonHeight = 32
-- |Width of contents of the window.
windowWidth = squareSize * boardCols + 2*borderSize
-- |Height of contents of the window.
windowHeight = squareSize * boardRows + 2*borderSize + buttonHeight


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
    rendererDrawColor renderer $= V4 0xFF 0xFF 0xFF 0x00

    -- Create texture from BMP.
    surfaceTexture <- loadBMP "assets/balls.bmp"
    texture <- createTextureFromSurface renderer surfaceTexture
    freeSurface surfaceTexture

    return $ GraphicsHandle window renderer texture


-- | Function that creates a SDL rectangle given two tuples: (x0, y0) and
-- (width, height). This allows to eliminate some boilerplate code when we
-- create rectangles.
mkRect :: Num a => (a, a) -> (a, a) -> Rectangle a
mkRect (x0, y0) (w, h) = Rectangle (P (V2 x0 y0)) (V2 w h)

-- |Wrapper around the 'copy' SDL function that converts the 'Rectangle's
-- inner type from Int to CInt before calling the function.
copy' :: MonadIO m =>
            Renderer -> Texture -> Rectangle Int -> Rectangle Int -> m ()
copy' renderer texture srcRec dstRec = 
    copy renderer texture (Just . fmap fromIntegral $ srcRec)
                          (Just . fmap fromIntegral $ dstRec)


-- |Renders a block consisting of the same tile.
renderSame :: MonadIO m
              => GraphicsHandle
              -> Rectangle Int -> Rectangle Int
              -> Int -> Int
              -> m ()
renderSame uih srcRect dstRect rows cols = do
    -- Iterate in both dimensions to obtain the list of all destination rects,
    let col = take rows $ iterate incrementDstRow dstRect
        dstRects = concat . take cols $ iterate (map incrementDstCol) col
    -- Map the drawing action to each rectangle.
    forM_ dstRects $ \dstRect -> copy' renderer texture srcRect dstRect
    where renderer = ghRenderer uih
          texture  = ghTexture uih
          -- Increment a rectangle in the positive Y axis.
          incrementDstRow rect = case rect of
            Rectangle (P (V2 x y)) (V2 width height) ->
                mkRect (x, y+height) (width, height)
          -- Increment a rectangle in the positive X axis.
          incrementDstCol rect = case rect of
            Rectangle (P (V2 x y)) (V2 width height) ->
                mkRect (x+width, y) (width, height)


-- Render the border of the board.
renderBorder :: MonadIO m => GraphicsHandle -> m ()
renderBorder uiHandle = do
    let renderer = ghRenderer uiHandle
        texture  = ghTexture uiHandle
    -- Edges.
    forM_ (zip3 borderSrcTiles dstInitialRects dstHowMany) $
        \(src, dst, counts) -> do
            let (howManyRows, howManyCols) = counts
            renderSame uiHandle src dst howManyRows howManyCols
    -- Corners.
    forM_ (cornerSrcTiles `zip` cornerDstRects) $ \(src, dst) ->
        copy' renderer texture src dst
    where -- top-left, bottom-left, top-right, bottom-right
          -- Here I use the applicative property of lists to save a few
          -- keystrokes.
          cornerSrcTiles = map (toRectangle . toTextureCoords) $
                               (,) <$> [0, 2] <*> [0, 2]
          cornerDstRects = map toRectangle $
                               (,) <$> [0, borderSize+boardWidth]
                                   <*> [0, borderSize+boardHeight]
          -- left, top, bottom, right
          borderSrcTiles = map (toRectangle . toTextureCoords)
                               [(0, 1), (1, 0), (1, 2), (2, 1)]
          dstInitialRects = map toRectangle 
                                [ (0, borderSize)
                                , (borderSize, 0)
                                , (borderSize, borderSize+boardHeight)
                                , (borderSize+boardWidth, borderSize) ]
          -- How many tiles to copy in each direction. Defines length of edges
          -- of the board.
          dstHowMany = [ (boardHeight `div` borderSize, 1)
                       , (1, boardWidth `div` borderSize)
                       , (1, boardWidth `div` borderSize)
                       , (boardHeight `div` borderSize, 1) ]
          -- Converts from tuple to rectangle. Resulting rectangles have fixed
          -- (= borderSize) area.
          toRectangle = (flip mkRect) (borderSize, borderSize)
          -- Maps from local tile coordinates to coordinates in the actual
          -- texture image.
          toTextureCoords (tileX, tileY) =
            (borderSize*tileX, srcSquareSize + borderSize*tileY)


renderButtons :: MonadIO m => GraphicsHandle -> m ()
renderButtons uiHandle = do
    let renderer = ghRenderer uiHandle
        texture = ghTexture uiHandle
        srcRectRestartBut = mkRect (0, squareSize+3*borderSize)
                                   (buttonWidth, buttonHeight)
        dstRectRestartBut = mkRect ( windowWidth `div` 2 - buttonWidth
                                   , windowHeight-buttonHeight )
                                   (buttonWidth, buttonHeight)
        srcRectExitBut = mkRect (0, squareSize+3*borderSize+buttonHeight)
                                (buttonWidth, buttonHeight)
        dstRectExitBut = mkRect ( windowWidth `div` 2
                                , windowHeight-buttonHeight)
                                (buttonWidth, buttonHeight)
        buttonRects = zip [srcRectRestartBut, srcRectExitBut]
                          [dstRectRestartBut, dstRectExitBut]
    mapM_ (uncurry $ copy' renderer texture) buttonRects


-- |Present the contents of the board to the window.
render :: MonadIO m => Board -> GraphicsHandle -> m ()
render board uiHandle = do
    let renderer = ghRenderer uiHandle
        texture  = ghTexture uiHandle
    -- Set whole backbuffer to the background color (in our case, white)
    clear renderer
    -- We obtain a list of board coordinates along with their contents.
    let twoDim       = V.fromList [(i,j) | j <- [0..boardRows-1],
                                           i <- [0..boardCols-1]]
        indexedBoard = V.zip twoDim (join board)
    -- Map the drawing action to each square in the aforementioned list.
    forM_ indexedBoard $ \((col, row), sq) -> do
        let -- Calculate source (from texture) and destination rects for the
            -- square.
            dstRec = Rectangle (P (V2 (col * squareSize + borderSize) 
                                      (row * squareSize + borderSize)))
                               (V2 squareSize squareSize)
            srcRec = Rectangle (P (V2 (textureXCoord sq) 0))
                               (V2 srcSquareSize srcSquareSize)
        -- Do the rendering.
        copy' renderer texture srcRec dstRec
    -- Render the border of the board.
    renderBorder uiHandle
    -- Render the buttons at the bottom.
    renderButtons uiHandle
    -- Swap buffers, so what we have rendered is shown to the screen.
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
