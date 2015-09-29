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

import Control.Monad (join, liftM2, forM_, zipWithM_)
import Control.Monad.IO.Class
import Data.Function (on)
import Data.List (zip3)
import qualified Data.Text   as T
import qualified Data.Vector as V
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable (peek)
import SDL
import SDL.Internal.Types (Window(..))
import SDL.Raw.Video (glGetDrawableSize)
import System.Environment (setEnv)
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))

import Board

-- |Data type containing SDL data structures.
data GraphicsHandle = GraphicsHandle
    {
        ghWindow    :: Window,
        ghRenderer  :: Renderer,
        ghTexture   :: Texture,
        ghTexture2x :: Texture
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
        True                -- Allow high DPI?
        False               -- Window input grabbed?
        Windowed            -- Window mode.
        Nothing             -- OpenGL config.
        Wherever            -- Window pos.
        False               -- Resizable?
        (V2 windowWidth windowHeight)  -- Window size.


-- |Initialize the SDL subsystem and return a handle
initUI :: MonadIO m => m GraphicsHandle
initUI = do
    -- Set some useful environment variables.
    liftIO $ do
        -- Set linear interpolation when rendering.
        -- Setting the environment variable because I cannot get the API function 
        -- that sets the hint to work.
        setEnv "SDL_RENDER_SCALE_QUALITY" "1"
        -- Also, disable fullscreen in OS X
        setEnv "SDL_VIDEO_MAC_FULLSCREEN_SPACES" "0"

    -- SDL initialization.
    initialize [InitEverything]

    window <- createWindow "Connect 4 Haskell" windowConfig
    renderer <- createRenderer window (-1) defaultRenderer
    rendererDrawColor renderer $= V4 0xFF 0xFF 0xFF 0x00

    -- Set logical size. Useful for resizing.
    rendererLogicalSize renderer $= Just (V2 windowWidth windowHeight)

    -- Create texture from BMP.
    texture   <- loadTexture renderer "assets/balls.bmp"
    texture2x <- loadTexture renderer "assets/balls2x.bmp"

    return $ GraphicsHandle window renderer texture texture2x
    where loadTexture renderer path = do
             surfaceTexture <- loadBMP path
             texture <- createTextureFromSurface renderer surfaceTexture
             freeSurface surfaceTexture
             return texture


-- | Function that creates a SDL rectangle given two tuples: (x0, y0) and
-- (width, height). This allows to eliminate some boilerplate code when we
-- create rectangles.
mkRect :: Num a => (a, a) -> (a, a) -> Rectangle a
mkRect (x0, y0) (w, h) = Rectangle (P (V2 x0 y0)) (V2 w h)


-- |Gets the size of the drawable contents of the window. This may differ
-- from the window size in HiDPI screens.
getDrawableSize :: MonadIO m => GraphicsHandle -> m (Int, Int)
getDrawableSize uiHandle =
    -- Since we are using SDL low level (raw) bindings, we have to do some
    -- magic. We allocate two variables that will contain the results of
    -- the 'glGetDrawableSize' call. Then, we extract the values of said
    -- variables (represented as pointers) and return them in a tuple.
    liftIO $ alloca $ \widthPtr -> (alloca $ \heightPtr -> do
        glGetDrawableSize windowPtr widthPtr heightPtr
        liftM2 (,) (fromPtr widthPtr) (fromPtr heightPtr)
    )
    where -- Unwrap the window type (returning a pointer to the Window data
          -- structure in memory)
          windowPtr = case ghWindow uiHandle of
                        Window ptr -> ptr
          -- Extract a value from pointer. The type of this is:
          -- Num a, Integral b => Ptr a -> IO b
          fromPtr = fmap fromIntegral . peek


-- |Wrapper around the 'copy' SDL function that converts the 'Rectangle's
-- inner type from Int to CInt before calling the function.
copy' :: MonadIO m =>
            GraphicsHandle -> Rectangle Int -> Rectangle Int -> m ()
copy' uiHandle srcRec dstRec = do
    let renderer  = ghRenderer uiHandle
        texture   = ghTexture uiHandle
        texture2x = ghTexture2x uiHandle
    (drawableWidth, drawableHeight) <- getDrawableSize uiHandle
    let gDiv = (/) `on` fromIntegral
        scaleFactor = min (drawableWidth `gDiv` windowWidth)
                          (drawableHeight `gDiv` windowHeight)
        (usedTexture, srcScale) = if scaleFactor > 1.0
                                     then (texture2x, 2)
                                     else (texture, 1)
    copy renderer usedTexture
                          (Just . fmap ((*srcScale) . fromIntegral) $ srcRec)
                          (Just . fmap fromIntegral $ dstRec)


-- |Renders a block consisting of the same source tile.
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
    forM_ dstRects $ \dstRect -> copy' uih srcRect dstRect
    where  -- Increment a rectangle in the positive Y axis.
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
    -- Edges.
    forM_ (zip3 borderSrcTiles dstInitialRects dstHowMany) $
        \(src, dst, counts) -> do
            let (howManyRows, howManyCols) = counts
            renderSame uiHandle src dst howManyRows howManyCols
    -- Corners.
    zipWithM_ (copy' uiHandle) cornerSrcTiles cornerDstRects
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


-- |Render the buttons at the bottom of the window.
renderButtons :: MonadIO m => GraphicsHandle -> m ()
renderButtons uiHandle = do
    let -- Calculate source and destination rects.
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
    zipWithM_ (copy' uiHandle) [srcRectRestartBut, srcRectExitBut]
                               [dstRectRestartBut, dstRectExitBut]


-- |Present the contents of the board to the window.
render :: MonadIO m => Board -> GraphicsHandle -> m ()
render board uiHandle = do
    let renderer = ghRenderer uiHandle
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
        copy' uiHandle srcRec dstRec
    -- Render the border of the board.
    renderBorder uiHandle
    -- Render the buttons at the bottom.
    renderButtons uiHandle
    -- Swap buffers, so what we have rendered is shown to the screen.
    present renderer
    where textureXCoord (Just X) = 0 * srcSquareSize
          textureXCoord (Just O) = 1 * srcSquareSize
          textureXCoord Nothing  = 2 * srcSquareSize

-- |Handle a SDL Event.
handleEvent :: MonadIO m => Event -> Board -> GraphicsHandle -> m (Maybe Input)
handleEvent ev board uiHandle = case eventPayload ev of
    WindowResizedEvent _ -> render board uiHandle >> return Nothing
    WindowMovedEvent _ -> render board uiHandle >> return Nothing
    -- 'Exit application' events.
    KeyboardEvent keyboardEvent ->
        return $ if isQPressed keyboardEvent then Just Exit
                                             else Nothing
    WindowClosedEvent _ -> return $ Just Exit
    QuitEvent -> return $ Just Exit
    -- 'Select column' events.
    MouseButtonEvent mouseEvent ->
        return $ if isLeftClickPressed mouseEvent then
                    Just . ColSelected . fromIntegral . getPressedCol
                    $ mouseEvent
                 else
                    Nothing
    _ -> return Nothing
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

-- |Gets input from user.
getInput :: MonadIO m => Board -> GraphicsHandle -> m Input
getInput board uiHandle = do
    event <- waitEvent
    mInput <- handleEvent event board uiHandle
    case mInput of
        Nothing    -> getInput board uiHandle
        Just input -> return input

-- |Shows a simple message in a dialog.
showMessageWindow :: MonadIO m => T.Text -> T.Text -> GraphicsHandle -> m ()
showMessageWindow title msg uiHandle =
    showSimpleMessageBox (Just $ ghWindow uiHandle) Information title msg


-- |Free the resources created by SDL.
cleanup :: MonadIO m => GraphicsHandle -> m ()
cleanup uiHandle = do
    destroyTexture $ ghTexture uiHandle
    destroyTexture $ ghTexture2x uiHandle
    destroyRenderer $ ghRenderer uiHandle
    destroyWindow $ ghWindow uiHandle
    quit
