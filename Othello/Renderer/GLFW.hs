module Othello.Renderer.GLFW where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)

import Othello.Core
import Othello.Game

play looper initState = do
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 0 0 0 0
 
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
    do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
  -- keep all line strokes as a list of points in an IORef
  lines <- newIORef []
  -- run the main loop
  passive lines
  -- finish up
  GLFW.closeWindow 
  GLFW.terminate

passive lines = do
  -- disable auto polling in swapBuffers
  GLFW.disableSpecial GLFW.AutoPollEvent
 
  -- keep track of whether ESC has been pressed
  quit <- newIORef False
 
  -- keep track of whether screen needs to be redrawn
  dirty <- newIORef True
 
  -- mark screen dirty in refresh callback which is often called
  -- when screen or part of screen comes into visibility.
  GLFW.windowRefreshCallback $= writeIORef dirty True
 
  -- use key callback to track whether ESC is pressed
  GLFW.keyCallback $= \k s -> 
     when (fromEnum k == fromEnum GLFW.ESC && s == GLFW.Press) $ 
        writeIORef quit True
 
  -- Terminate the program if the window is closed
  GLFW.windowCloseCallback $= (writeIORef quit True >> return True)
 
  -- by default start with waitForPress
  waitForPress dirty
  loop dirty quit
  where
    loop dirty quit = do
        GLFW.waitEvents
        -- redraw screen if dirty
        d <- readIORef dirty
 
        when d $ 
          render lines >> GLFW.swapBuffers
 
        writeIORef dirty False
        -- check if we need to quit the loop
        q <- readIORef quit
        unless q $
          loop dirty quit
 
    waitForPress dirty =
      do
        GLFW.mousePosCallback    $= \_ -> return ()
 
        GLFW.mouseButtonCallback $= \b s -> 
            when (b == GLFW.ButtonLeft && s == GLFW.Press) $
              do
                -- when left mouse button is pressed, add the point
                -- to lines and switch to waitForRelease action.
                (GL.Position x y) <- GL.get GLFW.mousePos
                modifyIORef lines (((x,y):) . ((x,y):))
                waitForRelease dirty
 
    waitForRelease dirty = 
      do 
        GLFW.mousePosCallback $= \(Position x y) ->
          do
            -- update the line with new ending position
            modifyIORef lines (((x,y):) . tail)
            -- mark screen dirty
            writeIORef dirty True
 
        GLFW.mouseButtonCallback $= \b s ->
            -- when left mouse button is released, switch back to
            -- waitForPress action.
            when (b == GLFW.ButtonLeft && s == GLFW.Release) $
              waitForPress dirty

toFloatVertices fi = mapM_ (GL.vertex . v)
  where v (x,y) = vertex3 (fi x) (fi y) 0

floatVertices :: [(Int,Int)] -> IO ()
floatVertices = toFloatVertices fromIntegral

renderTable :: (Float,Float) -> Float -> Float -> Float -> IO ()
renderTable (x,y) pxs margin size = do

  let toGLfloats = toFloatVertices id
      trans  (m,n) (xx,yy) = (xx + m, yy + n)
      trans'   = map $ trans (x,y)
      sq (m,n) = [(m,m),(m,n),(n,n),(n,m)]

      table    = trans' $ sq (0,pxs)
      (i,j)    = (margin,pxs-margin)
      trans''  = map $ trans (x+i,y+i)
      border   = let [b1,b2,b3,b4] = trans' $ sq (i,j) in [b1,b2,b2,b3,b3,b4,b4,b1]

      st       = (j-i) / size
      lines    = trans'' $ concat [ [(n,0),(n,j-i),(0,n),(j-i,n)] | n <- [st, 2*st .. (size-1) * st] ]

  GL.color $ color3 0.1 0.3 0.1
  GL.lineWidth $= 1.0
  GL.renderPrimitive Polygon (points2Vertexes table)

  GL.color $ color3 0.0 0.0 0.0
  GL.lineWidth $= 5.0
  GL.renderPrimitive GL.Lines $ toGLfloats border

  GL.color $ color3 0.0 0.0 0.0
  GL.lineWidth $= 1.0
  GL.renderPrimitive GL.Lines $ toGLfloats lines

  let ls      = trans'' $ sq (st*2, st*(size-2))
      circles = map (points2Vertexes . (flip circle) 3) ls

  forM_ circles (GL.renderPrimitive Polygon)

render lines = do
  l <- readIORef lines
  GL.clear [GL.ColorBuffer]

  renderTable (10,10) 400 20 8

  GL.color $ color3 1 1 1
  GL.renderPrimitive GL.Lines $ mapM_
      (\ (x, y) -> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0)) l

  forM l (\ (x, y) ->
    let (xx,yy) = (fromIntegral x, fromIntegral y) in
    GL.renderPrimitive Polygon (points2Vertexes $ circle (xx,yy) 10))
 
vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3

type Point   = (Float, Float)
type Polygon = [Point]

circle :: Point -> Float -> Polygon
circle (x,y) r = map (\t -> (x+r*cos (t), y+r*sin (t))) [0,0.2..(2*pi)]
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

points2GL :: [Point] -> [(GLfloat,GLfloat)]
points2GL l = [ (realToFrac x, realToFrac y) | (x,y) <- l ]

glPoints2Vertexes = mapM_ (\(x, y) -> vertex $ Vertex2 x y)
points2Vertexes   = glPoints2Vertexes . points2GL

