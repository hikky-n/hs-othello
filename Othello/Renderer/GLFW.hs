module Othello.Renderer.GLFW where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
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
  GL.clearColor $= GL.Color4 0 0 0 0
 
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) -> do
    GL.viewport   $= (GL.Position 0 0, size)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity

    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
    -- let (iw,ih) = ((realToFrac w) / 2,(realToFrac h) / 2)
    -- GL.frustum (-iw) iw (-ih) ih (-1) (1)

  -- disable auto polling in swapBuffers
  GLFW.disableSpecial GLFW.AutoPollEvent

  state <- newIORef initState
  dirty <- newIORef True
  quit  <- newIORef False
  posM  <- newIORef (GL.Position 0 0)
 
  -- mark screen dirty in refresh callback which is often called
  -- when screen or part of screen comes into visibility.
  GLFW.windowRefreshCallback $= writeIORef dirty True
 
  -- use key callback to track whether ESC is pressed
  GLFW.keyCallback $= \key st -> do
    when (fromEnum key == fromEnum GLFW.ESC && st == GLFW.Press) $ 
      writeIORef quit True
 
  -- Terminate the program if the window is closed
  GLFW.windowCloseCallback $=
    (writeIORef quit True >> return True)

  let margin   = 10.0
      padding  = 20.0
      (wx,wy)  = windowSize initState
      wsz      = fromIntegral $ min wx wy
      pxs      = wsz - margin * 2
      sz       = fromIntegral . size . table $ initState
      stonePos = let n = (margin + padding) in
          tmap (floor . (\x -> (((fromIntegral x - n)/(wsz-2*n))*sz)))
      (i,j)    = (padding,pxs-padding)
      trans m (x,y) = (x + m, y + m)
      trans'   = trans margin
      trans''  = trans (margin+i)
 
  let render = do
        s <- readIORef state

        let sq (m,n) = [(m,m),(m,n),(n,n),(n,m)]

            tbl      = vertices . map trans' . sq $ (0.0,pxs)
            border   = let [a,b,c,d] = map trans' . sq $ (i,j) in 
              vertices [a,b,b,c,c,d,d,a]

            st    = (j-i) / sz -- step
            lines = vertices . map trans'' . concat $ [
                [(n,0),(n,j-i),(0,n),(j-i,n)] |
                  n <- [st, 2*st .. (sz-1) * st] ]

            color3 a b c = GL.color (GL.Color3 a b c :: GL.Color3 GL.GLfloat)

        color3 0.2 0.5 0.2
        GL.lineWidth $= 1.0
        GL.renderPrimitive GL.Polygon tbl

        color3 0.0 0.0 0.0
        GL.lineWidth $= 5.0
        GL.renderPrimitive GL.Lines border

        color3 0.0 0.0 0.0
        GL.lineWidth $= 1.0
        GL.renderPrimitive GL.Lines lines
          -- toGLfloats lines

        let vcircle x = vertices . (flip circle) x
        color3 0.0 0.0 0.0
        GL.lineWidth $= 1.0
        mapM_ (GL.renderPrimitive GL.Polygon . vcircle 4) $
          map trans'' . sq $ (st*2, st*(sz-2))

        -- White
        let asFloats (x,y) = ((fromIntegral x),(fromIntegral y))
            renderStone = GL.renderPrimitive GL.Polygon . vcircle (st*0.35)
            stonesOf stoneColor = map (asFloats . snd) . filter ((== stoneColor).fst) .
              stones . table $ s
            renderStones = mapM_ (renderStone . trans'' . tmap ((+ st/2) . (* st))) . stonesOf

        color3 1.0 1.0 1.0
        GL.lineWidth $= 1.0
        renderStones White

        -- Black
        color3 0.0 0.0 0.0
        GL.lineWidth $= 1.0
        renderStones Black

  let loop = do
        GLFW.waitEvents

        -- redraw screen if dirty
        d <- readIORef dirty
        when d $ do 
          -- l <- readIORef lines
          GL.clear [GL.ColorBuffer]
          modifyIORef state (looper 0.0) 
          render
          GLFW.swapBuffers

        writeIORef dirty False
        -- check if we need to quit the loop
        q <- readIORef quit
        unless q loop

  let waitForPress = do
        GLFW.mousePosCallback    $= \_   -> return ()
        GLFW.mouseButtonCallback $= \b s -> do
          when (b == GLFW.ButtonLeft && s == GLFW.Press) $ do
            -- when left mouse button is pressed, add the point
            -- to lines and switch to waitForRelease action.
            GL.get GLFW.mousePos >>= writeIORef posM
            waitForRelease 

      waitForRelease = do
        GLFW.mousePosCallback $= \(GL.Position x y) -> do
          -- mark screen dirty
          writeIORef dirty True

        GLFW.mouseButtonCallback $= \b s ->
          -- when left mouse button is released, switch back to
          -- waitForPress action.
          when (b == GLFW.ButtonLeft && s == GLFW.Release) $ do
            (GL.Position px py) <- readIORef posM
            (GL.Position cx cy) <- GL.get GLFW.mousePos
            s <- readIORef state

            let cs = stonePos (cx,cy)
                ps = stonePos (px,py)
                ms = movables s

            when ((cs == ps && inRange cs (size . table $ s) && isPlayer s) &&
                  ((not . null $ ms) && elem cs ms)) $
                writeIORef state $ s {
                    step = 0
                  , skipped  = False
                  , finished = False
                  , players  = nextPlayers (players s)
                  , table    = makeMove s cs
                }

            waitForPress

  -- by default start with waitForPress
  waitForPress
  loop

  -- finish up
  GLFW.closeWindow 
  GLFW.terminate
  -- let stones = stones . table $ state
    
circle :: Point -> Float -> Polygon
circle (x,y) r = map (\t -> (x+r*cos t, y+r*sin t)) [0,0.2..(2*pi)]

type Point   = (Float, Float)
type Polygon = [Point]

 
{--
color3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
color3 = GL.Color3
--}

vertices :: [Point] -> IO ()
vertices = mapM_ (\(x,y) -> GL.vertex $ GL.Vertex3 x y (-1) )


