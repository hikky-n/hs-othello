{-# LANGUAGE OverloadedStrings, PackageImports #-}
module Othello.Renderer.GLFW(play) where

-- import Graphics.UI.GLFW as GLFW
-- import qualified "GLFW-b" Graphics.UI.GLFW as GLFWb
-- import Graphics.GL as RawGL
import qualified "GLFW" Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Codec.Picture.Repa as Repa
import           Data.Array.Repa (Z(Z), (:.) (..))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Repr.ForeignPtr as RepaF
import Foreign.Marshal.Alloc(alloca)
import Foreign.ForeignPtr(withForeignPtr)
import Graphics.Rendering.OpenGL (($=))
import Control.Monad (forM_, mapM_, replicateM_, when, liftM, unless)
import Data.Maybe(Maybe(..),maybe)
import Data.IORef(newIORef, readIORef, atomicModifyIORef, atomicWriteIORef)
import Unsafe.Coerce(unsafeCoerce)
import Data.List(intercalate)

import Othello.Core
import Othello.Game
import qualified Debug.Trace as Debug
import GHC.Float(float2Double,double2Float,cosDouble,sinDouble,tanDouble)

loadFont fontPath fontSize renderResolution =  do
  font <- FTGL.createPolygonFont fontPath
  FTGL.setFontFaceSize font fontSize renderResolution
  return font

{-# INLINE gsizei #-}
gsizei :: Int -> GL.GLsizei
gsizei = unsafeCoerce

loadTextures :: [FilePath] -> IO [GL.TextureObject]
loadTextures = mapM loadTexture 

loadTexture :: FilePath -> IO GL.TextureObject
loadTexture filePath = do
  image <- either error id <$> Repa.readImageRGBA filePath
  let content = Repa.delay . Repa.imgData $ image
      (Z :. width :. height :. _) = Repa.extent content
      size = GL.TextureSize2D (gsizei width) (gsizei height)
      pix  = GL.PixelData GL.RGBA GL.UnsignedInt8888

  [texture] <- GL.genObjectNames 1
  GL.textureBinding GL.Texture3D $= Just texture

  fptr <- liftM RepaF.toForeignPtr $ Repa.computeP $ content

  withForeignPtr fptr $
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 size 0 . pix

  return texture

play :: Play
play looper initState = do
  -- main resolution
  let (xres,yres) = windowSize initState
      (near,far)  = (-2000,2000) :: (Float,Float)
      glfwWinSize = GL.Size (fromIntegral xres) (fromIntegral yres)

  font <- loadFont "MTLmr3m.ttf" (xres `div` 64) xres

  let textOut str = FTGL.renderFont font str (FTGL.All)

  GLFW.initialize

  -- enable antialiasing (especially for fonts)
  GLFW.openWindowHint GLFW.FSAASamples 4

  let params = [
          GLFW.DisplayAlphaBits 8
        , GLFW.DisplayDepthBits 16
        ]
    in GLFW.openWindow glfwWinSize params GLFW.Window
  GLFW.windowTitle $= "Hothello"

  -- multisample is enabled as opengl default.
  -- GL.multisample $= GL.Enabled

  -- disable auto polling in swapBuffers
  GLFW.disableSpecial GLFW.AutoPollEvent

  GL.lineSmooth $= GL.Enabled
  -- GL.blend      $= GL.Enabled
  -- GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.cullFace   $= Just GL.Back
  GL.frontFace  $= GL.CCW

  -- l <- readIORef lines
  GL.depthFunc  $= Just GL.Lequal
  -- GL.depthMask  $= GL.Enabled
  GL.texture GL.Texture2D $= GL.Enabled

  -- state vars
  gameState         <- newIORef initState
  dirty             <- newIORef True
  quit              <- newIORef False

  posStoneMouseOver <- newIORef ( Nothing :: Maybe (Int,Int) )
  posStoneClicked   <- newIORef ( Nothing :: Maybe (Int,Int) )
  posRotate         <- newIORef (0,0)
  posClicked        <- newIORef (0,0)
  sizeWindow        <- newIORef (0,0)
  count             <- newIORef 0

  -- utilities
  let square :: Float -> [(Float,Float,Float)]
      square m   = [(-m,-m,0),(m,-m,0),(m,m,0),(-m,m,0)]

      boardWidth, inner, sz, st :: Float
      boardWidth = fromIntegral (min yres xres)
      inner      = (boardWidth/2) - (0.03 * boardWidth)
      sz         = fromIntegral . size . table $ initState
      st         = 2 * inner / sz

      withZ   z (x,y,_) = (x,y,z)
      -- uncurry3 f (a,b,c) = f a b c
      color3     (a,b,c)   = GL.color     (GL.Color3  a b c   :: GL.Color3  GL.GLfloat)
      color4     (a,b,c,d) = GL.color     (GL.Color4  a b c d :: GL.Color4  GL.GLfloat)
      vertex3    (a,b,c)   = GL.vertex    (GL.Vertex3 a b c   :: GL.Vertex3 GL.GLfloat)
      normal3    (a,b,c)   = GL.normal    (GL.Normal3 a b c   :: GL.Normal3 GL.GLfloat)
      translate3 (a,b,c)   = GL.translate (GL.Vector3 a b c   :: GL.Vector3 GL.GLfloat)
      rotate3 d  (a,b,c)   = GL.rotate d  (GL.Vector3 a b c   :: GL.Vector3 GL.GLfloat)

      circleVertices r = [ (r*cos t,r*sin t,0) | t <- [0,0.2..0.2 + (2*pi)] ]

      -- rotated xy plane normal
      rad n      = pi * (fromIntegral n / 180)
      rotate r rv p = (
          line (t*x*x + cos r,     t*x*y - sin r * z, t*x*z + sin r * y)
        , line (t*x*y + sin r * z, t*y*y + cos r,     t*y*z - sin r * x)
        , line (t*x*z - sin r * y, t*y*z + sin r * x, t*z*z + cos r    )
        )
        where line    = (* lengthVec p) . dotProduct (unitVec p)
              (x,y,z) = unitVec rv
              t       = 1 - cos r

      dotProduct (x,y,z) (x',y',z') = (x*x') + (y*y') + (z*z')
      lengthVec  a = sqrt $ dotProduct a a
      unitVec    v@(x,y,z) = (x / lengthVec v, y / lengthVec v, z / lengthVec v)

      normalizePoint2d (rx,ry) (mx,my)
        | inPlane            = Nothing
        | paraPlane (0,0,-1) = Nothing
        | otherwise          = Just (mx', my')
          where (nx',ny',nz') = rotate ( rad rx) (1,0,0) . rotate ( rad ry) (0,1,0) $ (0,0,-1)
                (mx',my', _)  = rotate (-rad ry) (0,1,0) . rotate (-rad rx) (1,0,0) $ mv
                mv = (mx,my, (-(mx*nx' + my*ny') / nz') )
                paraPlane v   = abs (dotProduct (nx',ny',nz') v) < 0.01 -- epsiron for error -- == 0
                inPlane       = all paraPlane [(mx,my,0),(mx,my,1)]

      normWinPos sz mPos =
        let (rw,rh) = tmap fromIntegral sz
            (rx,ry) = tmap fromIntegral (xres,yres)
            scale_  = max (rx/rw) (ry/rh)
            (mx,my) = tmap fromIntegral mPos

        in tmap (* scale_) (mx-(rw/2),(rh/2)-my) -- normalize

      stonePos (x,y) | abs x > inner = Nothing
                     | abs y > inner = Nothing
                     | otherwise     = Just . tmap (floor . (/ st) . (+ inner)) $ (x,-y)

      pointToStonePos sizeWindow_ rt mpos = normalizePoint2d rt normPos >>= stonePos
          where normPos = normWinPos sizeWindow_ mpos


  -- onResize (and onOpen)
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) -> do
    -- compute target scale for viewport to fit direction and size appropriately.
    let (rw,rh) = tmap fromIntegral (w,h)       :: (Float,Float)
        (rx,ry) = tmap fromIntegral (xres,yres)
        ar      = (ry*rw)/(rx*rh)
        (sx,sy) = if ar > 1 then (ar,1) else (1,1/ar)
        (fw,fh) = tmap realToFrac ((sx*rx/2),(sy*ry/2))
        (zn,zf) = tmap realToFrac (near,far) -- (/ scale_)) (near,far)

    atomicWriteIORef sizeWindow (w,h)

    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.clearColor $= GL.Color4 0 0 0 0
    -- GL.cullFace   $= Just GL.Back
    -- GL.frontFace  $= GL.CCW

    -- l <- readIORef lines
    GL.depthFunc  $= Just GL.Lequal
    -- GL.depthMask  $= GL.Enabled
    GL.texture GL.Texture2D $= GL.Enabled

    GL.viewport   $= (GL.Position 0 0, size)
    GL.matrixMode $=  GL.Projection
    GL.loadIdentity
    GL.ortho (-fw) fw (-fh) fh zn zf -- centering
    -- GL.frustum (-fw) fw (-fh) fh zn zf -- zn zf -- centering
    -- GL.perspective 150 1 10 10000
    -- frustum (-fw) fw (-fh) fh zn zf -- zn zf -- centering


    putStrLn $ show [fw,fh,zn,zf]

    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

  GLFW.windowRefreshCallback $= atomicWriteIORef dirty True
  GLFW.windowCloseCallback   $= do
    atomicWriteIORef quit True
    return True

  -- use key callback to track whether ESC or Q is pressed
  GLFW.keyCallback $= \key st -> do
    when (key == GLFW.SpecialKey GLFW.ESC && st == GLFW.Press) $ do
      atomicWriteIORef quit True

    when (key == GLFW.CharKey 'Q' && st == GLFW.Press) $ do
      atomicWriteIORef quit True

    when (key == GLFW.CharKey 'R' && st == GLFW.Press) $ do
      atomicWriteIORef posRotate (0,0)
      atomicWriteIORef dirty True

  -- renderer utils
  let renderStone :: Float -> Float -> [(Float,Float,Float,Float)] -> (Int,Int) -> IO ()
      renderStone radius thickness colors (sx,sy) =
        let convertPos x = (fromIntegral x + 0.5) * st
            vsBase = circleVertices radius
            vsTop  = map (withZ ( thickness/2)) vsBase
            sqvs (x1:[])    (y1:[])    = []
            sqvs (x1:x2:xs) (y1:y2:ys) = (x1:y1:y2:x2: sqvs (x2:xs) (y2:ys))

        in GL.preservingMatrix $ do
          translate3   $ (convertPos sx - inner, inner - convertPos sy, 0)
          forM_ colors $ \faceColor -> do
            color4 faceColor
            GL.renderPrimitive GL.Polygon . mapM_ vertex3 $ vsTop
            GL.renderPrimitive GL.Quads   . mapM_ vertex3 $ sqvs vsTop vsBase
            rotate3 180 (1,1,0)

  -- renderers
  let withTexture texType ioAction = do
        GL.texture texType $= GL.Enabled
        ioAction
        GL.texture texType $= GL.Disabled

  let setupLights = do
        GL.position (GL.Light 0) $= GL.Vertex4 0 0 0 1
        GL.position (GL.Light 1) $= GL.Vertex4 0 0 0 1
        GL.diffuse  (GL.Light 1) $= GL.Color4  1 1 1 1
        GL.specular (GL.Light 1) $= GL.Color4  1 1 1 1

  let renderBackground = GL.preservingMatrix $ do
        color3 (0.8,0.5,0.3)
        GL.lineWidth $= 1.0
        GL.renderPrimitive GL.Quads $ do
          mapM_ vertex3 $ square (boardWidth)

  let renderBoard = GL.preservingMatrix $ do
        color3 (0.2,0.5,0.2)
        GL.lineWidth $= 1.0
        GL.renderPrimitive GL.Quads $ do
          let vBoard = square (boardWidth/2)
          mapM_ vertex3 $ vBoard
          mapM_ (vertex3 . withZ (-1)) . reverse $ vBoard
          -- mapM_ normal3 $ replicate 3 (0,0,1)

  let renderStones s_ = GL.preservingMatrix $ do
        let thickness = (st*0.1)
            radius    = (0.35*st) 
        translate3 (0,0,thickness/2)

        GL.preservingMatrix . forM_ (stones . table $ s_) $ \(stoneColor, pos) ->
          let colorCodes x = (if x == White then id else reverse) [(1,1,1,1),(0,0,0,1)]
          in renderStone radius thickness (colorCodes stoneColor) pos

  let renderDebugPoints = GL.preservingMatrix $ do
        sizeWindow_ <- readIORef sizeWindow
        (rtx,rty)   <- readIORef posRotate

        GL.Position pcx pcy <- GL.get GLFW.mousePos
        let (mpx,mpy) = normWinPos       sizeWindow_ (pcx,pcy)
            np        = normalizePoint2d (rty,rtx)   (mpx,mpy)

        case np of
          Just (npx,npy) -> do
                color3 (1,0,0)
                translate3 (npx,npy,10)
                GL.renderPrimitive GL.Polygon .
                  mapM_ vertex3 $ circleVertices (st*0.1)
                translate3 (st*0.2,0,0)
                textOut $ show (mpx,mpy)
          Nothing -> return ()

  let renderDebugTexts gameState_ = GL.preservingMatrix $ do
        count_     <- readIORef count
        ps         <- readIORef posStoneMouseOver

        color3 (1,0,1)
        translate3 (0,0,1)
        textOut $ "Test! count = " ++ show count_
        translate3 (0,-50,0)
        textOut $ "StonePos =" ++ show ps
        translate3 (-100,-20,0)
        forM_ (stones . table $ gameState_) $ \s -> do
          translate3 (0,-12,0)
          textOut $ show s

  let renderOuterBoader = GL.preservingMatrix $ do
        color3 (0,0,0)
        GL.lineWidth $= 5.0
        replicateM_ 4 $ do
          GL.renderPrimitive GL.Lines $ do
            vertex3 (inner, inner,0)
            vertex3 (inner,-inner,0)
          rotate3 90 (0,0,1)

  let renderInnerBoader = GL.preservingMatrix $ do
        color3 (0,0,0)
        GL.lineWidth $= 1.0
        replicateM_ 2 $ do
          let pts = (size . table $ initState) - 1
              ls  = [ (st * fromIntegral x) | x <- [ 1 .. pts ] ]
          GL.renderPrimitive GL.Lines . forM_ ls $ \p -> do
            vertex3 (p-inner,-inner,0)
            vertex3 (p-inner, inner,0)
          rotate3 90 (0,0,1)

  let renderMarkerPoints = GL.preservingMatrix $ do
        let n = st*(sz-4)/2
        color3 (0,0,0)
        replicateM_ 4 $ do
          GL.preservingMatrix $ do
            translate3 (n,n,0)
            GL.renderPrimitive GL.Polygon .
              mapM_ vertex3 $ circleVertices (st*0.075)
          rotate3 90 (0,0,1)


  let render = do
        gameState_  <- readIORef gameState

        GL.clearColor $= GL.Color4 0 0 0 0
        GL.clearDepth $= 1.0
        GL.clear [GL.ColorBuffer,GL.DepthBuffer]

        -- render board
        withTexture GL.Texture3D . GL.preservingMatrix $ do

          setupLights

          renderBackground

          do (rtx,rty) <- readIORef posRotate
             translate3 (0,0,boardWidth*1.5)
             rotate3 (fromIntegral rty) (1,0,0)
             rotate3 (fromIntegral rtx) (0,1,0)

          renderBoard
          translate3 (0,0,1)

          -- lines
          renderOuterBoader
          renderInnerBoader
          renderMarkerPoints

          renderStones gameState_

          renderDebugPoints
          renderDebugTexts gameState_

        atomicModifyIORef count (\x -> (x + 1, ()))

        -- color3 1 1 1
        -- GL.rasterPos (GL.Vertex3 wy 0 -1)
        -- GL.print (show c)
        -- GL.flush


  let loop = do
        GLFW.waitEvents

        dirty_ <- readIORef dirty
        when dirty_ $ do 
          atomicModifyIORef count     (\x -> (x + 1, ()))
          atomicModifyIORef gameState (\x -> (looper 0.0 x, ()))
          -- l <- readIORef lines
          GL.clear [GL.ColorBuffer]
          render
          -- color3 1 1 1
          -- GL.rasterPos (GL.Vertex3 wy 0 -1)
          -- GL.print (show c)
          GLFW.swapBuffers

        atomicWriteIORef dirty False

        -- check if we need to quit the loop
        quit_ <- readIORef quit
        unless quit_ loop


  let waitForPress = do
        GLFW.mousePosCallback $= \x -> do
          atomicWriteIORef dirty True
          return ()

        GLFW.mouseButtonCallback $= \b s -> do
          when (b == GLFW.ButtonLeft && s == GLFW.Press) $ do
            -- when left mouse button is pressed, add the point
            -- to lines and switch to waitForRelease action.
            GL.Position x y <- GL.get GLFW.mousePos 
            (rtx,rty)   <- readIORef posRotate
            sizeWindow_ <- readIORef sizeWindow

            atomicWriteIORef posClicked (x,y)
            atomicWriteIORef posStoneClicked $ pointToStonePos sizeWindow_ (rty,rtx) (x,y)
            atomicWriteIORef dirty True

            waitForRelease 

      waitForRelease = do
        GLFW.mousePosCallback $= \(GL.Position x y) -> do
          -- mark screen dirty
          GL.Position mx my <- GL.get GLFW.mousePos
          sizeWindow_       <- readIORef sizeWindow

          (px,py)           <- readIORef posClicked
          (rx,ry)           <- readIORef posRotate


          let posStone = pointToStonePos sizeWindow_ (ry,rx) (x,y)

          atomicWriteIORef posClicked        (mx,my)
          atomicWriteIORef posStoneMouseOver posStone
          atomicWriteIORef posRotate         (rx+(px-mx),ry+(py-my))
          atomicWriteIORef dirty             True

        GLFW.mouseButtonCallback $= \b s ->
          -- when left mouse button is released, switch back to
          -- waitForPress action.
          when (b == GLFW.ButtonLeft && s == GLFW.Release) $ do
            (GL.Position cx cy) <- GL.get GLFW.mousePos

            gameState_        <- readIORef gameState
            sizeWindow_       <- readIORef sizeWindow
            posStoneClicked_  <- readIORef posStoneClicked
            (rtx,rty)         <- readIORef posRotate

            -- set rotate
            atomicWriteIORef dirty True

            maybe (return ()) (atomicWriteIORef gameState) $ do
              let die x y = x y Nothing
              m <- posStoneClicked_
              c <- pointToStonePos sizeWindow_ (rty,rtx) (cx,cy)
              die unless ( c == m && isPlayer gameState_ && (elem c $ movables gameState_)  )
              return gameState_ {
                  step = 0
                , skipped  = False
                , finished = False
                , players  = nextPlayers (players gameState_)
                , table    = makeMove gameState_ c
              }

            waitForPress

  -- by default start with waitForPress
  waitForPress
  loop

  -- finish up
  FTGL.destroyFont font
  GLFW.closeWindow 
  GLFW.terminate
  -- let stones = stones . table $ state
