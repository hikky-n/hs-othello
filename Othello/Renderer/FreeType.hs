module Othello.Renderer.FreeType where

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.FreeType.Internal as F
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FP
import Graphics.Rendering.FreeType.Internal.Library as FL
import Graphics.Rendering.FreeType.Internal.Glyph as FG
import Graphics.Rendering.FreeType.Internal.Face as FC
import Foreign.C as C
import Foreign.Marshal.Alloc(alloca)

import Data.List.Split(chunksOf)
import Data.List(intercalate,replicate)

runFT :: IO a -> IO ()
runFT monad = do
  ret <- monad
  when (ret /= 0) $
    error $ "FreeType Error (code = " ++ show ret ++ ")"

initFT :: IO FL.FT_Library
initFT = alloca $ \pLibFT -> do
  runFT $ F.ft_Init_FreeType pLibFT
  peek pLibFT

loadFont :: FL.FT_Library -> FilePath -> IO FC.FT_Face
loadFont libFT fontPath =
  C.withCString fontPath $ \cFontPath ->
    alloca $ \pFontFace -> do
      runFT $ F.ft_New_Face libFT cFontPath 0 pFontFace
      peek pFontFace

paddingHorizontal :: Int -> Int -> a -> [a] -> [a]
paddingHorizontal padding element =
  intercalate (replicate padding element) . chunksOf
  
{-
loadChar :: FC.FT_Face -> Char -> Int -> FG.FT_Glyph
loadChar fontFace char size = 
-}

{-

-- | Create 'Font' from a given path.
readFont :: FilePath -> IO Font
readFont path = alloca $ \p -> do
	runFT $ withCString path $ \str -> ft_New_Face ftlib str 0 p
	face <- peek p
	b <- peek (bbox face)
	asc <- fromIntegral <$> peek (ascender face)
	desc <- fromIntegral <$> peek (descender face)
	u <- fromIntegral <$> peek (units_per_EM face)
	let box = pure ((/u).fromIntegral) <*> Box
		(V2 (xMin b) (yMin b))
		(V2 (xMax b) (yMax b))
	return $ Font face (asc/u) (desc/u) box

-- | Single line text rendering
textLine :: Font -> Float -> String -> IO (ForeignPtr Word8, V2 Int, Box V2 Int)
textLine Font{fontFace = face} size text = do
	let dpi = 72
	runFT $ ft_Set_Char_Size face 0 (floor $ size * 64) dpi dpi
	slot <- peek $ glyph face
	
	let text' = map fromEnum text
	let measure :: Ptr V.FT_Vector -> Ptr FT_BBox -> (Int, Ptr FT_Glyph, Int, [Box V2 Int]) -> Int -> IO (Int, Ptr FT_Glyph, Int, [Box V2 Int])
	    measure delta glyphBBox (prev, glyphs, penX, xs) ch = do
		glyphIx <- ft_Get_Char_Index face (fromIntegral ch)
		
		ft_Get_Kerning face (fromIntegral prev) glyphIx (fromIntegral ft_KERNING_DEFAULT) delta
		kx <- fromIntegral . V.x <$> peek delta
		
		runFT $ ft_Load_Glyph face glyphIx ft_LOAD_DEFAULT
		runFT $ ft_Get_Glyph slot glyphs
		
		dx <- fromIntegral . V.x <$> peek (GS.advance slot)
		glyph' <- peek glyphs
		ft_Glyph_Get_CBox glyph' ft_GLYPH_BBOX_UNSCALED glyphBBox
		bbox@FT_BBox{..} <- peek glyphBBox
		--putStrLn $ show bbox
		let (left, y) = (penX + kx, 0)
		let f = fromIntegral
		let box = Box (V2 (left + f xMin) (y + f yMin)) (V2 (left + f xMax) (y + f yMax))
		--putStrLn $ show box
		return (ch, advancePtr glyphs 1, penX+kx+dx, box:xs)
	
	let render :: Ptr FT_Glyph -> Ptr Word8 -> Int -> Box V2 Int -> Box V2 Int -> Int -> IO ()
	    render glyphs dst bmpW (Box bmin bmax) (Box gmin gmax) i = do
		let imgPtr = advancePtr glyphs i
		--poke pen $ V.FT_Vector (fromIntegral x + fromIntegral loff) (fromIntegral toff)
		--runFT $ ft_Glyph_To_Bitmap imgPtr ft_RENDER_MODE_NORMAL pen 1
		-- destroy old glyph
		runFT $ ft_Glyph_To_Bitmap imgPtr ft_RENDER_MODE_NORMAL nullPtr 1
		img <- peek imgPtr
		let im = BG.cast img
		--bl <- fromIntegral <$> peek (left im)
		--bt <- fromIntegral <$> peek (top im)
		bmp <- peek $ bitmap im
		--let bw = fromIntegral $ width bmp
		--let bh = fromIntegral $ rows bmp
		let image = buffer bmp
		
		let V2 (V2 xMin yMin) (V2 gxMin gyMin) = fmap (`shiftR` 6) <$> V2 bmin gmin
		let V2 (V2 xMax yMax) (V2 gxMax gyMax) = fmap ((`shiftR` 6) . (+63)) <$> V2 bmax gmax
		let V4 gl gt gw gh = V4 (-xMin+gxMin) (yMax-gyMax) (gxMax-gxMin) (gyMax-gyMin)
		--putStrLn $ show ("left", bl, "top", bt, "gl", gl, "gt", gt)
		--putStrLn $ show ("cols", bw, "rows", bh, "gw", gw, "gh", gh)
		
		forM_ [0..gh-1] $ \y -> do
			--putStrLn $ show ((y + gt) * bmpW + gl, (y * gw), gw)
			copyBytes (plusPtr dst $ (y + gt) * bmpW + gl) (plusPtr image (y * gw)) gw
		ft_Done_Glyph img
	
	alloca $ \ftVec -> do
		alloca $ \glyphBBox -> do
			allocaArray (length text) $ \glyphs -> do
				(_, _, textW, heads) <- foldM (measure ftVec glyphBBox) (0, glyphs, 0, []) text'
				
				let minimumBox = Box maxBound (V2 textW minBound)
				let bitmapBox@(Box (V2 l b) (V2 r t)) = foldl union minimumBox heads
				--putStrLn $ "BitBox: " ++ show (fmap (`div`64) bitmapBox)
				
				let bmpDim@(V2 w h) = (`shiftR` 6) <$> V2 (r - l + 63) (t - b + 63)
				--putStrLn $ unwords ["all:",show(w * h),"W",show w,"H",show h,show heads]
				
				let alignedW = ((w + 3) `div` 4) * 4
				fp <- mallocForeignPtrBytes (alignedW * h)
				withForeignPtr fp $ \image -> do
					memset image 0 (fromIntegral $ alignedW * h)
					zipWithM (render glyphs image alignedW bitmapBox) (reverse heads) [0..]
				return (fp, bmpDim, bitmapBox)

foreign import ccall unsafe "FT_Glyph_Get_CBox"
	ft_Glyph_Get_CBox :: FT_Glyph -> FT_UInt -> Ptr B.FT_BBox -> IO ()

--ft_GLYPH_BBOX_PIXELS = 3
ft_GLYPH_BBOX_UNSCALED = 0

-}

renderText :: FilePath -> Float -> GL.Vector3 GLfloat -> IO ()
renderText font size pos = do
  return ()

