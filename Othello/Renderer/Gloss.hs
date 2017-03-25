module Othello.Renderer.Gloss where

import Othello.Core

import Data.List(maximumBy)
import Data.Function(on)

import Graphics.Gloss as G
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

backgroundColor = makeColor 0.0 0.6 0.0 1.0
play looper initialState =
  G.play (InWindow "Reversi" (fwsize initialState) (20,20))
    backgroundColor (fps initialState) initialState drawTable eventHandler looper
 
fwsize          = tmap fromIntegral . windowSize
 
eventHandler :: Event -> GameState -> GameState
eventHandler (EventKey (MouseButton LeftButton) Down _ (x,y)) state
  | not (isPlayer state)  = state
  | finished state        = state
  | null (movables state) = state
  | not (elem stonePos (movables state)) = state
  | otherwise = nextState
    where
      (w,h)    = tmap fromIntegral $ windowSize state
      (nx,ny)  = tmap fromIntegral $ size . table $ state
      stonePos = tmap floor ( (0.5 + (x/w))*nx,(0.5+ (y/h))*ny)
      nextState = state {
        step = 0
        , skipped  = False
        , finished = False
        , players  = nextPlayers (players state)
        , table    = makeMove state stonePos
      }
 
eventHandler _ state = state
 
tablePict :: (Int,Int) -> (Int,Int) -> Picture
tablePict (w,h) (sx,sy) = Pictures . map (Color black . Line . flr) $ lines
  where
    flr = map $ tmap fromIntegral
    axesX = [ ((0, y):(sx, y):[]) | t <- [ 1 .. (w-1)], let y = (t * sy) `div` h ]
    axesY = [ ((x, 0):(x, sy):[]) | t <- [ 1 .. (h-1)], let x = (t * sx) `div` w ]
    lines = concat (axesX:axesY:[])
 
stonePict :: (Int,Int) -> (Int,Int) -> Stone -> Picture
stonePict (w,h) (sx,sy) (color,(x,y)) = 
  Translate tx ty circle
  where
    ((fx,fy):(fsx,fsy):(fw,fh):[]) = map (tmap fromIntegral) ((x,y):(sx,sy):(w,h):[])
 
    (ux,uy) = (fsx/fw, fsy/fh)
    (tx,ty) = (ux * (fx + 0.5), uy * (fy + 0.5))
    (cw,ct) = ((ux + uy) / 16, (ux + uy) / 8)
    circle = circleColor $ ThickCircle cw ct
    circleColor = case color of
      White -> Color white
      Black -> Color black
 
drawTable :: GameState -> Picture
drawTable state =
  -- (step,sp@(sx,sy),_,tbl@(tableSize,ls)) =
  if finished state then Pictures (playScreen:getWinnerText state:[]) else playScreen
  where
    playScreen = Translate tx ty $ Pictures (frameLine:stonePics)
    sp         = windowSize state
    (tx,ty)    = tmap ((* (-1)) . (/ 2). fromIntegral ) sp
    frameLine  = tablePict (size . table $ state) sp
    stonePics  = map (stonePict (size . table $ state) sp) (stones . table $ state)
 
getWinnerText :: GameState -> Picture
getWinnerText state = Color violet . Scale 0.5 0.5 . Text $ winnerText
  where
    count table pl = length $ filter ((== colorOf pl) . fst) table
    counter x      = (count (stones . table $ state) x, x)
    countList      = map counter $ players state
 
    winner         = maximumBy (compare `on` fst) countList
    winnerText     = nameOf (snd winner)  ++ " Win"
    nameOf pl = case pl of
      AI  name _ _ -> name
      Man name _   -> name
 
 

