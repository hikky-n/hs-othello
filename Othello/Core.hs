module Othello.Core where

import Control.Arrow
import Data.List

data StoneColor = White | Black | Empty
  deriving (Eq,Show)
 
reversed White = Black
reversed Black = White
reversed Empty = Empty
 
tmap f = f *** f 

type Position = (Int,Int)
type Stone = (StoneColor,Position)

data ReversiTable = ReversiTable {
      size   :: Int
    , stones :: [Stone]
  }
 
type Solver = (GameState -> Maybe Position)

data Player =
    Man String StoneColor 
  | AI  String StoneColor Solver
 
data GameState = GameState {
  -- | frames/sec
    fps        :: Int

  -- | random seeds
  , seeds      :: [Int]

  -- | current steps (of frames)
  , step       :: Int

  -- | outer window size
  , windowSize :: (Int,Int)

  -- | represents whether the former player skipped.
  , skipped    :: Bool

  -- | represents whether the game is over.
  , finished   :: Bool

  -- | whole table state.
  , table      :: ReversiTable

  -- | players.
  , players    :: [Player]

  -- | play history.
  , history    :: [(Player,Stone)]
}
 
isPlayer :: GameState -> Bool
isPlayer state = case head (players state) of
  Man _ _   -> True
  AI  _ _ _ -> False
 
nextPlayers players = take (length players) (tail . cycle $ players)
 
stoneColor :: GameState -> StoneColor
stoneColor = colorOf . head . players
colorOf (AI  _ color _) = color
colorOf (Man _ color)   = color

makeMove :: GameState -> Position -> ReversiTable
makeMove state pos = put nextTable (color,pos)
  where
    (tbl,color) = (table state, stoneColor state)
    nextTable   = foldl doReverse tbl posList
    posList     = map snd $ allReversibleStoneList tbl color pos
 
get :: ReversiTable -> Position -> Stone
get table pos = case found of
  Just os -> os
  Nothing -> (Empty,pos)
  where
    found = find ((== pos) . snd) (stones table)
 
doReverse :: ReversiTable -> Position -> ReversiTable
doReverse table pos = case get table pos of
  (Empty,_)    -> table
  (stone,dpos) -> put table (reversed stone,dpos)
 
put :: ReversiTable -> Stone -> ReversiTable
put table (color,pos) = table { stones = ((color,pos):rest) }
  where
    rest = case get table pos of
      (Empty,_)    -> stones table
      (color,dpos) -> filter ((/= dpos) . snd) (stones table)

inRange (x,y) sz
  | x <  0 || sz <= x = False
  | y <  0 || sz <= y = False
  | otherwise         = True
 
reversibleStoneListByLine :: ReversiTable -> StoneColor -> Position -> Position -> [Stone]
reversibleStoneListByLine table color pos direction@(nx,ny)
  | null rest || (fst (head rest) /= color) = []
  | otherwise  = revs
  where
    target = map (get table) $ targetPosList pos
    (revs,rest) = span ((== reversed color) . fst) target
    targetPosList (px,py)
      | not $ inRange (px,py) (size table) = []
      | otherwise = let npos = (px + nx, py + ny) in (npos: targetPosList npos)
 
allReversibleStoneList :: ReversiTable -> StoneColor -> Position -> [Stone]
allReversibleStoneList table color pos =
  let dirs = [(-1,-1),(0,-1),(1,-1),(-1,0),(1,0),(-1,1),(0,1),(1,1)] in
    concatMap (reversibleStoneListByLine table color pos) $ dirs
 
getEmptyList :: ReversiTable -> [Position]
getEmptyList table = [
    (x,y) | let sz = [0 .. (size table - 1)], x <- sz, y <- sz,
            not $ elem (x,y) (map snd $ stones table) ]


movables :: GameState -> [Position]
movables state = filter (movable tbl color) (getEmptyList tbl)
  where
    color = stoneColor state
    tbl   = table state
 
movable :: ReversiTable -> StoneColor -> Position -> Bool
movable tbl color pos = not . null $ allReversibleStoneList tbl color pos

