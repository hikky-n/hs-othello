module Main where
 
import Data.Time
import Data.Time.Clock.POSIX
import System.Random.Mersenne.Pure64(pureMT,randomInt)
import System.Environment
import Data.List(unfoldr)
 
import Othello.Core
import Othello.Game(looper, getPlayers)
import qualified Othello.Renderer.Gloss as Gloss 
import qualified Othello.Renderer.GLFW  as GLFW
 
randomList time = unfoldr (Just . randomInt) .  pureMT $ round (time * 1000)

baseState = GameState {
      fps        = 12
    , seeds      = [0]
    , step       = 0
    , windowSize = (1366,768)
    , skipped    = False
    , finished   = False
    , table      = initialTable (10,10)
    , players    = []
    , history    = []
  }
  where
    initialTable (size) = ReversiTable {
        size   = size
      , stones = [(White,(dx,dy)),(White,(dx+1,dy+1)),(Black,(dx+1,dy)),(Black,(dx,dy+1))]
      }
        where (dx,dy) = tmap (\x -> x `div` 2 - 1) size

main = do
  argv <- getArgs
  let proc = if null argv then "man" else head argv
  time    <- getPOSIXTime  
  initialState <- return $ baseState {
        seeds   = randomList time
      , players = getPlayers proc
    }
 
  GLFW.play looper initialState 
 
