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
    , windowSize = (640,480)
    , skipped    = False
    , finished   = False
    , table      = initialTable 10
    , players    = []
    , history    = []
  }
  where
    initialTable sz = ReversiTable {
        size   = sz
      , stones = [(White,(m,m)),(White,(n,n)),(Black,(n,m)),(Black,(m,n))]
      }
        where (m,n) = (sz `div` 2 - 1, sz `div` 2)

main = do
  argv <- getArgs
  let proc = if null argv then "man" else head argv
  time    <- getPOSIXTime  
  initialState <- return $ baseState {
        seeds   = randomList time
      , players = getPlayers proc
    }
 
  GLFW.play looper (initialState {fps = 0})
 
