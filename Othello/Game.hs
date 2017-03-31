module Othello.Game where

import Othello.Core
import Othello.Solvers

type Looper = Float  -> GameState -> GameState
type Play   = Looper -> GameState -> IO ()

looper :: Looper
looper _ state
  | finished state         = state
  | step state < fps state = nextStep
  | null (movables state)  = skipState
  | otherwise              = nextState
    where 
      nextStep  = state    {step = step state + 1}
      baseState = nextStep {
            step    = 0
          , players = nextPlayers (players state)
        }
 
      skipState   = baseState {skipped = True, finished = skipped state}
      nextAiState ai = case ai state of
          Nothing       -> skipState
          Just stonePos -> baseState {
                skipped  = False
              , finished = False
              , table    = makeMove state stonePos
            }
 
      nextState = case (head . players $ state) of
        Man _ _    -> state {step = 0}
        AI  _ _ ai -> nextAiState ai

getPlayers :: String -> [Player]
getPlayers "cpu" = [ (AI "CPU1"    Black randomAI), (AI  "CPU2"   White randomAI) ]
getPlayers "rev" = [ (AI "CPU"     Black randomAI), (Man "Player" White) ]
getPlayers _     = [ (Man "Player" Black),          (AI  "CPU"    White randomAI) ]

