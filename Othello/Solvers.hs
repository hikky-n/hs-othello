
module Othello.Solvers where

import Othello.Core

randomSelect :: Int -> [a] -> a
randomSelect seed ls = head . drop (seed `mod` length ls) $ ls

randomAI :: Solver
randomAI state
  | null $ movables state = Nothing
  | otherwise             = Just . randomSelect (head $ seeds state) $ movables state
 
