module Main where

import UI (runUI)

-- Simple main that launches the Gloss UI
main :: IO ()
main = do
  putStrLn "=== MazeBreaker: Interactive Maze Solver ==="
  putStrLn "Starting GUI..."
  putStrLn ""
  putStrLn "Controls:"
  putStrLn "  - Click 'New Maze' to generate a new random maze"
  putStrLn "  - Click 'Solve' to watch the pathfinding animation"
  putStrLn ""
  runUI