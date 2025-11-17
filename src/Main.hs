module Main where

import UI (runUI)

-- Simple main that launches the Gloss UI
main :: IO ()
main = do
  putStrLn "=== MazeBreaker: Special Walls Edition ==="
  putStrLn "Starting GUI..."
  putStrLn ""
  putStrLn "Features:"
  putStrLn "  - Brown walls (B) = Breakable (3 breaks max)"
  putStrLn "  - Orange walls (J) = Jumpable (2 jumps max)"
  putStrLn ""
  putStrLn "Controls:"
  putStrLn "  - Click 'New Maze' to generate a new random maze"
  putStrLn "  - Click 'Solve' to watch the A* pathfinding animation"
  putStrLn ""
  runUI