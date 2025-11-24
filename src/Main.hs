module Main where

import UI (runUI)

main :: IO ()
main = do
  putStrLn "=== MazeBreaker: Gate System Edition ==="
  putStrLn "Starting GUI..."
  putStrLn ""
  putStrLn "Features:"
  putStrLn "  - Collect KEY (K) to unlock gates"
  putStrLn "  - Purple GATES (G) block until key collected"
  putStrLn "  - Brown walls (B) = Breakable (3 max)"
  putStrLn "  - Orange walls (J) = Jumpable (2 max)"
  putStrLn "  - Special walls only in strategic interior positions"
  putStrLn ""
  putStrLn "Controls:"
  putStrLn "  - Click 'New Maze' to generate a new random maze"
  putStrLn "  - Click 'Solve' to watch the A* pathfinding animation"
  putStrLn ""
  runUI