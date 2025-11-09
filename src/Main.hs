module Main where

import System.Random (getStdGen)
import Data.Maybe (fromMaybe)
import Core.Maze
import Core.Generator
import Core.Solver

-- Ubah representasi Tile jadi karakter
tileToChar :: Tile -> Char
tileToChar Start = 'S'
tileToChar Goal  = 'G'
tileToChar Wall  = '#'
tileToChar Empty = '.'

-- Cetak maze dalam bentuk grid karakter
printMaze :: Maze -> IO ()
printMaze = mapM_ (putStrLn . map tileToChar)

-- Tampilkan jalur (path) di maze dengan karakter '*'
printMazeWithPath :: Maze -> [Coord] -> IO ()
printMazeWithPath maze path = do
  let startPos = fromMaybe (-1, -1) (findTile Start maze)
      goalPos  = fromMaybe (-1, -1) (findTile Goal maze)
      rendered = [ [ charAt (r, c)
                   | c <- [0 .. mazeWidth maze - 1] ]
                 | r <- [0 .. mazeHeight maze - 1] ]
      charAt pos
        | pos == startPos = 'S'
        | pos == goalPos  = 'G'
        | pos `elem` path = '*'
        | otherwise = tileToChar (fromMaybe Wall (getTile maze pos))
  mapM_ putStrLn rendered

main :: IO ()
main = do
  putStrLn "=== MazeBreaker: Functional Maze Solver ==="
  putStrLn ""
  gen <- getStdGen
  
  -- IMPORTANT: Use odd dimensions for best results
  -- The algorithm works with cells separated by walls
  let width  = 21  -- Changed to odd number
      height = 15  -- Changed to odd number
      maze = generateMaze height width gen

  putStrLn $ "Generating " ++ show height ++ "x" ++ show width ++ " maze...\n"
  printMaze maze

  putStrLn "\nSolving...\n"
  case solveMaze maze of
    Nothing -> putStrLn "[X] No path found."
    Just path -> do
      putStrLn $ "[OK] Path found! Length: " ++ show (length path)
      putStrLn "\nMaze with solution:\n"
      printMazeWithPath maze path