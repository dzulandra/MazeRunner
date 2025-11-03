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
  let maze' = foldl (\m p -> setTile m p Empty) maze path
      rendered = [ [ charAt (r, c)
                   | c <- [0 .. mazeWidth maze - 1] ]
                 | r <- [0 .. mazeHeight maze - 1] ]
      charAt pos
        | pos == fromMaybe (-1, -1) (findTile Start maze) = 'S'
        | pos == fromMaybe (-1, -1) (findTile Goal maze)  = 'G'
        | pos `elem` path = '*'
        | otherwise = tileToChar (fromMaybe Wall (getTile maze pos))
  mapM_ putStrLn rendered

main :: IO ()
main = do
  putStrLn "=== MazeBreaker: Functional Maze Solver ==="
  gen <- getStdGen
  let width  = 10
      height = 10
      maze = generateMaze height width gen

  putStrLn "\nGenerated Maze:\n"
  printMaze maze

  putStrLn "\nSolving...\n"
  case solveMaze maze of
    Nothing -> putStrLn "[X] No path found."
    Just path -> do
      putStrLn "[OK] Path found!"
      print path
      putStrLn "\nMaze with solution:\n"
      printMazeWithPath maze path
