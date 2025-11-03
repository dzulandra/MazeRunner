module Core.Generator (generateMaze) where

import System.Random
import Core.Maze

-- Fungsi utama untuk membuat maze acak
generateMaze :: Int -> Int -> StdGen -> Maze
generateMaze h w gen =
  let (tiles, _) = foldl genRow ([], gen) [0..h-1]
  in placeStartGoal tiles
  where
    genRow (rows, g) _ =
      let (row, g') = genCols w g
      in (rows ++ [row], g')

    genCols 0 g = ([], g)
    genCols n g =
      let (r, g') = randomR (0, 4 :: Int) g
          tile = if r == 0 then Wall else Empty
          (rest, g'') = genCols (n - 1) g'
      in (tile : rest, g'')

-- Tempatkan titik Start dan Goal
placeStartGoal :: Maze -> Maze
placeStartGoal maze =
  let maze'  = setTile maze (0,0) Start
      maze'' = setTile maze (mazeHeight maze - 1, mazeWidth maze - 1) Goal
  in maze''
