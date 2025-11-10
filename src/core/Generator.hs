module Core.Generator (generateMaze) where

import Core.Maze
import System.Random (StdGen, randomR, split)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Semua arah gerak (atas, bawah, kiri, kanan)
directions :: [Coord]
directions = [(-1,0), (1,0), (0,-1), (0,1)]  -- up, down, left, right

-- Generator utama
-- EXPLANATION: We use odd dimensions internally so cells and walls alternate
-- For a 10x10 requested maze, we need at least 11x11 to have proper structure
generateMaze :: Int -> Int -> StdGen -> Maze
generateMaze h w gen =
  let -- Make dimensions odd (minimum 5 to have room for walls and paths)
      h' = if odd h then h else h + 1
      w' = if odd w then w else w + 1
      h'' = max h' 5
      w'' = max w' 5
      
      -- Create maze filled with walls
      emptyMaze = replicate h'' (replicate w'' Wall)
      
      -- Start from position (1,1) - first valid cell, not boundary
      startPos  = (1, 1)
      
      -- Carve the maze
      carved    = carveMaze emptyMaze startPos gen
      
      -- Find farthest empty cell from start for goal
      goalPos   = findFarthest carved startPos
      
      -- Place Start and Goal markers
      withStart = setTile carved startPos Start
      withGoal  = setTile withStart goalPos Goal
  in withGoal

-- Carve maze menggunakan Randomized DFS
-- EXPLANATION: This is a recursive backtracking algorithm
-- 1. Mark current cell as Empty (carved)
-- 2. Randomly shuffle directions
-- 3. For each direction:
--    - Calculate next cell (2 steps away, so there's a wall between)
--    - If next cell is valid and uncarved, carve the wall between and recurse
carveMaze :: Maze -> Coord -> StdGen -> Maze
carveMaze maze pos gen =
  let -- First, mark current position as Empty
      maze' = setTile maze pos Empty
      
      -- Shuffle directions randomly
      (shuffledDirs, gen') = shuffle gen directions
      
      -- Process each direction, threading the generator
      (finalMaze, _) = foldl processDir (maze', gen') shuffledDirs
  in finalMaze
  where
    processDir (m, g) dir =
      let -- Move 2 cells in this direction (skipping the wall between)
          next = move2 pos dir
          
          -- Split generator for recursion
          (g1, g2) = split g
      in 
      -- Check if we can carve to this cell
      if isValidCarve m next
      then 
        -- Carve the wall between current and next
        let wallPos = midpoint pos next
            m1 = setTile m wallPos Empty
            -- Carve the next cell
            m2 = setTile m1 next Empty
            -- Recurse to continue carving from next cell
            m3 = carveMaze m2 next g1
        in (m3, g2)
      else 
        -- Can't carve here, try next direction
        (m, g2)

-- Move 2 steps in a direction (to skip the wall cell)
-- EXPLANATION: In our maze structure, cells alternate with walls
-- (1,1) is a cell, (1,2) is a wall, (1,3) is a cell, etc.
move2 :: Coord -> Coord -> Coord
move2 (r, c) (dr, dc) = (r + 2*dr, c + 2*dc)

-- Find the midpoint between two cells (this is the wall between them)
midpoint :: Coord -> Coord -> Coord
midpoint (r1, c1) (r2, c2) = ((r1 + r2) `div` 2, (c1 + c2) `div` 2)

-- Check if a position is valid for carving
-- EXPLANATION: Position must be:
-- 1. Inside the maze boundaries (not on the edge)
-- 2. Currently a Wall (not already carved)
isValidCarve :: Maze -> Coord -> Bool
isValidCarve maze (r, c) =
  r > 0 && c > 0 && 
  r < mazeHeight maze - 1 && 
  c < mazeWidth maze - 1 &&
  getTile maze (r, c) == Just Wall

-- Find the empty cell farthest from start (Manhattan distance)
-- EXPLANATION: This gives us a challenging goal position
-- that requires navigating through the maze
findFarthest :: Maze -> Coord -> Coord
findFarthest maze start =
  let empties = [ (pos, manhattan start pos)
                | r <- [0 .. mazeHeight maze - 1]
                , c <- [0 .. mazeWidth maze - 1]
                , getTile maze (r, c) == Just Empty
                , let pos = (r, c)
                , pos /= start  -- Don't pick the start position
                ]
  in if null empties
     then start  -- Fallback (shouldn't happen with proper carving)
     else fst $ maximumBy (comparing snd) empties
  where
    manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

-- Fisher-Yates shuffle algorithm
-- EXPLANATION: Randomly reorder a list using a random number generator
-- Returns both the shuffled list and the updated generator
shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle gen [] = ([], gen)
shuffle gen [x] = ([x], gen)
shuffle gen xs = go gen xs []
  where
    go g [] acc = (acc, g)
    go g remaining acc =
      let (idx, g') = randomR (0, length remaining - 1) g
          picked = remaining !! idx
          rest = take idx remaining ++ drop (idx + 1) remaining
      in go g' rest (picked : acc)