module Core.Generator (generateMaze) where

import Core.Maze
import System.Random (StdGen, randomR, split)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Semua arah gerak (atas, bawah, kiri, kanan)
directions :: [Coord]
directions = [(-1,0), (1,0), (0,-1), (0,1)]

-- Generator utama dengan special walls
generateMaze :: Int -> Int -> StdGen -> Maze
generateMaze h w gen =
  let -- Make dimensions odd
      h' = if odd h then h else h + 1
      w' = if odd w then w else w + 1
      h'' = max h' 5
      w'' = max w' 5
      
      -- Create maze filled with walls
      emptyMaze = replicate h'' (replicate w'' Wall)
      
      -- Start from position (1,1)
      startPos  = (1, 1)
      
      -- Split generator for different purposes
      (g1, g2) = split gen
      (g3, g4) = split g2
      
      -- Carve the maze
      carved    = carveMaze emptyMaze startPos g1
      
      -- Find farthest empty cell from start for goal
      goalPos   = findFarthest carved startPos
      
      -- Add special walls (breakable and jumpable)
      withSpecial = addSpecialWalls carved g3
      
      -- Place Start and Goal markers
      withStart = setTile withSpecial startPos Start
      withGoal  = setTile withStart goalPos Goal
  in withGoal

-- Add breakable and jumpable walls to the maze
addSpecialWalls :: Maze -> StdGen -> Maze
addSpecialWalls maze gen =
  let allWalls = findAllWalls maze
      -- Select 20% of walls to be special
      numSpecial = max 3 (length allWalls `div` 5)
      (specialWalls, gen') = selectRandom numSpecial allWalls gen
      -- Split special walls: half breakable, half jumpable
      (breakables, jumpables) = splitAt (length specialWalls `div` 2) specialWalls
      -- Apply special wall types
      withBreakable = foldl (\m pos -> setTile m pos BreakableWall) maze breakables
      withJumpable = foldl (\m pos -> setTile m pos JumpableWall) withBreakable jumpables
  in withJumpable

-- Find all wall positions that are adjacent to empty spaces
findAllWalls :: Maze -> [Coord]
findAllWalls maze =
  [ (r, c)
  | r <- [0 .. mazeHeight maze - 1]
  , c <- [0 .. mazeWidth maze - 1]
  , getTile maze (r, c) == Just Wall
  , hasEmptyNeighbor maze (r, c)
  ]

-- Check if a position has at least one empty neighbor
hasEmptyNeighbor :: Maze -> Coord -> Bool
hasEmptyNeighbor maze (r, c) =
  any (\(dr, dc) -> getTile maze (r + dr, c + dc) == Just Empty) directions

-- Select N random items from a list
selectRandom :: Int -> [a] -> StdGen -> ([a], StdGen)
selectRandom 0 _ gen = ([], gen)
selectRandom _ [] gen = ([], gen)
selectRandom n xs gen
  | n >= length xs = (xs, gen)
  | otherwise =
      let (idx, gen') = randomR (0, length xs - 1) gen
          picked = xs !! idx
          rest = take idx xs ++ drop (idx + 1) xs
          (more, gen'') = selectRandom (n - 1) rest gen'
      in (picked : more, gen'')

-- Carve maze menggunakan Randomized DFS
carveMaze :: Maze -> Coord -> StdGen -> Maze
carveMaze maze pos gen =
  let maze' = setTile maze pos Empty
      (shuffledDirs, gen') = shuffle gen directions
      (finalMaze, _) = foldl processDir (maze', gen') shuffledDirs
  in finalMaze
  where
    processDir (m, g) dir =
      let next = move2 pos dir
          (g1, g2) = split g
      in 
      if isValidCarve m next
      then 
        let wallPos = midpoint pos next
            m1 = setTile m wallPos Empty
            m2 = setTile m1 next Empty
            m3 = carveMaze m2 next g1
        in (m3, g2)
      else 
        (m, g2)

-- Move 2 steps in a direction
move2 :: Coord -> Coord -> Coord
move2 (r, c) (dr, dc) = (r + 2*dr, c + 2*dc)

-- Find the midpoint between two cells
midpoint :: Coord -> Coord -> Coord
midpoint (r1, c1) (r2, c2) = ((r1 + r2) `div` 2, (c1 + c2) `div` 2)

-- Check if a position is valid for carving
isValidCarve :: Maze -> Coord -> Bool
isValidCarve maze (r, c) =
  r > 0 && c > 0 && 
  r < mazeHeight maze - 1 && 
  c < mazeWidth maze - 1 &&
  getTile maze (r, c) == Just Wall

-- Find the empty cell farthest from start
findFarthest :: Maze -> Coord -> Coord
findFarthest maze start =
  let empties = [ (pos, manhattan start pos)
                | r <- [0 .. mazeHeight maze - 1]
                , c <- [0 .. mazeWidth maze - 1]
                , getTile maze (r, c) == Just Empty
                , let pos = (r, c)
                , pos /= start
                ]
  in if null empties
     then start
     else fst $ maximumBy (comparing snd) empties
  where
    manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

-- Fisher-Yates shuffle
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