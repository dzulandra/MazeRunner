module Core.Generator (generateMaze) where

import Core.Maze
import System.Random (StdGen, randomR, split)
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)

directions :: [Coord]
directions = [(-1,0), (1,0), (0,-1), (0,1)]

-- Main maze generator with gate system
generateMaze :: Int -> Int -> StdGen -> Maze
generateMaze h w gen =
  let -- Ensure odd dimensions
      h' = if odd h then h else h + 1
      w' = if odd w then w else w + 1
      h'' = max h' 5
      w'' = max w' 5
      
      -- Create all-wall maze
      emptyMaze = replicate h'' (replicate w'' Wall)
      startPos  = (1, 1)
      
      -- Split generators
      (g1, g2) = split gen
      (g3, g4) = split g2
      (g5, g6) = split g4
      
      -- Carve paths
      carved = carveMaze emptyMaze startPos g1
      
      -- Place key in middle area (easily accessible from start)
      keyPos = findMiddlePosition carved startPos
      
      -- Find goal position (farthest from key)
      goalPos = findFarthestFrom carved keyPos
      
      -- Place gates NEAR GOAL (not near key or start)
      withGates = placeGatesNearGoal carved startPos keyPos goalPos g3
      
      -- Add special walls (not near edges)
      withSpecial = addSpecialWalls withGates g5
      
      -- Place markers
      withStart = setTile withSpecial startPos Start
      withKey = setTile withStart keyPos Key
      withGoal = setTile withKey goalPos Goal
  in withGoal

-- Carve maze using recursive DFS
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

move2 :: Coord -> Coord -> Coord
move2 (r, c) (dr, dc) = (r + 2*dr, c + 2*dc)

midpoint :: Coord -> Coord -> Coord
midpoint (r1, c1) (r2, c2) = ((r1 + r2) `div` 2, (c1 + c2) `div` 2)

isValidCarve :: Maze -> Coord -> Bool
isValidCarve maze (r, c) =
  r > 0 && c > 0 && 
  r < mazeHeight maze - 1 && 
  c < mazeWidth maze - 1 &&
  getTile maze (r, c) == Just Wall

-- Find position roughly in middle of carved area
findMiddlePosition :: Maze -> Coord -> Coord
findMiddlePosition maze start =
  let empties = [ (pos, manhattan start pos)
                | r <- [0 .. mazeHeight maze - 1]
                , c <- [0 .. mazeWidth maze - 1]
                , getTile maze (r, c) == Just Empty
                , let pos = (r, c)
                , pos /= start
                ]
      sorted = sortBy (comparing snd) empties
      middleIdx = length sorted `div` 2
  in if null sorted then start else fst (sorted !! middleIdx)

-- Find position farthest from given position
findFarthestFrom :: Maze -> Coord -> Coord
findFarthestFrom maze fromPos =
  let empties = [ (pos, manhattan fromPos pos)
                | r <- [0 .. mazeHeight maze - 1]
                , c <- [0 .. mazeWidth maze - 1]
                , getTile maze (r, c) == Just Empty
                , let pos = (r, c)
                , pos /= fromPos
                ]
  in if null empties then fromPos else fst $ maximumBy (comparing snd) empties

-- Place gates near goal, ensuring key remains accessible
placeGatesNearGoal :: Maze -> Coord -> Coord -> Coord -> StdGen -> Maze
placeGatesNearGoal maze start key goal gen =
  let -- Find empty cells in the "goal area"
      distToGoal pos = manhattan pos goal
      distKeyToGoal = manhattan key goal
      distStartToKey = manhattan start key
      
      allEmpty = [ (r, c)
                 | r <- [0 .. mazeHeight maze - 1]
                 , c <- [0 .. mazeWidth maze - 1]
                 , getTile maze (r, c) == Just Empty
                 , let pos = (r, c)
                 , pos /= start && pos /= key && pos /= goal
                 -- Must be closer to goal than to key (goal side)
                 , manhattan pos goal < manhattan pos key
                 -- Must be farther from start than key is (past the key)
                 , manhattan pos start > distStartToKey
                 -- Not too close to goal (leave room)
                 , distToGoal pos > 2
                 ]
      
      -- Sort by distance to goal (prefer positions closer to goal)
      sortedByGoalDist = sortBy (comparing (manhattan goal)) allEmpty
      
      -- Take 2-4 positions for gates
      numGates = min 4 (max 2 (length sortedByGoalDist `div` 8))
      (gatePositions, _) = selectRandom numGates (take 20 sortedByGoalDist) gen
      
      finalMaze = foldl (\m pos -> setTile m pos Gate) maze gatePositions
  in finalMaze

-- Add special walls (not near edges)
addSpecialWalls :: Maze -> StdGen -> Maze
addSpecialWalls maze gen =
  let allWalls = findMeaningfulWalls maze
      numSpecial = max 2 (length allWalls `div` 6)
      (specialWalls, gen') = selectRandom numSpecial allWalls gen
      splitPoint = (length specialWalls * 3) `div` 5
      (breakables, jumpables) = splitAt splitPoint specialWalls
      withBreakable = foldl (\m pos -> setTile m pos BreakableWall) maze breakables
      withJumpable = foldl (\m pos -> setTile m pos JumpableWall) withBreakable jumpables
  in withJumpable

-- Find meaningful walls (not near edges, actually between paths)
findMeaningfulWalls :: Maze -> [Coord]
findMeaningfulWalls maze =
  [ (r, c)
  | r <- [2 .. mazeHeight maze - 3]
  , c <- [2 .. mazeWidth maze - 3]
  , getTile maze (r, c) == Just Wall
  , hasPerpedicularEmptyNeighbors maze (r, c)  -- Must have paths on opposite sides
  , not (isIsolatedWall maze (r, c))
  ]

-- Check if wall has empty spaces on opposite sides (useful for breaking/jumping)
hasPerpedicularEmptyNeighbors :: Maze -> Coord -> Bool
hasPerpedicularEmptyNeighbors maze (r, c) =
  let -- Check horizontal (left-right)
      hasHorizontal = (getTile maze (r, c-1) == Just Empty || getTile maze (r, c-1) == Just Key) &&
                      (getTile maze (r, c+1) == Just Empty || getTile maze (r, c+1) == Just Key)
      -- Check vertical (up-down)
      hasVertical = (getTile maze (r-1, c) == Just Empty || getTile maze (r-1, c) == Just Key) &&
                    (getTile maze (r+1, c) == Just Empty || getTile maze (r+1, c) == Just Key)
  in hasHorizontal || hasVertical

isIsolatedWall :: Maze -> Coord -> Bool
isIsolatedWall maze (r, c) =
  let wallCount = length $ filter isWallTile directions
      isWallTile (dr, dc) = case getTile maze (r + dr, c + dc) of
        Just Wall -> True
        Just BreakableWall -> True
        Just JumpableWall -> True
        _ -> False
  in wallCount >= 3

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

manhattan :: Coord -> Coord -> Int
manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

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