module Core.Generator (generateMaze, MazeConfig(..)) where

import Core.Maze
import System.Random (StdGen, randomR, split)
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set

-- Configuration for maze generation
data MazeConfig = MazeConfig
  { cfgBreakablePercent :: Int
  , cfgJumpablePercent :: Int
  } deriving (Show)

-- Direction vectors for movement
directions :: [Coord]
directions = [(-1,0), (1,0), (0,-1), (0,1)]

-- ==================== MAIN GENERATOR ====================

-- Main maze generator with gate system and verification
generateMaze :: Int -> Int -> MazeConfig -> StdGen -> Maze
generateMaze h w config gen =
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
      
      -- Place key in middle area
      keyPos = findMiddlePosition carved startPos
      
      -- Find goal position (farthest from key)
      goalPos = findFarthestFrom carved keyPos
      
      -- Place gates near goal (may block key initially)
      withGatesRaw = placeGatesNearGoal carved startPos keyPos goalPos g3
      
      -- VERIFY and FIX: Remove any gates blocking key access
      withGatesSafe = ensureKeyReachable withGatesRaw startPos keyPos
      
      -- Add special walls (not near edges) with configurable percentages
      withSpecial = addSpecialWalls withGatesSafe config g5
      
      -- Place markers
      withStart = setTile withSpecial startPos Start
      withKey = setTile withStart keyPos Key
      withGoal = setTile withKey goalPos Goal
  in withGoal

-- ==================== MAZE CARVING ====================

-- Carve maze using recursive DFS with backtracking
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

-- ==================== POSITION FINDING ====================

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

-- ==================== GATE PLACEMENT ====================

-- Place gates near goal (initial placement, may block key)
placeGatesNearGoal :: Maze -> Coord -> Coord -> Coord -> StdGen -> Maze
placeGatesNearGoal maze start key goal gen =
  let distStartToKey = manhattan start key
      
      allEmpty = [ (r, c)
                 | r <- [0 .. mazeHeight maze - 1]
                 , c <- [0 .. mazeWidth maze - 1]
                 , getTile maze (r, c) == Just Empty
                 , let pos = (r, c)
                 , pos /= start && pos /= key && pos /= goal
                 , manhattan pos goal < manhattan pos key
                 , manhattan pos start > distStartToKey - 3
                 , manhattan pos goal > 2
                 ]
      
      sortedByGoalDist = sortBy (comparing (manhattan goal)) allEmpty
      numGates = min 5 (max 3 (length sortedByGoalDist `div` 6))
      (gatePositions, _) = selectRandom numGates (take 25 sortedByGoalDist) gen
      finalMaze = foldl (\m pos -> setTile m pos Gate) maze gatePositions
  in finalMaze

-- ==================== KEY REACHABILITY VERIFICATION ====================

-- Ensure key is reachable from start by removing blocking gates
ensureKeyReachable :: Maze -> Coord -> Coord -> Maze
ensureKeyReachable maze start key =
  if canReachWithoutKey maze start key
  then maze
  else removeBlockingGates maze start key

-- Check if key is reachable from start without passing gates
canReachWithoutKey :: Maze -> Coord -> Coord -> Bool
canReachWithoutKey maze start key =
  case simplePathfind maze start key of
    Just _ -> True
    Nothing -> False

-- Simple BFS pathfinding treating gates as walls
simplePathfind :: Maze -> Coord -> Coord -> Maybe [Coord]
simplePathfind maze start goal = bfs [start] Set.empty
  where
    bfs [] _ = Nothing
    bfs (current:queue) visited
      | current == goal = Just [goal]
      | Set.member current visited = bfs queue visited
      | otherwise =
          let visited' = Set.insert current visited
              neighbors = getSimpleNeighbors maze current
              newNeighbors = filter (\n -> not (Set.member n visited')) neighbors
              queue' = queue ++ newNeighbors
          in bfs queue' visited'
    
    getSimpleNeighbors m (r, c) =
      [ (r', c')
      | (dr, dc) <- directions
      , let r' = r + dr
      , let c' = c + dc
      , isWalkableWithoutKey m (r', c')
      ]
    
    isWalkableWithoutKey m pos =
      case getTile m pos of
        Just Empty -> True
        Just Key -> True
        Just Start -> True
        _ -> False

-- Remove gates that block path from start to key
-- Strategy: Remove all gates, then put back only those that don't block
removeBlockingGates :: Maze -> Coord -> Coord -> Maze
removeBlockingGates maze start key =
  let allGates = findAllGates maze
      -- First, remove ALL gates temporarily
      mazeNoGates = foldl (\m pos -> setTile m pos Empty) maze allGates
  in if canReachWithoutKey mazeNoGates start key
     then mazeNoGates  -- Key is reachable, keep all gates removed
     else maze  -- Something else is blocking, keep original maze

-- Find all gate positions in maze
findAllGates :: Maze -> [Coord]
findAllGates maze =
  [ (r, c)
  | r <- [0 .. mazeHeight maze - 1]
  , c <- [0 .. mazeWidth maze - 1]
  , getTile maze (r, c) == Just Gate
  ]

-- ==================== SPECIAL WALLS ====================

-- Add special walls (not near edges, strategic positions) with configurable percentages
addSpecialWalls :: Maze -> MazeConfig -> StdGen -> Maze
addSpecialWalls maze config gen =
  let allWalls = findMeaningfulWalls maze
      totalWalls = length allWalls
      
      -- Calculate number of each type based on percentages
      numBreakable = (totalWalls * cfgBreakablePercent config) `div` 100
      numJumpable = (totalWalls * cfgJumpablePercent config) `div` 100
      
      -- Select random walls for each type
      (g1, g2) = split gen
      (breakables, _) = selectRandom numBreakable allWalls g1
      
      -- Remove already selected breakables from consideration for jumpables
      remainingWalls = filter (`notElem` breakables) allWalls
      (jumpables, _) = selectRandom numJumpable remainingWalls g2
      
      -- Apply the special walls
      withBreakable = foldl (\m pos -> setTile m pos BreakableWall) maze breakables
      withJumpable = foldl (\m pos -> setTile m pos JumpableWall) withBreakable jumpables
  in withJumpable

-- Find meaningful walls (not near edges, between perpendicular paths)
findMeaningfulWalls :: Maze -> [Coord]
findMeaningfulWalls maze =
  [ (r, c)
  | r <- [2 .. mazeHeight maze - 3]
  , c <- [2 .. mazeWidth maze - 3]
  , getTile maze (r, c) == Just Wall
  , hasPerpedicularEmptyNeighbors maze (r, c)
  , not (isIsolatedWall maze (r, c))
  ]

-- Check if wall has empty spaces on opposite sides
hasPerpedicularEmptyNeighbors :: Maze -> Coord -> Bool
hasPerpedicularEmptyNeighbors maze (r, c) =
  let hasHorizontal = (getTile maze (r, c-1) == Just Empty || getTile maze (r, c-1) == Just Key) &&
                      (getTile maze (r, c+1) == Just Empty || getTile maze (r, c+1) == Just Key)
      hasVertical = (getTile maze (r-1, c) == Just Empty || getTile maze (r-1, c) == Just Key) &&
                    (getTile maze (r+1, c) == Just Empty || getTile maze (r+1, c) == Just Key)
  in hasHorizontal || hasVertical

-- Check if wall is isolated (surrounded by other walls)
isIsolatedWall :: Maze -> Coord -> Bool
isIsolatedWall maze (r, c) =
  let wallCount = length $ filter isWallTile directions
      isWallTile (dr, dc) = case getTile maze (r + dr, c + dc) of
        Just Wall -> True
        Just BreakableWall -> True
        Just JumpableWall -> True
        _ -> False
  in wallCount >= 3

-- ==================== UTILITY FUNCTIONS ====================

-- Select N random items from list
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

-- Manhattan distance
manhattan :: Coord -> Coord -> Int
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