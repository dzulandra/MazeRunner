module Core.Solver (solveMaze) where

import Core.Maze
import qualified Data.Set as Set

-- Arah gerak (atas, bawah, kiri, kanan)
directions :: [Coord]
directions = [(-1,0), (1,0), (0,-1), (0,1)]

-- Solver utama menggunakan Depth-First Search
-- EXPLANATION: 
-- 1. Find Start and Goal positions in the maze
-- 2. Use DFS with a visited set to avoid cycles
-- 3. Return the path from Start to Goal if found
solveMaze :: Maze -> Maybe [Coord]
solveMaze maze =
  case (findTile Start maze, findTile Goal maze) of
    (Nothing, _) -> Nothing  -- No start found
    (_, Nothing) -> Nothing  -- No goal found
    (Just start, Just goal) -> dfs Set.empty start goal
  where
    -- DFS recursive function
    -- EXPLANATION:
    -- - visited: Set of coordinates we've already explored
    -- - current: Current position we're exploring
    -- - goal: Target position we're trying to reach
    dfs visited current goal
      -- Base case: reached the goal!
      | current == goal = Just [goal]
      
      -- Already visited this cell, no point exploring again
      | Set.member current visited = Nothing
      
      -- Explore this cell
      | otherwise =
          let -- Mark current as visited
              visited' = Set.insert current visited
              
              -- Get all valid neighboring cells
              neighbors = getValidNeighbors current visited'
              
              -- Try to find a path through each neighbor
          in case tryNeighbors visited' neighbors of
               Nothing -> Nothing  -- No path found through any neighbor
               Just path -> Just (current : path)  -- Path found! Add current to it

    -- Get valid neighboring cells
    -- EXPLANATION: A neighbor is valid if:
    -- 1. It's not already visited
    -- 2. It's inside the maze bounds
    -- 3. It's not a wall (must be Empty, Goal, or Start)
    getValidNeighbors (r, c) visited =
      [ (r', c')
      | (dr, dc) <- directions
      , let r' = r + dr
      , let c' = c + dc
      , not (Set.member (r', c') visited)
      , isWalkable (r', c')
      ]
    
    -- Check if a position is walkable
    isWalkable pos =
      case getTile maze pos of
        Just Empty -> True
        Just Goal  -> True
        Just Start -> True  -- Allow walking on Start (for the first move)
        _          -> False
    
    -- Try to find a path through any of the neighbors
    -- EXPLANATION: Try each neighbor in order until one succeeds
    tryNeighbors _ [] = Nothing
    tryNeighbors visited (n:ns) =
      case dfs visited n (findGoal maze) of
        Just path -> Just path
        Nothing   -> tryNeighbors visited ns
    
    -- Helper to get goal position (we know it exists from outer check)
    findGoal m = case findTile Goal m of
                   Just g -> g
                   Nothing -> error "Goal disappeared!"  -- Should never happen