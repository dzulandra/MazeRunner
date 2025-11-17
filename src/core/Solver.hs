{-# LANGUAGE RecordWildCards #-}

module Core.Solver (solveMaze, SolverState(..), Move(..)) where

import Core.Maze
import qualified Data.Set as Set
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Represent different types of moves
data Move 
  = Walk Coord           -- Normal walk
  | Break Coord          -- Break a wall
  | Jump Coord Coord     -- Jump over a wall (from, to)
  deriving (Eq, Show)

-- Solver state tracks position and remaining abilities
data SolverState = SolverState
  { ssPosition :: Coord
  , ssBreaksLeft :: Int
  , ssJumpsLeft :: Int
  , ssPath :: [Move]
  } deriving (Eq, Show)

-- For visited set, we only care about position and remaining abilities
instance Ord SolverState where
  compare s1 s2 = 
    case compare (ssPosition s1) (ssPosition s2) of
      EQ -> case compare (ssBreaksLeft s1) (ssBreaksLeft s2) of
              EQ -> compare (ssJumpsLeft s1) (ssJumpsLeft s2)
              other -> other
      other -> other

-- Arah gerak (atas, bawah, kiri, kanan)
directions :: [Coord]
directions = [(-1,0), (1,0), (0,-1), (0,1)]

-- Solver dengan limit breaks dan jumps (menggunakan A* algorithm)
solveMaze :: Maze -> Maybe [Move]
solveMaze maze =
  case (findTile Start maze, findTile Goal maze) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just start, Just goal) -> 
      let initialState = SolverState start 3 2 []  -- 3 breaks, 2 jumps
      in astar maze goal (Set.singleton initialState) [initialState]

-- A* pathfinding with priority queue (simplified with list)
astar :: Maze -> Coord -> Set.Set SolverState -> [SolverState] -> Maybe [Move]
astar _ _ _ [] = Nothing
astar maze goal visited (current:queue)
  | ssPosition current == goal = Just (reverse $ ssPath current)
  | otherwise =
      let neighbors = getNeighborStates maze current
          newStates = filter (\s -> not (Set.member s visited)) neighbors
          visited' = foldl (flip Set.insert) visited newStates
          -- Sort by heuristic (distance to goal + path length)
          sortedNew = sortByHeuristic goal newStates
          queue' = queue ++ sortedNew
      in astar maze goal visited' queue'

-- Get all possible next states from current state
getNeighborStates :: Maze -> SolverState -> [SolverState]
getNeighborStates maze state@SolverState{..} =
  let (r, c) = ssPosition
      -- Try normal walks
      walks = [ state { ssPosition = (r', c')
                      , ssPath = Walk (r', c') : ssPath }
              | (dr, dc) <- directions
              , let r' = r + dr
              , let c' = c + dc
              , isWalkable maze (r', c')
              ]
      -- Try breaking walls
      breaks = [ state { ssPosition = (r', c')
                       , ssBreaksLeft = ssBreaksLeft - 1
                       , ssPath = Break (r', c') : ssPath }
               | ssBreaksLeft > 0
               , (dr, dc) <- directions
               , let r' = r + dr
               , let c' = c + dc
               , getTile maze (r', c') == Just BreakableWall
               ]
      -- Try jumping over walls
      jumps = [ state { ssPosition = (r'', c'')
                      , ssJumpsLeft = ssJumpsLeft - 1
                      , ssPath = Jump (r', c') (r'', c'') : ssPath }
              | ssJumpsLeft > 0
              , (dr, dc) <- directions
              , let r' = r + dr
              , let c' = c + dc
              , getTile maze (r', c') == Just JumpableWall
              , let r'' = r + 2*dr
              , let c'' = c + 2*dc
              , isWalkable maze (r'', c'')
              ]
  in walks ++ breaks ++ jumps

-- Check if a position is walkable
isWalkable :: Maze -> Coord -> Bool
isWalkable maze pos =
  case getTile maze pos of
    Just Empty -> True
    Just Goal -> True
    Just Start -> True
    _ -> False

-- Sort states by heuristic (Manhattan distance + path cost)
sortByHeuristic :: Coord -> [SolverState] -> [SolverState]
sortByHeuristic goal states =
  let heuristic state = manhattan (ssPosition state) goal + length (ssPath state)
  in sortBy (comparing heuristic) states
  where
    sortBy _ [] = []
    sortBy cmp (x:xs) =
      let smaller = sortBy cmp [y | y <- xs, cmp y x == LT]
          larger = sortBy cmp [y | y <- xs, cmp y x /= LT]
      in smaller ++ [x] ++ larger

-- Manhattan distance
manhattan :: Coord -> Coord -> Int
manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)