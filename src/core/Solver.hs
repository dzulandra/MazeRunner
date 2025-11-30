{-# LANGUAGE RecordWildCards #-}

module Core.Solver (solveMaze, SolverState(..), Move(..), SolverConfig(..)) where

import Core.Maze
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Ord (comparing)

-- Configuration for solver
data SolverConfig = SolverConfig
  { solverMaxBreaks :: Int
  , solverMaxJumps :: Int
  } deriving (Show)

-- Move types
data Move 
  = Walk Coord
  | Break Coord
  | Jump Coord Coord
  | CollectKey Coord
  | PassGate Coord
  deriving (Eq, Show)

-- Solver state with key tracking
data SolverState = SolverState
  { ssPosition :: Coord
  , ssBreaksLeft :: Int
  , ssJumpsLeft :: Int
  , ssHasKey :: Bool
  , ssPath :: [Move]
  } deriving (Eq, Show)

instance Ord SolverState where
  compare s1 s2 = 
    case compare (ssPosition s1) (ssPosition s2) of
      EQ -> case compare (ssBreaksLeft s1) (ssBreaksLeft s2) of
              EQ -> case compare (ssJumpsLeft s1) (ssJumpsLeft s2) of
                      EQ -> compare (ssHasKey s1) (ssHasKey s2)
                      other -> other
              other -> other
      other -> other

directions :: [Coord]
directions = [(-1,0), (1,0), (0,-1), (0,1)]

-- Main solver function with configurable limits
solveMaze :: Maze -> SolverConfig -> Maybe [Move]
solveMaze maze config =
  case (findTile Start maze, findTile Goal maze) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just start, Just goal) -> 
      let initialState = SolverState start (solverMaxBreaks config) (solverMaxJumps config) False []
      in astar maze goal (Set.singleton initialState) [initialState]

-- A* algorithm - try without key first, if fails then we need key
astar :: Maze -> Coord -> Set.Set SolverState -> [SolverState] -> Maybe [Move]
astar _ _ _ [] = Nothing
astar maze goal visited (current:queue)
  -- Can reach goal regardless of key status (gates might not be in the way)
  | ssPosition current == goal = 
      Just (reverse $ ssPath current)
  | otherwise =
      let neighbors = getNeighborStates maze current
          newStates = filter (\s -> not (Set.member s visited)) neighbors
          visited' = foldl (flip Set.insert) visited newStates
          sortedNew = sortByHeuristic goal newStates
          queue' = queue ++ sortedNew
      in astar maze goal visited' queue'

-- Generate all possible next states
getNeighborStates :: Maze -> SolverState -> [SolverState]
getNeighborStates maze state@SolverState{..} =
  let (r, c) = ssPosition
      
      -- Regular walks (Empty, Goal, Start, Key only - NOT gates without key)
      walks = [ state { ssPosition = (r', c')
                      , ssPath = Walk (r', c') : ssPath }
              | (dr, dc) <- directions
              , let r' = r + dr
              , let c' = c + dc
              , isWalkable maze (r', c')
              , not (isGateWithoutKey maze (r', c') ssHasKey)  -- Can't walk through gates without key
              ]
      
      -- Collect key if standing on it
      collectKey = [ state { ssHasKey = True
                           , ssPath = CollectKey (r, c) : ssPath }
                   | not ssHasKey
                   , getTile maze (r, c) == Just Key
                   ]
      
      -- Pass through gates (only if have key)
      passGates = [ state { ssPosition = (r', c')
                          , ssPath = PassGate (r', c') : ssPath }
                  | ssHasKey  -- Must have key
                  , (dr, dc) <- directions
                  , let r' = r + dr
                  , let c' = c + dc
                  , getTile maze (r', c') == Just Gate
                  ]
      
      -- Break walls
      breaks = [ state { ssPosition = (r', c')
                       , ssBreaksLeft = ssBreaksLeft - 1
                       , ssPath = Break (r', c') : ssPath }
               | ssBreaksLeft > 0
               , (dr, dc) <- directions
               , let r' = r + dr
               , let c' = c + dc
               , getTile maze (r', c') == Just BreakableWall
               ]
      
      -- Jump over walls
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
              , not (isGateWithoutKey maze (r'', c'') ssHasKey)  -- Can't land on gate without key
              ]
  in walks ++ collectKey ++ passGates ++ breaks ++ jumps

-- Check if position is a gate and we don't have key
isGateWithoutKey :: Maze -> Coord -> Bool -> Bool
isGateWithoutKey maze pos hasKey =
  getTile maze pos == Just Gate && not hasKey

isWalkable :: Maze -> Coord -> Bool
isWalkable maze pos =
  case getTile maze pos of
    Just Empty -> True
    Just Goal -> True
    Just Start -> True
    Just Key -> True
    _ -> False

sortByHeuristic :: Coord -> [SolverState] -> [SolverState]
sortByHeuristic goal states =
  let heuristic state = 
        let distToGoal = manhattan (ssPosition state) goal
            pathLen = length (ssPath state)
        in distToGoal + pathLen
  in sortBy (comparing heuristic) states

manhattan :: Coord -> Coord -> Int
manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)