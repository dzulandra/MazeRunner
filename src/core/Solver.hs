module Core.Solver (solveMaze) where

import Core.Maze
import Data.Maybe (fromJust)

-- Arah gerak (atas, bawah, kiri, kanan)
directions :: [Coord]
directions = [(1,0), (-1,0), (0,1), (0,-1)]

-- Solver utama (DFS rekursif)
solveMaze :: Maze -> Maybe [Coord]
solveMaze maze =
  case findTile Start maze of
    Nothing -> Nothing
    Just start -> dfs [] start
  where
    goal = fromJust (findTile Goal maze)

    dfs visited current
      | current == goal = Just [current]
      | otherwise =
          let nextSteps =
                [ (r', c')
                | (dr, dc) <- directions
                , let (r', c') = (fst current + dr, snd current + dc)
                , notElem (r', c') visited
                , getTile maze (r', c') `elem` [Just Empty, Just Goal]
                ]
          in tryPaths visited current nextSteps

    tryPaths _ _ [] = Nothing
    tryPaths visited curr (n:ns) =
      case dfs (curr:visited) n of
        Just path -> Just (curr:path)
        Nothing   -> tryPaths visited curr ns
