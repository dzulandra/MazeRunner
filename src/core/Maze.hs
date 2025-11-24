module Core.Maze
  ( Coord
  , Tile(..)
  , Maze
  , mazeWidth
  , mazeHeight
  , getTile
  , setTile
  , findTile
  , findAllTiles
  ) where

type Coord = (Int, Int)

-- Tile types: basic, special walls, and gate system
data Tile
  = Empty
  | Wall
  | Start
  | Goal
  | BreakableWall  -- Can be broken (uses 1 break charge)
  | JumpableWall   -- Can be jumped over (uses 1 jump charge)
  | Gate           -- Blocks path until key collected
  | Key            -- Must collect to pass gates
  deriving (Eq, Show)

type Maze = [[Tile]]

-- Get maze dimensions
mazeWidth :: Maze -> Int
mazeWidth = length . head

mazeHeight :: Maze -> Int
mazeHeight = length

-- Get tile at coordinate (safe, returns Maybe)
getTile :: Maze -> Coord -> Maybe Tile
getTile maze (r, c)
  | r < 0 || c < 0 || r >= mazeHeight maze || c >= mazeWidth maze = Nothing
  | otherwise = Just ((maze !! r) !! c)

-- Set tile at coordinate (immutable update)
setTile :: Maze -> Coord -> Tile -> Maze
setTile maze (r, c) newTile =
  take r maze ++
  [take c (maze !! r) ++ [newTile] ++ drop (c + 1) (maze !! r)] ++
  drop (r + 1) maze

-- Find first occurrence of a tile
findTile :: Tile -> Maze -> Maybe Coord
findTile t maze = go 0 maze
  where
    go _ [] = Nothing
    go r (row:rs) =
      case lookupCol 0 row of
        Just c  -> Just (r, c)
        Nothing -> go (r + 1) rs
      where
        lookupCol _ [] = Nothing
        lookupCol c (x:xs)
          | x == t    = Just c
          | otherwise = lookupCol (c + 1) xs

-- Find all occurrences of a tile
findAllTiles :: Tile -> Maze -> [Coord]
findAllTiles t maze =
  [ (r, c)
  | r <- [0 .. mazeHeight maze - 1]
  , c <- [0 .. mazeWidth maze - 1]
  , getTile maze (r, c) == Just t
  ]