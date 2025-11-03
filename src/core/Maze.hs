module Core.Maze
  ( Coord
  , Tile(..)
  , Maze
  , mazeWidth
  , mazeHeight
  , getTile
  , setTile
  , findTile
  ) where


import System.Random (StdGen, randomR)

type Coord = (Int, Int)

data Tile
  = Empty
  | Wall
  | Start
  | Goal
  deriving (Eq, Show)

type Maze = [[Tile]]

-- Ambil ukuran maze
mazeWidth :: Maze -> Int
mazeWidth = length . head

mazeHeight :: Maze -> Int
mazeHeight = length

-- Ambil tile pada koordinat
getTile :: Maze -> Coord -> Maybe Tile
getTile maze (r, c)
  | r < 0 || c < 0 || r >= mazeHeight maze || c >= mazeWidth maze = Nothing
  | otherwise = Just ((maze !! r) !! c)

-- Update tile secara immutabel
setTile :: Maze -> Coord -> Tile -> Maze
setTile maze (r, c) newTile =
  take r maze ++
  [take c (maze !! r) ++ [newTile] ++ drop (c + 1) (maze !! r)] ++
  drop (r + 1) maze

-- Cari posisi tile tertentu dalam maze
findTile :: Tile -> Maze -> Maybe (Int, Int)
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
