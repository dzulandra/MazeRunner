{-# LANGUAGE RecordWildCards #-}

module UI (runUI) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, mkStdGen, split)
import Core.Maze
import Core.Generator
import Core.Solver
import Data.Maybe (fromMaybe)

-- ==================== CONSTANTS ====================
windowWidth, windowHeight :: Int
windowWidth = 1000
windowHeight = 750

cellSize :: Float
cellSize = 25

-- Button definitions
data Button = Button 
  { btnX :: Float
  , btnY :: Float
  , btnWidth :: Float
  , btnHeight :: Float
  , btnLabel :: String
  } deriving (Show)

newMazeBtn :: Button
newMazeBtn = Button (-250) (-300) 200 50 "New Maze"

solveBtn :: Button
solveBtn = Button 50 (-300) 200 50 "Solve"

-- ==================== GAME STATE ====================
data GameState = GameState
  { gsMaze :: Maze
  , gsPath :: Maybe [Move]
  , gsPathIndex :: Int
  , gsAnimating :: Bool
  , gsGen :: StdGen
  , gsMazeWidth :: Int
  , gsMazeHeight :: Int
  , gsBreaksUsed :: Int
  , gsJumpsUsed :: Int
  } deriving (Show)

-- Initial state
initialState :: StdGen -> GameState
initialState gen = 
  let w = 21
      h = 15
      maze = generateMaze h w gen
      (_, gen') = split gen
  in GameState
     { gsMaze = maze
     , gsPath = Nothing
     , gsPathIndex = 0
     , gsAnimating = False
     , gsGen = gen'
     , gsMazeWidth = w
     , gsMazeHeight = h
     , gsBreaksUsed = 0
     , gsJumpsUsed = 0
     }

-- ==================== RENDERING ====================
runUI :: IO ()
runUI = do
  let gen = mkStdGen 42
      window = InWindow "MazeRunner - Special Walls Edition" 
                        (windowWidth, windowHeight) 
                        (100, 100)
      bgColor = makeColorI 30 30 40 255
  play window bgColor 30 (initialState gen) drawGame handleEvent updateGame

-- Main drawing function
drawGame :: GameState -> Picture
drawGame gs@GameState{..} = pictures
  [ drawMaze gsMaze gsPath gsPathIndex
  , drawButton newMazeBtn (greyN 0.3) white
  , drawButton solveBtn (greyN 0.3) white
  , drawTitle
  , drawInstructions gs
  , drawLegend
  , drawStats gs
  ]

-- Draw the maze
drawMaze :: Maze -> Maybe [Move] -> Int -> Picture
drawMaze maze mPath pathIdx = 
  let h = mazeHeight maze
      w = mazeWidth maze
      offsetX = -(fromIntegral w * cellSize) / 2
      offsetY = (fromIntegral h * cellSize) / 2 + 50
      
      -- Get visible path
      visibleMoves = case mPath of
        Nothing -> []
        Just p -> take pathIdx p
      
      visiblePath = movesToCoords visibleMoves
      
      startPos = findTile Start maze
      goalPos = findTile Goal maze
      
      -- Draw all cells
      cells = [ drawCell maze (r, c) visiblePath visibleMoves startPos goalPos offsetX offsetY
              | r <- [0..h-1]
              , c <- [0..w-1]
              ]
  in pictures cells

-- Convert moves to coordinates for path display
movesToCoords :: [Move] -> [Coord]
movesToCoords = concatMap moveToCoords
  where
    moveToCoords (Walk pos) = [pos]
    moveToCoords (Break pos) = [pos]
    moveToCoords (Jump _ to) = [to]

-- Draw a single cell
drawCell :: Maze -> Coord -> [Coord] -> [Move] -> Maybe Coord -> Maybe Coord 
         -> Float -> Float -> Picture
drawCell maze pos@(r, c) path moves startPos goalPos offsetX offsetY =
  let x = offsetX + fromIntegral c * cellSize
      y = offsetY - fromIntegral r * cellSize
      tile = fromMaybe Wall (getTile maze pos)
      
      -- Check if this position was broken or jumped
      wasBroken = any (\m -> case m of Break p -> p == pos; _ -> False) moves
      wasJumped = any (\m -> case m of Jump j _ -> j == pos; _ -> False) moves
      
      -- Determine color
      cellColor
        | Just pos == startPos = makeColorI 50 200 50 255     -- Green start
        | Just pos == goalPos  = makeColorI 200 50 50 255     -- Red goal
        | pos `elem` path      = makeColorI 100 150 255 255   -- Blue path
        | wasBroken            = makeColorI 150 150 150 255   -- Gray (broken wall)
        | wasJumped            = makeColorI 200 200 100 255   -- Yellow (jumped wall)
        | tile == Wall         = makeColorI 50 50 60 255      -- Dark wall
        | tile == BreakableWall = makeColorI 139 69 19 255    -- Brown breakable
        | tile == JumpableWall = makeColorI 255 165 0 255     -- Orange jumpable
        | otherwise            = makeColorI 200 200 210 255   -- Light floor
      
      cell = translate x y $ color cellColor $ 
             rectangleSolid cellSize cellSize
      
      -- Border
      border = translate x y $ color (greyN 0.2) $ 
               rectangleWire cellSize cellSize
      
      -- Special marker for special walls
      marker = case tile of
        BreakableWall -> translate x y $ color white $ 
                        scale 0.1 0.1 $ text "B"
        JumpableWall -> translate x y $ color white $ 
                       scale 0.1 0.1 $ text "J"
        _ -> blank
  in pictures [cell, border, marker]

-- Draw a button
drawButton :: Button -> Color -> Color -> Picture
drawButton Button{..} bgCol textCol = pictures
  [ translate btnX btnY $ color bgCol $ 
    rectangleSolid btnWidth btnHeight
  , translate btnX btnY $ color white $ 
    rectangleWire btnWidth btnHeight
  , translate (btnX - 80) (btnY - 8) $ 
    scale 0.15 0.15 $ color textCol $ text btnLabel
  ]

-- Draw title
drawTitle :: Picture
drawTitle = translate (-320) 340 $ scale 0.3 0.3 $ 
            color white $ text "MazeRunner: Special Walls"

-- Draw instructions
drawInstructions :: GameState -> Picture
drawInstructions GameState{..} = pictures
  [ translate (-320) 300 $ scale 0.12 0.12 $ color (greyN 0.7) $ 
    text status
  , translate (-320) 270 $ scale 0.12 0.12 $ color (greyN 0.7) $ 
    text "Brown walls = Breakable (3 max) | Orange walls = Jumpable (2 max)"
  ]
  where
    status = case (gsPath, gsAnimating) of
      (Nothing, _) -> "Click 'Solve' to find the path"
      (Just p, True) -> "Solving... (" ++ show gsPathIndex ++ "/" ++ show (length p) ++ ")"
      (Just p, False) -> "Solved! Path length: " ++ show (length p)

-- Draw legend
drawLegend :: Picture
drawLegend = translate 280 280 $ pictures
  [ scale 0.12 0.12 $ color white $ text "Legend:"
  , translate 0 (-30) $ color (makeColorI 50 200 50 255) $ rectangleSolid 20 20
  , translate 30 (-35) $ scale 0.1 0.1 $ color white $ text "Start"
  , translate 0 (-60) $ color (makeColorI 200 50 50 255) $ rectangleSolid 20 20
  , translate 30 (-65) $ scale 0.1 0.1 $ color white $ text "Goal"
  , translate 0 (-90) $ color (makeColorI 139 69 19 255) $ rectangleSolid 20 20
  , translate 30 (-95) $ scale 0.1 0.1 $ color white $ text "Breakable"
  , translate 0 (-120) $ color (makeColorI 255 165 0 255) $ rectangleSolid 20 20
  , translate 30 (-125) $ scale 0.1 0.1 $ color white $ text "Jumpable"
  ]

-- Draw stats
drawStats :: GameState -> Picture
drawStats GameState{..} = translate 280 100 $ pictures
  [ scale 0.12 0.12 $ color white $ text "Statistics:"
  , translate 0 (-30) $ scale 0.1 0.1 $ color (greyN 0.8) $ 
    text $ "Breaks used: " ++ show gsBreaksUsed ++ "/3"
  , translate 0 (-55) $ scale 0.1 0.1 $ color (greyN 0.8) $ 
    text $ "Jumps used: " ++ show gsJumpsUsed ++ "/2"
  ]

-- ==================== EVENT HANDLING ====================
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) gs =
  handleClick mousePos gs
handleEvent _ gs = gs

-- Handle mouse clicks
handleClick :: Point -> GameState -> GameState
handleClick (mx, my) gs@GameState{..}
  | isInsideButton (mx, my) newMazeBtn =
      let (gen', gen'') = split gsGen
          newMaze = generateMaze gsMazeHeight gsMazeWidth gen'
      in gs { gsMaze = newMaze
            , gsPath = Nothing
            , gsPathIndex = 0
            , gsAnimating = False
            , gsGen = gen''
            , gsBreaksUsed = 0
            , gsJumpsUsed = 0
            }
  
  | isInsideButton (mx, my) solveBtn && not gsAnimating =
      case solveMaze gsMaze of
        Nothing -> gs
        Just path -> 
          let (breaks, jumps) = countAbilities path
          in gs { gsPath = Just path
                , gsPathIndex = 0
                , gsAnimating = True
                , gsBreaksUsed = breaks
                , gsJumpsUsed = jumps
                }
  
  | otherwise = gs

-- Count how many breaks and jumps were used
countAbilities :: [Move] -> (Int, Int)
countAbilities moves = foldl count (0, 0) moves
  where
    count (b, j) (Break _) = (b + 1, j)
    count (b, j) (Jump _ _) = (b, j + 1)
    count acc _ = acc

-- Check if point is inside button
isInsideButton :: Point -> Button -> Bool
isInsideButton (mx, my) Button{..} =
  mx >= btnX - btnWidth/2 && mx <= btnX + btnWidth/2 &&
  my >= btnY - btnHeight/2 && my <= btnY + btnHeight/2

-- ==================== ANIMATION UPDATE ====================
updateGame :: Float -> GameState -> GameState
updateGame _ gs@GameState{..}
  | gsAnimating && Just gsPathIndex < fmap length gsPath =
      let newIdx = gsPathIndex + 1
          done = case gsPath of
                   Just p -> newIdx >= length p
                   Nothing -> True
      in gs { gsPathIndex = newIdx
            , gsAnimating = not done
            }
  | otherwise = gs