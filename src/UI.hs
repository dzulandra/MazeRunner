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
windowWidth = 900
windowHeight = 700

mazeDisplayWidth, mazeDisplayHeight :: Float
mazeDisplayWidth = 600
mazeDisplayHeight = 500

cellSize :: Float
cellSize = 25

-- Button definitions (x, y, width, height, label)
data Button = Button 
  { btnX :: Float
  , btnY :: Float
  , btnWidth :: Float
  , btnHeight :: Float
  , btnLabel :: String
  } deriving (Show)

newMazeBtn :: Button
newMazeBtn = Button (-250) (-280) 200 50 "New Maze"

solveBtn :: Button
solveBtn = Button 50 (-280) 200 50 "Solve"

-- ==================== GAME STATE ====================
data GameState = GameState
  { gsMaze :: Maze
  , gsPath :: Maybe [Coord]
  , gsPathIndex :: Int  -- For animation: how much of path to show
  , gsAnimating :: Bool
  , gsGen :: StdGen
  , gsMazeWidth :: Int
  , gsMazeHeight :: Int
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
     }

-- ==================== RENDERING ====================
runUI :: IO ()
runUI = do
  let gen = mkStdGen 42  -- Initial seed
      window = InWindow "MazeBreaker - Interactive Solver" 
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
  ]

-- Draw the maze
drawMaze :: Maze -> Maybe [Coord] -> Int -> Picture
drawMaze maze mPath pathIdx = 
  let h = mazeHeight maze
      w = mazeWidth maze
      offsetX = -(fromIntegral w * cellSize) / 2
      offsetY = (fromIntegral h * cellSize) / 2 + 50
      
      -- Get visible path (animated portion)
      visiblePath = case mPath of
        Nothing -> []
        Just p -> take pathIdx p
      
      startPos = findTile Start maze
      goalPos = findTile Goal maze
      
      -- Draw all cells
      cells = [ drawCell maze (r, c) visiblePath startPos goalPos offsetX offsetY
              | r <- [0..h-1]
              , c <- [0..w-1]
              ]
  in pictures cells

-- Draw a single cell
drawCell :: Maze -> Coord -> [Coord] -> Maybe Coord -> Maybe Coord 
         -> Float -> Float -> Picture
drawCell maze pos@(r, c) path startPos goalPos offsetX offsetY =
  let x = offsetX + fromIntegral c * cellSize
      y = offsetY - fromIntegral r * cellSize
      tile = fromMaybe Wall (getTile maze pos)
      
      -- Determine color
      cellColor
        | Just pos == startPos = makeColorI 50 200 50 255   -- Green start
        | Just pos == goalPos  = makeColorI 200 50 50 255   -- Red goal
        | pos `elem` path      = makeColorI 100 150 255 255 -- Blue path
        | tile == Wall         = makeColorI 50 50 60 255    -- Dark wall
        | otherwise            = makeColorI 200 200 210 255 -- Light floor
      
      cell = translate x y $ color cellColor $ 
             rectangleSolid cellSize cellSize
      
      -- Border
      border = translate x y $ color (greyN 0.2) $ 
               rectangleWire cellSize cellSize
  in pictures [cell, border]

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
drawTitle = translate (-280) 320 $ scale 0.3 0.3 $ 
            color white $ text "MazeBreaker"

-- Draw instructions
drawInstructions :: GameState -> Picture
drawInstructions GameState{..} = pictures
  [ translate (-280) 280 $ scale 0.12 0.12 $ color (greyN 0.7) $ 
    text status
  , translate (-280) 250 $ scale 0.12 0.12 $ color (greyN 0.7) $ 
    text "Click buttons to interact"
  ]
  where
    status = case (gsPath, gsAnimating) of
      (Nothing, _) -> "Click 'Solve' to find the path"
      (Just p, True) -> "Solving... (" ++ show gsPathIndex ++ "/" ++ show (length p) ++ ")"
      (Just p, False) -> "Solved! Path length: " ++ show (length p)

-- ==================== EVENT HANDLING ====================
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) gs =
  handleClick mousePos gs
handleEvent _ gs = gs

-- Handle mouse clicks
handleClick :: Point -> GameState -> GameState
handleClick (mx, my) gs@GameState{..}
  -- New Maze button
  | isInsideButton (mx, my) newMazeBtn =
      let (gen', gen'') = split gsGen
          newMaze = generateMaze gsMazeHeight gsMazeWidth gen'
      in gs { gsMaze = newMaze
            , gsPath = Nothing
            , gsPathIndex = 0
            , gsAnimating = False
            , gsGen = gen''
            }
  
  -- Solve button
  | isInsideButton (mx, my) solveBtn && not gsAnimating =
      case solveMaze gsMaze of
        Nothing -> gs  -- No solution found
        Just path -> gs { gsPath = Just path
                        , gsPathIndex = 0
                        , gsAnimating = True
                        }
  
  | otherwise = gs

-- Check if point is inside button
isInsideButton :: Point -> Button -> Bool
isInsideButton (mx, my) Button{..} =
  mx >= btnX - btnWidth/2 && mx <= btnX + btnWidth/2 &&
  my >= btnY - btnHeight/2 && my <= btnY + btnHeight/2

-- ==================== ANIMATION UPDATE ====================
updateGame :: Float -> GameState -> GameState
updateGame _ gs@GameState{..}
  | gsAnimating && Just gsPathIndex < fmap length gsPath =
      -- Animate path drawing (advance by 1 cell per frame, adjust speed here)
      let newIdx = gsPathIndex + 1
          done = case gsPath of
                   Just p -> newIdx >= length p
                   Nothing -> True
      in gs { gsPathIndex = newIdx
            , gsAnimating = not done
            }
  | otherwise = gs