{-# LANGUAGE RecordWildCards #-}

module UI (runUI) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, mkStdGen, split)
import Core.Maze
import Core.Generator
import Core.Solver
import Data.Maybe (fromMaybe)

windowWidth, windowHeight :: Int
windowWidth = 1000
windowHeight = 750

cellSize :: Float
cellSize = 25

data Button = Button 
  { btnX :: Float
  , btnY :: Float
  , btnWidth :: Float
  , btnHeight :: Float
  , btnLabel :: String
  }

newMazeBtn :: Button
newMazeBtn = Button (-250) (-300) 200 50 "New Maze"

solveBtn :: Button
solveBtn = Button 50 (-300) 200 50 "Solve"

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
  , gsKeyCollected :: Bool
  }

instance Show GameState where
  show _ = "GameState{...}"

initialState :: StdGen -> GameState
initialState gen = 
  let w = 21
      h = 15
      maze = generateMaze h w gen
      (_, gen') = split gen
  in GameState maze Nothing 0 False gen' w h 0 0 False

runUI :: IO ()
runUI = do
  let gen = mkStdGen 42
      window = InWindow "MazeRunner - Gate System" (windowWidth, windowHeight) (100, 100)
      bgColor = makeColorI 30 30 40 255
  play window bgColor 30 (initialState gen) drawGame handleEvent updateGame

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

drawMaze :: Maze -> Maybe [Move] -> Int -> Picture
drawMaze maze mPath pathIdx = 
  let h = mazeHeight maze
      w = mazeWidth maze
      offsetX = -(fromIntegral w * cellSize) / 2
      offsetY = (fromIntegral h * cellSize) / 2 + 50
      
      visibleMoves = case mPath of
        Nothing -> []
        Just p -> take pathIdx p
      
      visiblePath = movesToCoords visibleMoves
      keyCollected = any isCollectKey visibleMoves
      
      startPos = findTile Start maze
      goalPos = findTile Goal maze
      keyPos = findTile Key maze
      
      cells = [ drawCell maze (r, c) visiblePath visibleMoves keyCollected startPos goalPos keyPos offsetX offsetY
              | r <- [0..h-1], c <- [0..w-1] ]
  in pictures cells
  where
    isCollectKey (CollectKey _) = True
    isCollectKey _ = False

movesToCoords :: [Move] -> [Coord]
movesToCoords = concatMap moveToCoords
  where
    moveToCoords (Walk pos) = [pos]
    moveToCoords (Break pos) = [pos]
    moveToCoords (Jump _ to) = [to]
    moveToCoords (CollectKey pos) = [pos]
    moveToCoords (PassGate pos) = [pos]

drawCell :: Maze -> Coord -> [Coord] -> [Move] -> Bool -> Maybe Coord -> Maybe Coord -> Maybe Coord
         -> Float -> Float -> Picture
drawCell maze pos@(r, c) path moves keyCollected startPos goalPos keyPos offsetX offsetY =
  let x = offsetX + fromIntegral c * cellSize
      y = offsetY - fromIntegral r * cellSize
      tile = fromMaybe Wall (getTile maze pos)
      
      wasBroken = any (\m -> case m of Break p -> p == pos; _ -> False) moves
      wasJumped = any (\m -> case m of Jump j _ -> j == pos; _ -> False) moves
      wasGatePassed = any (\m -> case m of PassGate p -> p == pos; _ -> False) moves
      
      cellColor
        | Just pos == startPos = makeColorI 50 200 50 255
        | Just pos == goalPos  = makeColorI 200 50 50 255
        | Just pos == keyPos && not keyCollected = makeColorI 255 215 0 255
        | Just pos == keyPos && keyCollected = makeColorI 100 100 100 255
        | pos `elem` path      = makeColorI 100 150 255 255
        | wasBroken            = makeColorI 150 150 150 255
        | wasJumped            = makeColorI 200 200 100 255
        | wasGatePassed        = makeColorI 180 255 180 255
        | tile == Wall         = makeColorI 50 50 60 255
        | tile == BreakableWall = makeColorI 139 69 19 255
        | tile == JumpableWall = makeColorI 255 165 0 255
        | tile == Gate         = makeColorI 128 0 128 255
        | otherwise            = makeColorI 200 200 210 255
      
      cell = translate x y $ color cellColor $ rectangleSolid cellSize cellSize
      border = translate x y $ color (greyN 0.2) $ rectangleWire cellSize cellSize
      
      marker = case tile of
        BreakableWall -> translate x y $ color white $ scale 0.1 0.1 $ text "B"
        JumpableWall -> translate x y $ color white $ scale 0.1 0.1 $ text "J"
        Gate -> translate x y $ color white $ scale 0.1 0.1 $ text "G"
        Key | not keyCollected -> translate x y $ color black $ scale 0.1 0.1 $ text "K"
        _ -> blank
  in pictures [cell, border, marker]

drawButton :: Button -> Color -> Color -> Picture
drawButton Button{..} bgCol textCol = pictures
  [ translate btnX btnY $ color bgCol $ rectangleSolid btnWidth btnHeight
  , translate btnX btnY $ color white $ rectangleWire btnWidth btnHeight
  , translate (btnX - 80) (btnY - 8) $ scale 0.15 0.15 $ color textCol $ text btnLabel
  ]

drawTitle :: Picture
drawTitle = translate (-320) 340 $ scale 0.3 0.3 $ color white $ text "MazeRunner: Gate System"

drawInstructions :: GameState -> Picture
drawInstructions GameState{..} = pictures
  [ translate (-320) 300 $ scale 0.12 0.12 $ color (greyN 0.7) $ text status
  , translate (-320) 270 $ scale 0.12 0.12 $ color (greyN 0.7) $ 
    text "Collect KEY (K) first, then pass GATES (G) to reach GOAL!"
  ]
  where
    status = case (gsPath, gsAnimating) of
      (Nothing, _) -> "Click 'Solve' to find the path"
      (Just p, True) -> "Solving... (" ++ show gsPathIndex ++ "/" ++ show (length p) ++ ")"
      (Just p, False) -> "Solved! Path length: " ++ show (length p)

drawLegend :: Picture
drawLegend = translate 280 280 $ pictures
  [ scale 0.12 0.12 $ color white $ text "Legend:"
  , translate 0 (-30) $ color (makeColorI 50 200 50 255) $ rectangleSolid 20 20
  , translate 30 (-35) $ scale 0.1 0.1 $ color white $ text "Start"
  , translate 0 (-60) $ color (makeColorI 200 50 50 255) $ rectangleSolid 20 20
  , translate 30 (-65) $ scale 0.1 0.1 $ color white $ text "Goal"
  , translate 0 (-90) $ color (makeColorI 255 215 0 255) $ rectangleSolid 20 20
  , translate 30 (-95) $ scale 0.1 0.1 $ color white $ text "Key"
  , translate 0 (-120) $ color (makeColorI 128 0 128 255) $ rectangleSolid 20 20
  , translate 30 (-125) $ scale 0.1 0.1 $ color white $ text "Gate"
  , translate 0 (-150) $ color (makeColorI 139 69 19 255) $ rectangleSolid 20 20
  , translate 30 (-155) $ scale 0.1 0.1 $ color white $ text "Breakable"
  , translate 0 (-180) $ color (makeColorI 255 165 0 255) $ rectangleSolid 20 20
  , translate 30 (-185) $ scale 0.1 0.1 $ color white $ text "Jumpable"
  ]

drawStats :: GameState -> Picture
drawStats GameState{..} = translate 280 50 $ pictures
  [ scale 0.12 0.12 $ color white $ text "Statistics:"
  , translate 0 (-30) $ scale 0.1 0.1 $ color (greyN 0.8) $ 
    text $ "Breaks used: " ++ show gsBreaksUsed ++ "/3"
  , translate 0 (-55) $ scale 0.1 0.1 $ color (greyN 0.8) $ 
    text $ "Jumps used: " ++ show gsJumpsUsed ++ "/2"
  , translate 0 (-80) $ scale 0.1 0.1 $ color (greyN 0.8) $ 
    text $ "Key: " ++ if gsKeyCollected then "Collected!" else "Not yet"
  ]

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) gs =
  handleClick mousePos gs
handleEvent _ gs = gs

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
            , gsKeyCollected = False
            }
  
  | isInsideButton (mx, my) solveBtn && not gsAnimating =
      case solveMaze gsMaze of
        Nothing -> gs
        Just path -> 
          let (breaks, jumps, hasKey) = countAbilities path
          in gs { gsPath = Just path
                , gsPathIndex = 0
                , gsAnimating = True
                , gsBreaksUsed = breaks
                , gsJumpsUsed = jumps
                , gsKeyCollected = hasKey
                }
  
  | otherwise = gs

countAbilities :: [Move] -> (Int, Int, Bool)
countAbilities moves = foldl count (0, 0, False) moves
  where
    count (b, j, k) (Break _) = (b + 1, j, k)
    count (b, j, k) (Jump _ _) = (b, j + 1, k)
    count (b, j, _) (CollectKey _) = (b, j, True)
    count acc _ = acc

isInsideButton :: Point -> Button -> Bool
isInsideButton (mx, my) Button{..} =
  mx >= btnX - btnWidth/2 && mx <= btnX + btnWidth/2 &&
  my >= btnY - btnHeight/2 && my <= btnY + btnHeight/2

updateGame :: Float -> GameState -> GameState
updateGame _ gs@GameState{..}
  | gsAnimating && Just gsPathIndex < fmap length gsPath =
      let newIdx = gsPathIndex + 1
          done = case gsPath of
                   Just p -> newIdx >= length p
                   Nothing -> True
      in gs { gsPathIndex = newIdx, gsAnimating = not done }
  | otherwise = gs