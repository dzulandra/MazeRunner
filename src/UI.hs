{-# LANGUAGE RecordWildCards #-}

module UI (runUI) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, mkStdGen, split)
import Core.Maze
import Core.Generator
import Core.Solver
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)

-- ==================== CONSTANTS ====================
windowWidth, windowHeight :: Int
windowWidth = 1200
windowHeight = 800

cellSize :: Float
cellSize = 20

-- Layout constants for autoscaling and panels
leftPanelWidth :: Float
leftPanelWidth = 320     -- matches your settings panel width (rectangleSolid 320 ...)

rightMargin :: Float
rightMargin = 200        -- reserve space on the right for legend/stats

-- The panel translation used for settings (single source of truth)
panelOffsetX, panelOffsetY :: Float
panelOffsetX = (-fromIntegral windowWidth / 2) + 80
panelOffsetY = 50

-- Button definitions
data Button = Button 
  { btnX :: Float
  , btnY :: Float
  , btnWidth :: Float
  , btnHeight :: Float
  , btnLabel :: String
  }

newMazeBtn :: Button
newMazeBtn = Button (-350) (-350) 200 50 "New Maze"

solveBtn :: Button
solveBtn = Button (-100) (-350) 200 50 "Solve"

-- Input field definitions (positions are PANEL-LOCAL coordinates now)
data InputField = InputField
  { fieldX :: Float       -- local to panel origin
  , fieldY :: Float       -- local to panel origin
  , fieldWidth :: Float
  , fieldHeight :: Float
  , fieldLabel :: String
  , fieldId :: String
  }

-- Settings input fields: X is small because they're relative to the panel translate
widthField :: InputField
widthField = InputField 0 150 160 40 "Width:" "width"

heightField :: InputField
heightField = InputField 0 100 160 40 "Height:" "height"

breakablePercentField :: InputField
breakablePercentField = InputField 0 40 160 40 "Break %:" "breakPercent"

jumpablePercentField :: InputField
jumpablePercentField = InputField 0 (-20) 160 40 "Jump %:" "jumpPercent"

maxBreaksField :: InputField
maxBreaksField = InputField 0 (-80) 160 40 "Max Breaks:" "maxBreaks"

maxJumpsField :: InputField
maxJumpsField = InputField 0 (-140) 160 40 "Max Jumps:" "maxJumps"

allInputFields :: [InputField]
allInputFields = [widthField, heightField, breakablePercentField, 
                  jumpablePercentField, maxBreaksField, maxJumpsField]

-- ==================== GAME STATE ====================
data MazeSettings = MazeSettings
  { settingWidth :: Int
  , settingHeight :: Int
  , settingBreakablePercent :: Int
  , settingJumpablePercent :: Int
  , settingMaxBreaks :: Int
  , settingMaxJumps :: Int
  } deriving (Show)

defaultSettings :: MazeSettings
defaultSettings = MazeSettings 21 15 10 8 3 2

data GameState = GameState
  { gsMaze :: Maze
  , gsPath :: Maybe [Move]
  , gsPathIndex :: Int
  , gsAnimating :: Bool
  , gsGen :: StdGen
  , gsSettings :: MazeSettings
  , gsInputValues :: [(String, String)]  -- Field ID -> Value
  , gsActiveField :: Maybe String
  , gsBreaksUsed :: Int
  , gsJumpsUsed :: Int
  , gsKeyCollected :: Bool
  }

instance Show GameState where
  show _ = "GameState{...}"

initialState :: StdGen -> GameState
initialState gen = 
  let settings = defaultSettings
      maze = generateMazeWithSettings settings gen
      (_, gen') = split gen
  in GameState
     { gsMaze = maze
     , gsPath = Nothing
     , gsPathIndex = 0
     , gsAnimating = False
     , gsGen = gen'
     , gsSettings = settings
     , gsInputValues = []
     , gsActiveField = Nothing
     , gsBreaksUsed = 0
     , gsJumpsUsed = 0
     , gsKeyCollected = False
     }

-- Generate maze with custom settings
generateMazeWithSettings :: MazeSettings -> StdGen -> Maze
generateMazeWithSettings MazeSettings{..} gen =
  let config = MazeConfig settingBreakablePercent settingJumpablePercent
  in generateMaze settingHeight settingWidth config gen

runUI :: IO ()
runUI = do
  let gen = mkStdGen 42
      window = InWindow "MazeBreaker - Configurable Settings" (windowWidth, windowHeight) (50, 50)
      bgColor = makeColorI 30 30 40 255
  play window bgColor 30 (initialState gen) drawGame handleEvent updateGame

-- ==================== RENDERING ====================
-- Replaced drawGame to draw maze with autoscale so it never overlaps left panel
drawGame :: GameState -> Picture
drawGame gs@GameState{..} = pictures
  [ drawMazeAutoScaled gs
  , drawButton newMazeBtn (greyN 0.3) white
  , drawButton solveBtn (greyN 0.3) white
  , drawTitle
  , drawInstructions gs
  , drawLegend
  , drawStats gs
  , drawSettingsPanel gs
  ]

-- Auto-scale helper: compute scale factor so maze fits inside available area
calcMazeScale :: Int -> Int -> Float -> Float -> Float -> Float
calcMazeScale mazeW mazeH cell maxW maxH =
    let mazePixelW = fromIntegral mazeW * cell
        mazePixelH = fromIntegral mazeH * cell
        scaleW = maxW / mazePixelW
        scaleH = maxH / mazePixelH
        scaleFactor = min scaleW scaleH
    in min 1 scaleFactor            -- never zoom in above 100%

-- Draw maze using autoscaling and centering in the available region (to right of left panel)
drawMazeAutoScaled :: GameState -> Picture
drawMazeAutoScaled gs@GameState{..} =
  let w = mazeWidth gsMaze
      h = mazeHeight gsMaze
      cell = cellSize

      -- available drawable area (screen minus left panel and right margin)
      availW = fromIntegral windowWidth - leftPanelWidth - rightMargin
      availH = fromIntegral windowHeight - 80  -- small vertical margin

      scaleVal = calcMazeScale w h cell availW availH

      -- unscaled maze pixel dims
      mazePixelW = fromIntegral w * cell
      mazePixelH = fromIntegral h * cell

      -- compute center of the available region in global (window) coords
      leftEdgeX = - (fromIntegral windowWidth / 2)
      availLeftX = leftEdgeX + leftPanelWidth
      availCenterX = availLeftX + (availW / 2)
      availCenterY = 0  -- center vertically

      -- we want the maze's own center to be placed at availCenter
      -- drawMaze expects to draw centered at origin (it uses offsets internally),
      -- so translate to availCenter then scale then call drawMaze.
      translateX = availCenterX
      translateY = availCenterY
  in translate translateX translateY $
     scale scaleVal scaleVal $
     drawMaze gsMaze gsPath gsPathIndex

-- original drawMaze signature preserved (draws maze centered around origin)
drawMaze :: Maze -> Maybe [Move] -> Int -> Picture
drawMaze maze mPath pathIdx = 
  let h = mazeHeight maze
      w = mazeWidth maze
      offsetX = fromIntegral w * cellSize / 2
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
  let x = -offsetX + fromIntegral c * cellSize
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
        BreakableWall -> translate x y $ color white $ scale 0.08 0.08 $ text "B"
        JumpableWall -> translate x y $ color white $ scale 0.08 0.08 $ text "J"
        Gate -> translate x y $ color white $ scale 0.08 0.08 $ text "G"
        Key | not keyCollected -> translate x y $ color black $ scale 0.08 0.08 $ text "K"
        _ -> blank
  in pictures [cell, border, marker]

-- ==================== SETTINGS PANEL & INPUTS ====================
-- Draw settings panel (single panel translation), inputs are PANEL-LOCAL
drawSettingsPanel :: GameState -> Picture
drawSettingsPanel gs = translate panelOffsetX panelOffsetY $ pictures
  [ drawSettingsBackground
  , drawSettingsTitle
  , drawAllInputFields gs
  , drawDefaultsNote
  ]

drawSettingsBackground :: Picture
drawSettingsBackground =
  color (makeColorI 60 60 75 255) $ rectangleSolid 320 600

drawSettingsTitle :: Picture
drawSettingsTitle = translate 0 280 $ scale 0.18 0.18 $ color white $ text "Settings"

drawAllInputFields :: GameState -> Picture
drawAllInputFields gs = pictures $ map (drawInputField gs) allInputFields

-- drawInputField now assumes FIELD coords are relative to the panel origin
-- Improved visuals: larger text, left padding, and caret display while active.
drawInputField :: GameState -> InputField -> Picture
drawInputField GameState{..} InputField{..} =
  let isActive = Just fieldId == gsActiveField
      currentValue = lookup fieldId gsInputValues
      baseValue = case currentValue of
                    Just v -> v
                    Nothing -> getDefaultValue fieldId gsSettings

      -- Show empty string if user cleared it explicitly (we treat Just "" as empty)
      displayValueRaw = baseValue

      -- Add a caret when active
      caret = if isActive then "|" else ""
      displayValue = displayValueRaw ++ caret

      -- Updated colors: more contrast and active highlight
      borderColor = if isActive
                    then makeColorI 150 185 255 255   -- lighter blue border when active
                    else makeColorI 200 200 220 255   -- light grey border when inactive

      bgColor = if isActive
                then makeColorI 80 80 110 255     -- slightly darker when active
                else makeColorI 95 95 120 255     -- normal input background

      -- label & input layout
      labelOffsetX = -120
      inputOffsetX = 14
      leftPadding = 8
      textScale = 0.16
      maxChars = 5  -- limit to avoid overflow

      -- trim display string to maxChars (excluding caret)
      trimmed =
        let trimmedRaw = take maxChars displayValueRaw
        in if isActive then trimmedRaw ++ "|" else trimmedRaw

      -- compute text x so text starts inside the input with padding
      textX = inputOffsetX - (fieldWidth / 2) + leftPadding

  in translate fieldX fieldY $ pictures
    [ -- Label (brightened, vertically centered)
      translate labelOffsetX (-5) $
        scale 0.13 0.13 $
        color (makeColorI 220 220 230 255) $
        text fieldLabel

      -- Input background box (centered)
    , translate inputOffsetX 0 $ color bgColor $ rectangleSolid fieldWidth fieldHeight

      -- Border
    , translate inputOffsetX 0 $ color borderColor $ rectangleWire fieldWidth fieldHeight

      -- Value text (left-aligned inside the box)
    , translate textX (-7) $
        scale textScale textScale $
        color white $
        text trimmed
    ]

-- convert mouse coords (global) to panel-local coords and test with field bounds
isInsideInputField :: Point -> InputField -> Bool
isInsideInputField (mx, my) InputField{..} =
  let localX = mx - panelOffsetX     -- convert to panel-local coords
      localY = my - panelOffsetY
      -- input box is centered at (fieldX, fieldY) with width/height
      left   = fieldX - (fieldWidth / 2)
      right  = fieldX + (fieldWidth / 2)
      top    = fieldY + (fieldHeight / 2)
      bottom = fieldY - (fieldHeight / 2)
  in localX >= left && localX <= right && localY >= bottom && localY <= top

-- findClickedField uses isInsideInputField which expects panel-local coords
findClickedField :: Point -> Maybe String
findClickedField (mx, my) =
  let matches = filter (\f -> isInsideInputField (mx, my) f) allInputFields
  in case matches of
       (field:_) -> Just (fieldId field)
       [] -> Nothing

getDefaultValue :: String -> MazeSettings -> String
getDefaultValue "width" s = show (settingWidth s)
getDefaultValue "height" s = show (settingHeight s)
getDefaultValue "breakPercent" s = show (settingBreakablePercent s)
getDefaultValue "jumpPercent" s = show (settingJumpablePercent s)
getDefaultValue "maxBreaks" s = show (settingMaxBreaks s)
getDefaultValue "maxJumps" s = show (settingMaxJumps s)
getDefaultValue _ _ = ""

drawDefaultsNote :: Picture
drawDefaultsNote =
  translate 0 (-260) $
    scale 0.11 0.11 $
      color (makeColorI 200 200 210 255) $
        text "Click > Type > Enter"

-- ==================== BUTTONS & OTHER UI ====================
drawButton :: Button -> Color -> Color -> Picture
drawButton Button{..} bgCol textCol = pictures
  [ translate btnX btnY $ color bgCol $ rectangleSolid btnWidth btnHeight
  , translate btnX btnY $ color white $ rectangleWire btnWidth btnHeight
  , translate (btnX - 80) (btnY - 8) $ scale 0.15 0.15 $ color textCol $ text btnLabel
  ]

drawTitle :: Picture
drawTitle = translate (-400) 370 $ scale 0.25 0.25 $ color white $ text "MazeBreaker: Configurable"

drawInstructions :: GameState -> Picture
drawInstructions GameState{..} = pictures
  [ translate 100 370 $ scale 0.1 0.1 $ color (greyN 0.7) $ text status
  , translate 100 350 $ scale 0.1 0.1 $ color (greyN 0.7) $ 
    text "Collect KEY first, then pass GATES to reach GOAL"
  ]
  where
    status = case (gsPath, gsAnimating) of
      (Nothing, _) -> "Click 'Solve' to find path"
      (Just p, True) -> "Solving... " ++ show gsPathIndex ++ "/" ++ show (length p)
      (Just p, False) -> "Solved! Length: " ++ show (length p)

drawLegend :: Picture
drawLegend = translate 480 280 $ pictures
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
drawStats GameState{..} = translate 480 50 $ pictures
  [ scale 0.12 0.12 $ color white $ text "Statistics:"
  , translate 0 (-30) $ scale 0.1 0.1 $ color (greyN 0.8) $ 
    text $ "Breaks: " ++ show gsBreaksUsed ++ "/" ++ show (settingMaxBreaks gsSettings)
  , translate 0 (-55) $ scale 0.1 0.1 $ color (greyN 0.8) $ 
    text $ "Jumps: " ++ show gsJumpsUsed ++ "/" ++ show (settingMaxJumps gsSettings)
  , translate 0 (-80) $ scale 0.1 0.1 $ color (greyN 0.8) $ 
    text $ "Key: " ++ if gsKeyCollected then "Collected!" else "Not yet"
  ]

-- ==================== EVENT HANDLING (INPUT FIXES) ====================
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) gs =
  handleClick mousePos gs
handleEvent (EventKey (Char c) Down _ _) gs =
  handleKeyPress c gs
handleEvent (EventKey (SpecialKey KeyBackspace) Down _ _) gs =
  handleBackspace gs
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) gs =
  applySettings gs
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) gs =
  gs { gsActiveField = Nothing }
handleEvent _ gs = gs

-- handleClick: when clicking a field we set it active and clear its value
-- if it is a different field than the currently active one.
handleClick :: Point -> GameState -> GameState
handleClick (mx, my) gs@GameState{..}
  | isInsideButton (mx, my) newMazeBtn =
      let settings = parseSettings gs
          (gen', gen'') = split gsGen
          newMaze = generateMazeWithSettings settings gen'
      in gs { gsMaze = newMaze
            , gsPath = Nothing
            , gsPathIndex = 0
            , gsAnimating = False
            , gsGen = gen''
            , gsSettings = settings
            , gsBreaksUsed = 0
            , gsJumpsUsed = 0
            , gsKeyCollected = False
            }
  
  | isInsideButton (mx, my) solveBtn && not gsAnimating =
      let solverConfig = SolverConfig (settingMaxBreaks gsSettings) (settingMaxJumps gsSettings)
      in case solveMaze gsMaze solverConfig of
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
  
  | otherwise = 
      -- Check if clicked on input field (global coords)
      let clickedField = findClickedField (mx, my)
      in case clickedField of
           Nothing -> gs { gsActiveField = Nothing }
           Just fid ->
             if Just fid == gsActiveField
             then gs -- already active, keep editing
             else
               -- Activate new field and clear its typed content to allow fresh typing
               let newInputVals = updateList fid "" gsInputValues
               in gs { gsActiveField = Just fid, gsInputValues = newInputVals }

-- Accept only digit characters and append them (with a max length safeguard)
handleKeyPress :: Char -> GameState -> GameState
handleKeyPress c gs@GameState{..} =
  case gsActiveField of
    Nothing -> gs
    Just fieldId ->
      if isDigit c
      then
        let currentValue = lookup fieldId gsInputValues
            base = case currentValue of
                     Just v -> v
                     Nothing -> ""   -- if nothing stored, start fresh
            maxLen = 5
            newVal = if length base < maxLen then base ++ [c] else base
            updatedValues = updateList fieldId newVal gsInputValues
        in gs { gsInputValues = updatedValues }
      else gs

handleBackspace :: GameState -> GameState
handleBackspace gs@GameState{..} =
  case gsActiveField of
    Nothing -> gs
    Just fieldId ->
      let currentValue = lookup fieldId gsInputValues
          newValue = case currentValue of
                      Just v -> if null v then "" else init v
                      Nothing -> ""
          updatedValues = updateList fieldId newValue gsInputValues
      in gs { gsInputValues = updatedValues }

updateList :: String -> String -> [(String, String)] -> [(String, String)]
updateList key val list =
  case lookup key list of
    Just _ -> map (\(k, v) -> if k == key then (key, val) else (k, v)) list
    Nothing -> (key, val) : list

applySettings :: GameState -> GameState
applySettings gs@GameState{..} = 
  let newSettings = parseSettings gs
      (gen', gen'') = split gsGen
      newMaze = generateMazeWithSettings newSettings gen'
  in gs { gsSettings = newSettings
        , gsMaze = newMaze
        , gsPath = Nothing
        , gsPathIndex = 0
        , gsAnimating = False
        , gsGen = gen''
        , gsBreaksUsed = 0
        , gsJumpsUsed = 0
        , gsKeyCollected = False
        , gsActiveField = Nothing
        }

parseSettings :: GameState -> MazeSettings
parseSettings GameState{..} =
  let width = parseField "width" (settingWidth gsSettings)
      height = parseField "height" (settingHeight gsSettings)
      breakPercent = parseField "breakPercent" (settingBreakablePercent gsSettings)
      jumpPercent = parseField "jumpPercent" (settingJumpablePercent gsSettings)
      maxBreaks = parseField "maxBreaks" (settingMaxBreaks gsSettings)
      maxJumps = parseField "maxJumps" (settingMaxJumps gsSettings)
  in MazeSettings width height breakPercent jumpPercent maxBreaks maxJumps
  where
    parseField fieldId defaultVal =
      case lookup fieldId gsInputValues of
        Just "" -> defaultVal
        Just v -> read v :: Int
        Nothing -> defaultVal

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
