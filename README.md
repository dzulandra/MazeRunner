# 🎮 MazeRunner: Functional Maze Solver

An interactive maze puzzle game demonstrating **functional programming concepts** through procedural maze generation and A* pathfinding. Watch an AI solve randomly generated mazes using pure functional code.

![Haskell](https://img.shields.io/badge/Language-Haskell-5e5086?style=flat-square)
![Status](https://img.shields.io/badge/Status-Complete-success?style=flat-square)
![FP](https://img.shields.io/badge/Paradigm-Functional-blue?style=flat-square)

---

## 📖 Table of Contents

- [What is This Project?](#what-is-this-project)
- [For Programmers New to Functional Programming](#for-programmers-new-to-functional-programming)
- [Quick Start](#quick-start)
- [Game Features](#game-features)
- [Functional Programming Concepts Demonstrated](#functional-programming-concepts-demonstrated)
- [Project Architecture](#project-architecture)
---

## 🎯 What is This Project?

MazeBreaker is a **visual demonstration** of functional programming principles applied to:
- **Procedural maze generation** (recursive backtracking)
- **Pathfinding algorithms** (A* with resource constraints)
- **Game state management** (immutable data structures)
- **Interactive UI** (event-driven functional architecture)

**Core Features:**
- 🎲 Generates unique, guaranteed-solvable mazes
- 🔑 Gate system requiring key collection
- 🧱 Breakable/jumpable walls with limited uses
- 🤖 A* pathfinding with heuristics
- ⚙️ Configurable settings (size, difficulty, resources)

---

## 👨‍💻 For Programmers New to Functional Programming

If you know **Java, Python, C++, or JavaScript** but haven't used functional languages, this project is perfect for learning!

### What Makes This "Functional"?

Unlike imperative programming where you tell the computer **HOW** to do something step-by-step:

**Imperative (Java/Python):**
```java
// Modify state in place
int[] maze = new int[10][10];
for (int i = 0; i < 10; i++) {
    maze[i][0] = WALL;  // Mutate array
}
```

**Functional (Haskell):**
```haskell
-- Describe WHAT you want, not HOW to do it
maze = replicate 10 (replicate 10 Wall)
-- Creates new structure, never modifies existing
```

### Key Differences You'll See:

| Concept | Imperative | Functional (This Project) |
|---------|-----------|---------------------------|
| **Variables** | Can change | Never change (immutable) |
| **Loops** | `for`, `while` | Recursion, `map`, `filter`, `fold` |
| **State** | Mutable objects | New copies with changes |
| **Functions** | May have side effects | Pure (same input = same output) |
| **Data Flow** | Hidden in mutations | Explicit transformations |

---

## 🚀 Quick Start

### Prerequisites

**Install Haskell:**
- Windows/Mac/Linux: https://www.haskell.org/ghcup/
- Follow installer prompts (similar to installing Python or Node.js)

**Install Graphics Dependencies:**

**Windows:**
```bash
# Download freeglut from https://www.transmissionzero.co.uk/software/freeglut-devel/
# Copy freeglut.dll to project folder
```

**Mac:**
```bash
brew install freeglut
```

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get install freeglut3 freeglut3-dev libglu1-mesa-dev
```

### Build & Run

```bash
# Clone repository
git clone <repository-url>
cd mazebreaker

# Build project (like 'npm install' or 'mvn compile')
cabal build

# Run game
cabal run
```

---

## 🎮 Game Features

### Gate System
- **Key (K)**: Collect to unlock gates
- **Gates (G)**: Purple barriers blocking goal
- **Goal (G)**: Reach after collecting key

### Special Abilities
- **Breakable Walls (B)**: Smash through (default: 3 max)
- **Jumpable Walls (J)**: Jump over (default: 2 max)

### Configurable Settings
- Maze size (width × height)
- Special wall percentages
- Maximum breaks/jumps allowed

### Guaranteed Solvability
Every maze is mathematically proven solvable through:
1. Connected graph generation (DFS with backtracking)
2. Key reachability verification (BFS pathfinding)
3. Automatic gate removal if blocking

---

## 🧠 Functional Programming Concepts Demonstrated

### 1️⃣ Immutability

**Concept:** Data never changes after creation. Instead, create new versions with modifications.

**Where We Use It:** Everywhere! The entire maze is immutable.

**Code Example:**
```haskell
-- In Maze.hs
setTile :: Maze -> Coord -> Tile -> Maze
setTile maze (r, c) newTile =
  take r maze ++                              -- Keep rows before
  [take c (maze !! r) ++ [newTile] ++ drop (c + 1) (maze !! r)] ++ -- New row
  drop (r + 1) maze                           -- Keep rows after
-- Returns NEW maze, original unchanged
```

**Comparison:**

**Imperative (Mutation):**
```java
// Java - Modify in place
void setTile(int[][] maze, int r, int c, int tile) {
    maze[r][c] = tile;  // Changes original array
}
// Risk: Other code might be using maze, now it's different!
```

**Functional (Immutability):**
```haskell
-- Haskell - Create new version
setTile maze (r, c) newTile = ...
-- Returns: New maze with change
-- Original maze still exists, unchanged
-- Benefit: No unexpected side effects, safe to use anywhere
```

**Real Example in Project:**
```haskell
-- In Generator.hs, carving the maze
carved = carveMaze emptyMaze startPos gen
withGates = placeGates carved start key goal gen
withSpecial = addSpecialWalls withGates gen
-- Each step creates NEW maze, previous versions still exist
-- Easy to debug: can inspect 'carved', 'withGates', 'withSpecial'
```

**Why This Matters:**
- ✅ No bugs from unexpected mutations
- ✅ Easy to debug (inspect any intermediate state)
- ✅ Safe parallelization (no race conditions)
- ✅ Time-travel debugging (keep old states)

---

### 2️⃣ Higher-Order Functions

**Concept:** Functions that take other functions as arguments or return functions.

**Where We Use It:** `map`, `filter`, `foldl` throughout the codebase.

**Code Example:**
```haskell
-- In Generator.hs
findMeaningfulWalls :: Maze -> [Coord]
findMeaningfulWalls maze =
  [ (r, c)
  | r <- [2 .. mazeHeight maze - 3]
  , c <- [2 .. mazeWidth maze - 3]
  , getTile maze (r, c) == Just Wall        -- Filter condition
  , hasPerpedicularEmptyNeighbors maze (r, c)
  , not (isIsolatedWall maze (r, c))
  ]
-- This is syntactic sugar for filter + map
```

**Comparison:**

**Imperative (Explicit Loops):**
```python
# Python - Manual iteration
def find_meaningful_walls(maze):
    result = []
    for r in range(2, len(maze) - 3):
        for c in range(2, len(maze[0]) - 3):
            if (get_tile(maze, r, c) == WALL and
                has_perpendicular_neighbors(maze, r, c) and
                not is_isolated(maze, r, c)):
                result.append((r, c))
    return result
# Have to manually: create list, iterate, check, append
```

**Functional (Declarative):**
```haskell
-- Haskell - Describe what you want
findMeaningfulWalls maze = 
  filter isValidWall allPositions
  where
    allPositions = [(r,c) | r <- [2..h-3], c <- [2..w-3]]
    isValidWall (r,c) = 
      getTile maze (r,c) == Just Wall &&
      hasPerpedicularEmptyNeighbors maze (r,c) &&
      not (isIsolatedWall maze (r,c))
-- Describe WHAT, not HOW
```

**Real Example: `foldl` (Fold Left)**
```haskell
-- In Generator.hs - Processing multiple directions
(finalMaze, _) = foldl processDir (maze', gen') shuffledDirs

-- What foldl does:
-- foldl f initialValue [a, b, c, d]
--   = f (f (f (f initialValue a) b) c) d

-- In our case:
-- Start with: (maze', gen')
-- Process direction1: processDir (maze', gen') dir1 -> (maze1, gen1)
-- Process direction2: processDir (maze1, gen1) dir2 -> (maze2, gen2)
-- Process direction3: processDir (maze2, gen2) dir3 -> (maze3, gen3)
-- Process direction4: processDir (maze3, gen3) dir4 -> (maze4, gen4)
-- Result: (maze4, gen4)
```

**Imperative Equivalent:**
```java
// Java - Manual loop with accumulator
Pair<Maze, Generator> result = new Pair<>(maze, gen);
for (Direction dir : shuffledDirections) {
    result = processDirection(result.maze, result.gen, dir);
}
return result;
// More verbose, explicit loop management
```

**Why This Matters:**
- ✅ No loop boilerplate code
- ✅ Reusable patterns (`map`, `filter`, `fold`)
- ✅ Composable (chain operations easily)
- ✅ Clear intent (express WHAT not HOW)

---

### 3️⃣ Pure Functions

**Concept:** Functions with no side effects. Same input always produces same output.

**Where We Use It:** Almost every function in the project.

**Code Example:**
```haskell
-- In Solver.hs
manhattan :: Coord -> Coord -> Int
manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

-- Pure function:
-- - No I/O
-- - No random numbers
-- - No global state
-- - No mutations
-- - Same inputs ALWAYS give same output
```

**Comparison:**

**Impure Function (Side Effects):**
```python
# Python - Has side effects
total_distance = 0  # Global state

def manhattan(p1, p2):
    global total_distance
    dist = abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])
    total_distance += dist  # Side effect: modifies global
    print(f"Distance: {dist}")  # Side effect: I/O
    return dist

# Problems:
# - Different results if called multiple times (total_distance changes)
# - Can't test in isolation (depends on global state)
# - Order of calls matters
```

**Pure Function (No Side Effects):**
```haskell
-- Haskell - Pure
manhattan :: Coord -> Coord -> Int
manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

-- Benefits:
-- manhattan (1, 2) (4, 6) ALWAYS returns 7
-- Can call in any order
-- Easy to test: assertEquals(7, manhattan((1,2), (4,6)))
-- Compiler can optimize (memoize, parallelize)
```

**Real Example in Project:**
```haskell
-- In Generator.hs - Every step is pure
carved = carveMaze emptyMaze startPos gen1     -- Pure: deterministic with same gen1
keyPos = findMiddlePosition carved startPos    -- Pure: same maze = same result
goalPos = findFarthestFrom carved keyPos       -- Pure: same inputs = same output

-- All testable without mocking or setup:
-- assertEqual expectedMaze (carveMaze initialMaze start fixedGen)
```

**Why This Matters:**
- ✅ Easy to test (no mocking needed)
- ✅ Easy to debug (no hidden state)
- ✅ Parallelizable (no race conditions)
- ✅ Cacheable (memoization)
- ✅ Predictable (deterministic)

---

### 4️⃣ Algebraic Data Types (ADTs)

**Concept:** Types that precisely model your domain, making invalid states impossible.

**Where We Use It:** Tile types, Move types, Solver state.

**Code Example:**
```haskell
-- In Maze.hs
data Tile
  = Empty
  | Wall
  | Start
  | Goal
  | BreakableWall
  | JumpableWall
  | Gate
  | Key
  deriving (Eq, Show)

-- In Solver.hs
data Move 
  = Walk Coord
  | Break Coord
  | Jump Coord Coord    -- Requires TWO coordinates
  | CollectKey Coord
  | PassGate Coord
  deriving (Eq, Show)
```

**Comparison:**

**Without ADTs (Stringly-Typed):**
```python
# Python - Using strings/dictionaries (error-prone)
move = {
    "type": "jump",
    "to": (5, 7)
    # BUG: Forgot "from" coordinate!
}

def execute_move(move):
    if move["type"] == "jump":
        from_pos = move["from"]  # KeyError at runtime!
        to_pos = move["to"]
        # ...
```

**With ADTs (Type-Safe):**
```haskell
-- Haskell - Impossible to create invalid move
move = Jump (3, 4) (5, 7)  -- Compiler enforces both coordinates

-- This won't compile:
badMove = Jump (5, 7)  -- ERROR: Jump needs 2 coordinates

-- Pattern matching ensures all cases handled:
executeMove :: Move -> IO ()
executeMove (Walk pos) = ...
executeMove (Break pos) = ...
executeMove (Jump from to) = ...  -- Compiler ensures we handle both coords
executeMove (CollectKey pos) = ...
executeMove (PassGate pos) = ...
-- Forgot a case? Compiler error!
```

**Real Example in Project:**
```haskell
-- In Solver.hs - Pattern matching on moves
movesToCoords :: [Move] -> [Coord]
movesToCoords = concatMap moveToCoords
  where
    moveToCoords (Walk pos) = [pos]
    moveToCoords (Break pos) = [pos]
    moveToCoords (Jump _ to) = [to]      -- Compiler knows Jump has 2 coords
    moveToCoords (CollectKey pos) = [pos]
    moveToCoords (PassGate pos) = [pos]

-- If we add new move type, compiler shows error here:
-- "Pattern match not exhaustive"
```

**Why This Matters:**
- ✅ Impossible to create invalid data
- ✅ Compiler catches bugs at compile-time
- ✅ Refactoring is safe (compiler finds all uses)
- ✅ Self-documenting code
- ✅ Pattern matching guarantees completeness

---

### 5️⃣ Recursion Instead of Loops

**Concept:** Functions call themselves instead of using `for`/`while` loops.

**Where We Use It:** Maze carving, pathfinding, tree traversal.

**Code Example:**
```haskell
-- In Generator.hs - Recursive maze carving
carveMaze :: Maze -> Coord -> StdGen -> Maze
carveMaze maze pos gen =
  let maze' = setTile maze pos Empty
      (shuffledDirs, gen') = shuffle gen directions
      (finalMaze, _) = foldl processDir (maze', gen') shuffledDirs
  in finalMaze
  where
    processDir (m, g) dir =
      if isValidCarve m next
      then let m3 = carveMaze m2 next g1  -- RECURSIVE CALL
           in (m3, g2)
      else (m, g2)
```

**Comparison:**

**Imperative (Loop with Stack):**
```java
// Java - Manual stack management
void carveMaze(Maze maze, Coord start) {
    Stack<Coord> stack = new Stack<>();
    stack.push(start);
    
    while (!stack.isEmpty()) {
        Coord current = stack.pop();
        maze.setTile(current, EMPTY);  // Mutation
        
        for (Direction dir : shuffleDirections()) {
            Coord next = move2(current, dir);
            if (isValidCarve(maze, next)) {
                stack.push(next);  // Manual stack management
            }
        }
    }
}
// Manual: loop, stack, mutation
```

**Functional (Recursive):**
```haskell
-- Haskell - Recursion handles backtracking
carveMaze maze pos gen =
  let maze' = setTile maze pos Empty       -- Mark current
      (shuffledDirs, gen') = shuffle gen directions
      (finalMaze, _) = foldl processDir (maze', gen') shuffledDirs
  in finalMaze
  where
    processDir (m, g) dir =
      if isValidCarve m next
      then 
        let m3 = carveMaze m2 next g1      -- Recurse (automatic backtrack)
        in (m3, g2)
      else (m, g2)
-- Automatic: recursion, call stack, immutability
```

**Call Stack Visualization:**
```
carveMaze at (1,1)
  ├─ Try RIGHT → valid
  │  └─ carveMaze at (1,3)
  │     ├─ Try DOWN → valid
  │     │  └─ carveMaze at (3,3)
  │     │     ├─ Try LEFT → invalid
  │     │     ├─ Try RIGHT → invalid
  │     │     └─ Return (backtrack)
  │     └─ Try RIGHT → valid
  │        └─ carveMaze at (1,5)
  │           └─ ...
  └─ Try DOWN → valid
     └─ carveMaze at (3,1)
        └─ ...
```

**Real Example: Finding Tiles**
```haskell
-- In Maze.hs - Recursive search
findTile :: Tile -> Maze -> Maybe Coord
findTile t maze = go 0 maze
  where
    go _ [] = Nothing                    -- Base case: empty
    go r (row:rs) =                      -- Recursive case
      case lookupCol 0 row of
        Just c  -> Just (r, c)           -- Found it!
        Nothing -> go (r + 1) rs         -- Recurse to next row
```

**Why This Matters:**
- ✅ Natural expression of algorithms
- ✅ No manual loop counter management
- ✅ Automatic backtracking (call stack)
- ✅ Compiler optimizations (tail-call)
- ✅ Matches mathematical definitions

---

### 6️⃣ Type-Driven Development

**Concept:** Design types first, then write functions that work with those types. Compiler guides implementation.

**Where We Use It:** Solver state, maze structure, move types.

**Code Example:**
```haskell
-- In Solver.hs - State tracks everything
data SolverState = SolverState
  { ssPosition :: Coord       -- Where we are
  , ssBreaksLeft :: Int       -- Breaks remaining
  , ssJumpsLeft :: Int        -- Jumps remaining
  , ssHasKey :: Bool          -- Have we collected key?
  , ssPath :: [Move]          -- How we got here
  } deriving (Eq, Show)
```

**Comparison:**

**Without Type Safety:**
```python
# Python - Dictionary (runtime errors)
state = {
    "position": (5, 7),
    "breaks": 3,
    # BUG: Forgot "jumps" and "hasKey"!
}

def process_state(state):
    jumps = state["jumps"]  # KeyError at runtime!
    # ...
```

**With Type Safety:**
```haskell
-- Haskell - Compiler enforces all fields
state = SolverState (5, 7) 3 2 False []

-- Can't create incomplete state:
badState = SolverState (5, 7) 3  -- ERROR: Missing fields

-- Accessing fields is safe:
processState :: SolverState -> Int
processState state = ssJumpsLeft state  -- Guaranteed to exist

-- Adding new field? Compiler shows all places to update:
-- "Fields of SolverState not initialized: ssNewField"
```

**Real Example: Type-Driven Refactoring**
```haskell
-- Original: Simple coordinate tracking
data OldState = OldState Coord [Move]

-- New: Added resource tracking
data NewState = NewState Coord Int Int Bool [Move]

-- After adding fields, compiler shows errors at:
-- - Line 45: Pattern match incomplete
-- - Line 67: Constructor needs more arguments
-- - Line 123: Missing field in record update
-- Fix each error → Refactoring complete!

-- In Python/JavaScript: Find bugs at runtime over weeks
-- In Haskell: Compiler finds ALL bugs in seconds
```

**Why This Matters:**
- ✅ Impossible to forget fields
- ✅ Refactoring is safe (compiler finds all uses)
- ✅ Self-documenting types
- ✅ Less runtime errors
- ✅ Better IDE support

---

## 🏗️ Project Architecture

```
mazebreaker/
│
├── Main.hs                 # Entry point, initializes UI
│   └── Launches Gloss game loop
│
├── UI.hs                   # User interface & rendering
│   ├── drawGame            # Render everything
│   ├── handleEvent         # Mouse/keyboard input
│   ├── updateGame          # Animation loop
│   └── drawSettingsPanel   # Configurable settings UI
│
└── Core/                   # Core logic (pure functions)
    │
    ├── Maze.hs             # Data structures
    │   ├── Tile (ADT)      # Wall, Empty, Gate, Key, etc.
    │   ├── getTile         # Pure lookup
    │   ├── setTile         # Immutable update
    │   └── findTile        # Recursive search
    │
    ├── Generator.hs        # Procedural maze generation
    │   ├── generateMaze    # Main entry point
    │   ├── carveMaze       # Recursive DFS carving
    │   ├── placeGates      # Strategic gate placement
    │   ├── ensureKeyReachable  # BFS verification
    │   └── addSpecialWalls # Constraint-based placement
    │
    └── Solver.hs           # A* pathfinding
        ├── solveMaze       # Entry point
        ├── astar           # A* algorithm (recursive)
        ├── getNeighborStates  # Generate valid moves
        └── sortByHeuristic # Priority queue sorting
```

**Data Flow (Pure Functional Pipeline):**
```
User clicks "New Maze"
  ↓
generateMaze settings gen
  ↓
carveMaze (recursive DFS) → carved maze
  ↓
placeGates → maze with gates
  ↓
ensureKeyReachable (BFS verification) → verified maze
  ↓
addSpecialWalls → final maze
  ↓
Display in UI

User clicks "Solve"
  ↓
solveMaze maze
  ↓
astar (A* algorithm) → explores states recursively
  ↓
getNeighborStates (generates walk/break/jump moves)
  ↓
sortByHeuristic (priority queue)
  ↓
Returns path: [Move]
  ↓
Animate path in UI
```

**Every step is a pure function - no mutations anywhere!**

## 🙏 Acknowledgments

Built with:
- **Gloss** - Haskell graphics library
- **GHC** - Glasgow Haskell Compiler
- **Cabal** - Haskell build tool

---

**Happy Functional Programming! 🎉**

*Remember: Functional programming isn't about avoiding mutation for its own sake - it's about making code easier to reason about, test, and maintain. The constraints force you into good design!*