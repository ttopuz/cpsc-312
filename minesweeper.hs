module Minesweeper( State,
                    Result(EndOfGame, ContinueGame),
                    Game,
                    Player,
                    Click(LeftClick,RightClick),
                    UserAction(UserAction),
                    minesweeper,
                    mineDistributor,
                    printGrid) where 

import System.Random
import Data.Char
import Text.Read

data State = State InternalState        -- the state of the game is the
         deriving (Ord, Eq)--, Show)    -- internal state of the game

data Result = EndOfGame Double State    -- end of game, value, endstate
            | ContinueGame State        -- continue with new state
         deriving (Eq)

type Game = UserAction -> State -> Result

type Player = State -> UserAction

-- Minesweeper Game

data Click = LeftClick  --click
            |RightClick --flag?
        deriving (Eq)

-- an action is a triple of an x value, a y value, and the value to be placed
-- at said x,y location
newtype UserAction = UserAction (Int, Int, Click)
    deriving (Eq)

type InternalState = [[Int]]

-- the game is over when all the 1's are replaced with 4's and there are
-- no more 2's
-- ıdea ıs taken from https://github.com/unoctium1/HaskellMinesweeper/blob/master/src/Minesweeper.hs
empty = 0               -- empty space, uncleared
mine = 1                -- mine, undiscovered
emptyFlagged = 2        -- empty space, flagged incorrectly
emptyCleared = 3        -- empty space, cleared over the course of the game
bombFlagged = 4         -- mine, flagged
bombCleared = 5         -- mine, cleared (loss condition)

minesweeper :: Game
minesweeper (UserAction (x,y,c)) (State (grid))
    | to_replace == bombCleared                 = EndOfGame 0 (State new_grid)   -- did we lose?
    | win new_grid                              = EndOfGame 1 (State new_grid)   -- did we win?
    | otherwise                                 = if boolToExplode then ContinueGame (State (explode new_grid (x,y))) else ContinueGame (State new_grid)
        where
            init = find grid x y
            -- https://github.com/unoctium1/HaskellMinesweeper/blob/master/src/Minesweeper.hs
            newValue = if (init == empty && c == LeftClick) then emptyCleared
                         else if (init == empty) then emptyFlagged
                         else if (init == mine && c == LeftClick) then bombCleared
                         else if (init == mine) then bombFlagged
                         else if (init == emptyFlagged && c == LeftClick) then empty
                         else if (init == bombFlagged && c == LeftClick) then mine
                         else init
            newGrid = updateGrid grid x y newValue
            boolToExplode = newValue == emptyCleared && (countbombs newGrid (x,y)) == 0

-- assembles an InternalState given length height difficulty and# of mines
makeGrid length height diff mine = 
      do
            grid <- mineDistributor length height mine (matrix height length)
            return (State grid)

-- helper function which assembles lists into a list of lists
matrix :: Int -> Int -> InternalState
matrix 0 x = []
matrix y x = (makeRow x) : (matrix (y-1) x)

-- helper function which builds lists of int
makeRow :: Int -> [Int]
makeRow 0 = []
makeRow x = 0 : makeRow (x-1)

-- given the length height and difficulty and # of mines, the functions returns a grid
mineDistributor :: Int -> Int -> Int -> InternalState -> InternalState
mineDistributor length height 0 grid = grid
mineDistributor length height mines grid =
      do 
            rg <- newStdGen
            let randomX = head(randomRs (1,gridSize) rg)
            rg2 <- newStdGen
            let randomY = head(randomRs (1,gridSize) rg2)
            if (hasBomb randomX randomY grid)
                  then mineDistributor length height mines grid
                  else mineDistributor length height (mines - 1) (updateGrid grid randomX randomY 1) 

-- takes the current grid and checks the win the condition
-- true if there is no mines or emptyflags on the grid
win :: [[Int]] -> Bool
win [] = True
win (h:r) = 
      | (elem mine h || elem emptyFlagged h) = False
      | otherwise = True && win r

-- given coordinate x,y, the function replace the value with given c value
updateGrid :: [[Int]] -> Int -> Int -> Int -> [[Int]]
updateGrid (h:r) x y c =
      | y == 1 = updateRow h x c : r   
      | otherwise = h : updateGrid r x (y - 1) c

updateRow (h:r) x c =
      | x == 1 = c : r
      | otherwise = h : updateRow r (x - 1) c


-- print the grid
printGrid :: State -> IO ()
printGrid (State grid) = do
    let size = length grid
    let topper = "  +──" ++ (getTopper (head grid)) ++ "─+"
    putStrLn topper
    let board = getBoard grid size size grid
    putStrLn board
    let bottom = "  +──" ++ (getTopper (head grid)) ++ "─+"
    putStrLn bottom

-- builds an x axis for the board
getTopper :: [Int] -> String
getTopper [] = []
getTopper (first:rest) = (getTopper rest) ++ (show (length (first:rest))) ++ "─"

-- builds a string of the entire board and the y axis of the board
getBoard :: InternalState -> Int -> Int -> InternalState -> String
getBoard [] index size _ = []
getBoard (first:rest) index size ins
    |index == size = do
        let row = "1 |  " ++ (getRow first 1 1 ins) ++ " |\n" ++ (getBoard rest (index - 1) size ins)
        row
    |index == 1 = do
        let row = (show size) ++ " |  " ++ (getRow first 1 size ins) ++ " |"
        row
    |otherwise = do
        let row = (show (size - index + 1)) ++ " |  " ++ (getRow first 1 (size-(index-1)) ins) ++ " |\n" ++ (getBoard rest (index - 1) size ins)
        row

-- generates each row to be assembled by getBoard
getRow :: [Int] -> Int -> Int -> InternalState -> String
getRow [] _ _ _ = []
getRow (first:rest) x y ins = (getSpace first x y ins) ++ " " ++ (getRow rest (x+1) y ins)

-- generates each space, concealing hidden information from the player
-- which is then assembled by getRow
-- from https://github.com/unoctium1/HaskellMinesweeper/blob/master/src/Minesweeper.hs
getSpace :: Int -> Int -> Int -> InternalState -> String
getSpace space x y ins
    | space == mine = "_"
    | space == emptyFlagged = "F"
    | space == emptyCleared = show (countbombs ins (x, y))
    | space == bombFlagged = "F"
    | space == bombCleared = "B"
    | otherwise = "_"

find :: InternalState -> Int -> Int -> Int
find lst x y = (lst!!(y-1))!!(x-1)

-- explode function given x,y coordinate and the grid
-- taken from https://github.com/unoctium1/HaskellMinesweeper/blob/master/src/Minesweeper.hs
explode :: InternalState -> (Int, Int) -> InternalState
explode [] _ = []
explode (first:rest) (x, y) =
    let
        neighbors = [(x-1, y),(x+1, y),(x-1, y-1),(x, y-1),(x+1,y-1),(x-1, y+1),(x, y+1),(x+1,y+1)]
        neighborsfilter = filter (\ (a,b) -> (a > 0) && (a <= (length first)) && b > 0 && (b <= (1+(length rest)))) neighbors
        neighborsfilter2 = filter (\ (a,b) -> (find (first:rest) a b) == 0) neighborsfilter
    in
        explodehelper (first:rest) neighborsfilter2

explodehelper st [] = st
explodehelper st ((x,y):rst)
    | countbombs st (x,y) == 0      =   explodehelper (explode (find_replace st x y emptyCleared) (x,y)) rst
    | otherwise                     =   explodehelper (find_replace st x y emptyCleared) rst

-- Given an (x,y) coordinate, returns the number of bombs at the space
-- Returns true if no spaces are emptyFlagged or mines
countbombs :: [[Int]] -> (Int, Int) -> Int
countbombs [] _ = 0
countbombs (first:rest) (x, y) =
    let
        neighbors = [(x-1, y),(x+1, y),(x-1, y-1),(x, y-1),(x+1,y-1),(x-1, y+1),(x, y+1),(x+1,y+1)]
        neighborsfilter = filter (\ (a,b) -> (a > 0) && (a <= (length first)) && b > 0 && (b <= (1+(length rest)))) neighbors
        mappedneighbors = map (\ (a,b) -> find (first:rest) a b) neighborsfilter

    in
        length (filter (\ a -> a >= bombFlagged || a == mine) mappedneighbors)

-- returns true if there is a bomb at x,y
hasBomb :: InternalState -> Int -> Int -> Bool
hasBomb grid x 0 = False
hasBomb (first:rest) x y
      |y == 1 = hasBombHelper first x
      |otherwise = hasBomb rest x (y-1)
      
hasBombHelper :: [Int] -> Int -> Bool
hasBombHelper grid 0 = False
hasBombHelper (first:rest) x
      |x == 1 = if (first == 1) then True else False
      |otherwise = hasBombHelper rest (x-1)
