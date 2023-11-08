{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Project where

import CodeWorld
import Data.Maybe
import Data.Text (Text, pack)
import Data.HashMap (HashMap, insert, member, empty)

-- Game state
-- Previous board
-- Current Board
-- Whose turn? black or white?
-- Black captures -> how many black tokens were captured
-- White captures -> how many white tokens were captured
-- Was last play pass?
-- Is game won? by who
data GameState = State Board Board Turn BlackCaptures WhiteCaptures WasLastPass IsWon
data Token = Black | White deriving (Eq, Show)
data Move = Add Token Double Double
type Play = Maybe Move -- Where Nothing equates pass
type Cell = (Double, Double, Maybe Token)
type Board = [[Cell]]
type WhiteCaptures = Int
type BlackCaptures = Int
type IsWon = Maybe Token
type WasLastPass = Bool
type Turn = Token

-- | Define grid size here
-- Typically, size is chosen from 9, 13, 19.
gridSize :: Double
gridSize = 13

-- | Draw a square empty board of any specified size.
emptyBoard :: Double -> Board
emptyBoard size = [[(fromIntegral x, fromIntegral y, Nothing) | 
          x <- [(- floor (size / 2)) .. (floor (size - (size / 2)) )]]
          | y <- [(- floor (size / 2)) .. (floor (size - (size / 2)))]]

initialBoard :: Board
initialBoard = emptyBoard gridSize

initialState :: GameState
initialState = State initialBoard initialBoard Black 0 0 False Nothing

-- Drawing Functions

-- | Draw Token at a specific intersection
drawTokenAt :: Token -> Double -> Double -> Picture
drawTokenAt Black x y = translated x y (solidCircle 0.45)
drawTokenAt White x y = colored white (translated x y (solidCircle 0.45))

-- A cell is where tokens can be: intersections.
-- | Draw cell with the token on it if it exists.
drawCell :: Cell -> Picture
drawCell (x, y, tk) = drawToken <> horizontalLine <> verticalLine 
  where
    drawToken =
      case tk of
        Nothing -> blank
        Just a -> drawTokenAt a x y
    horizontalLine = colored (translucent black) (translated x y (solidRectangle 1 0.05))
    verticalLine =   colored (translucent black) (translated x y (solidRectangle 0.05 1))


-- | Draw a frame around the board with 0.5 thickness
--  and add a background color to the board.
drawFrame :: Double -> Picture
drawFrame size = pictures
    [
     colored (light brown) (solidRectangle (size - 1) (size - 1)),
     colored (dark brown) (solidRectangle size size)
    ]

-- | Draw the main GO board
drawBoard :: Board -> Picture
drawBoard board =
  foldl
  (\rowState row ->
   foldl
   (\state x -> state <> drawCell x)
   blank -- initial state
   row <> rowState)
   blank -- initial state
   board
  <> 
  drawFrame gridSize

-- | TODO: print winner if it exists, for now it's always black
drawState :: GameState -> Picture 
drawState (State _ cur tk bc wc hasPassed won) = turnImage <> drawBoard cur 
  <> whiteCaptures <> blackCaptures <> pass <> passed
  where
    turnImage = 
      case tk of 
        Black -> translated 0 8 (lettering "Black's Turn")
        White -> translated 0 8 (lettering "White's Turn")

    bcStr = translated 0 0.75 (lettering (pack "You have captured\n")  
            <> translated 0 (-1.5) (lettering (pack (show bc ++ " Black tokens"))))
    wcStr = translated 0 0.75 (lettering (pack "You have captured\n")  
            <> translated 0 (-1.5) (lettering (pack (show wc ++ " White tokens"))))
    passStr = colored white (lettering "Press Esc to Pass")
    passedStr = scaled 0.7 0.7 (colored red (lettering "If you pass, game ends"))
    wonStr = scaled 0.7 0.7 (colored (dark green) (lettering "Game overrr, BLACK WON!"))
    whiteCaptures = translated 13 (-4) 
      (scaled 0.75 0.75 (colored white wcStr) <> solidRectangle 7 3)
    blackCaptures = translated (-13) 4 
      (scaled 0.75 0.75 bcStr <> rectangle 7 3)
    pass = translated 0 (-8.5) (scaled 0.75 0.75 passStr <> colored (dark brown) (solidRectangle 7 1.5))
    passed
      | isJust won =  translated 0 7 wonStr
      | hasPassed = translated 0 7 passedStr
      | otherwise = blank

-- | Apply token to board as per given move.
addTokenAt :: Move -> Board -> Board
addTokenAt (Add tk x y) board =
  map
    ( \row ->
        map
          ( \(x2, y2, js) ->
              if x == x2 && y == y2
                then (x2, y2, (Just tk))
                else (x2, y2, js)
          )
          row
    )
    board

-- Utility functions
thrd :: (a, b, c) -> c
thrd (_, _, c) = c

find' :: (Double, Double) -> [(Double, Double, Maybe Token)] -> Maybe Token
find' _ [] = Nothing
find' (x, y) ((a, b, c):xs) 
  | x == a && y == b = c
  | otherwise = find' (x, y) xs

getNewTurn :: Token -> Token
getNewTurn Black = White
getNewTurn White = Black

-- | Get value of token at cell at x, y
getCellAt :: (Double, Double) -> Board -> Maybe Token
getCellAt (x, y) board = find' (x, y) (concat board)

doesCellExist :: (Double, Double) -> Bool
doesCellExist (x, y) = (x >= (-gridSize /2) && x <= (gridSize/2))
                    && (y >= (-gridSize /2) && y <= (gridSize/2))

-- | Is this neighbouring cell considered a liberty to me?
-- My type - Neighbour type - Neighbour position - board 
isMyLiberty :: Maybe Token -> Maybe Token -> (Double, Double) -> Board -> HashMap Double Bool -> Bool
isMyLiberty _ Nothing _ _ _ = True
isMyLiberty tk tk2 (x, y) board visited
  | member (x * gridSize + y) visited = False
  | tk == tk2 = doIHaveALiberty (x, y) board visited
  | otherwise = False

-- | Check the 4 neighbouring cell if any of them is a liberty.
doIHaveALiberty :: (Double, Double) -> Board -> HashMap Double Bool -> Bool
doIHaveALiberty (x, y) board visited = upL || downL || rightL || leftL
  where
    newVisited = insert (x * gridSize + y) True visited
    myType = getCellAt (x, y) board
    upL =   doesCellExist (x, y-1) && (not (isJust myType) ||
            isMyLiberty myType (getCellAt (x, y-1) board) (x, y-1) board newVisited)
    downL = doesCellExist (x, y+1) && (not (isJust myType) ||
            isMyLiberty myType (getCellAt (x, y+1) board) (x, y+1) board newVisited)
    rightL = doesCellExist (x+1, y) && (not (isJust myType) ||
            isMyLiberty myType (getCellAt (x+1, y) board) (x+1, y) board newVisited)
    leftL = doesCellExist (x-1, y) && (not (isJust myType) ||
            isMyLiberty myType (getCellAt (x-1, y) board) (x-1, y) board newVisited)


-- | Do any of this Player's tokens get captured?
doIGetCaptured :: Board -> Token -> Bool
doIGetCaptured board tk = not (foldl
    (\state (x, y, z) -> if z == Just tk then state && doIHaveALiberty (x, y) board empty else state)
    True
    (concat board))

-- | Do I capture any of my opponent's tokens?
doICapture :: Board -> Token -> Bool
doICapture board tk = doIGetCaptured board newTk
  where 
    newTk = getNewTurn tk

-- | Verify the rule of Ko, which states that the new move can't 
-- result in the previous state of the board.
-- lastBoard currentBoard
verifyKo :: Board -> Board -> Bool
verifyKo board newBoard = board /= newBoard

-- | Check if move is valid, it's valid if 
-- The move isn't a sacrifice, and satisfies KO.
isMoveValid :: Board -> Board -> Move -> Bool
isMoveValid lastBoard currentBoard (Add tk x y) 
  | isJust (getCellAt (x, y) currentBoard) = False
  | doICapture newBoard tk = verifyKo lastBoard newBoard
  | doIGetCaptured newBoard tk = False
  | otherwise = verifyKo lastBoard newBoard
  where
    newBoard = addTokenAt (Add tk x y) currentBoard

deleteTokenAt :: (Double, Double) -> Board -> Board
deleteTokenAt (x, y) board =
  map
    ( \row ->
        map
          ( \(x2, y2, js) ->
              if x == x2 && y == y2
                then (x2, y2, Nothing)
                else (x2, y2, js)
          )
          row
    )
    board


-- | Capture tokens of the token given if they are in a captured state
-- and increase the blackCaptured and whiteCaptured counts accordingly
capture :: (Int, Int) -> Board -> Token -> (Int, Int, Board)
capture (bc, wc) board tk = (newBc, newWc, newBoard)
  where
    flatBoard = concat board
    isCellCaptured = map (\(x, y, z) -> (x, y, (not (doIHaveALiberty (x, y) board empty)) && z == (Just tk))) flatBoard
    (newBoard, newC) =
      foldl
      (\(state, stateN) (x, y, z) -> if z then (deleteTokenAt (x, y) state, stateN +1) else (state, stateN))
      (board, 0)
      isCellCaptured
    newBc 
      | tk == Black = bc + newC
      | otherwise = bc
    newWc
      | tk == White = wc + newC
      | otherwise = wc

-- | Make the move
processMove :: GameState -> Move -> GameState
processMove (State _ cur tk bc wc passed won) move = 
  State newCur board newTk newBc newWc passed won
  where
    newCur = addTokenAt move cur
    newTk = getNewTurn tk
    (newBc, newWc, board) = capture (bc, wc) newCur newTk

-- | TODO: handle end game
processPlay :: GameState -> Play -> GameState
processPlay (State a b c d e _ g) Nothing = State a b c d e True g
processPlay (State pre cur tk bc wc passed won) (Just move)
  | isMoveValid pre cur move = processMove (State pre cur tk bc wc passed won) move
  | otherwise = State pre cur tk bc wc passed won

-- TODO: implement Game end: Two passes in a row, game ends, get winner
-- For now, black always wins
-- | Handle state transitions and process moves
transitionState :: Event -> GameState -> GameState 
transitionState (PointerPress (x, y)) (State pre cur tk bc wc _ won) =
         if doesCellExist (flooredX, flooredY) then
           processPlay
            (State pre cur tk bc wc False won)
            (Just (Add tk flooredX flooredY))
          else
            State pre cur tk bc wc False won
         where
          flooredX = fromIntegral (round x)
          flooredY = fromIntegral (round y)
transitionState (KeyPress "Esc") (State pre cur tk bc wc passed won)
  | passed = State pre cur (getNewTurn tk) bc wc passed (Just Black)
  | otherwise = State pre cur (getNewTurn tk) bc wc True won
transitionState _ world = world

-- TODO: implement these
-- calculateScore :: State -> (Double, Double)
-- getWinner :: (Double, Double) -> Maybe Token

-- | Default entry point.
run :: IO ()
run = do
  activityOf initialState transitionState drawState
