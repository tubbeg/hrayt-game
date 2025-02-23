{-# LANGUAGE OverloadedLists #-}
module Main where


import Raylib.Core (
  initWindow,
  beginDrawing,
  endDrawing,
  windowShouldClose,
  closeWindow,
  setTargetFPS,
  clearBackground)
import Raylib.Core.Text (drawText)
import Raylib.Util.Colors (orange, white, magenta, green, purple, skyBlue)
import Raylib.Internal(WindowResources)
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Types (Color)
import qualified Data.Map as M
import Types (Game, CellState (Falling, Static), TetrisCell (Cell), Position(Pos))
import qualified Config as C
import qualified DrawTetris as DT
import qualified GameTime as GT
import System.Clock

-- i think that I'll skip using an ECS since I want to keep things easy
-- but I could defintely see the value of it

getCellAbove :: (Int,Int) -> Game -> Maybe TetrisCell
getCellAbove (x,y) game =
  maybeCell
  where
    p = Pos (x,y - 1)
    maybeCell = M.lookup p game

updateCell :: Int -> Int -> Game -> Game
updateCell x y game =
  case aboveCell of
    Just cell ->
      M.insert p cell game
    _ -> game
  where
    p = Pos (x,y)
    aboveCell = getCellAbove (x,y) game

dropCellsInRow :: Int -> Int -> Game -> Game
dropCellsInRow x y game
  | x <= 0 = game
  | otherwise =
    dropCellsInRow (x - 1) y updatedGame
    where
      updatedGame = updateCell x y game

dropRows :: Int -> Game -> Game
dropRows y game
  | y <= 0 = game
  | otherwise =
    dropRows (y - 1) updatedGame
    where
      updatedGame = dropCellsInRow sx y game
      (sx,_) = C.tetrisSize

dropCells :: Game -> Game
dropCells = dropRows sy
    where
      (_,sy) = C.tetrisSize

removeCells :: Game -> Game
removeCells game = game

updateGameState :: Game -> Game
updateGameState =
  -- compose all "systems"
  dropCells . removeCells

drawFunc :: Game -> TimeSpec -> IO (Game, TimeSpec)
drawFunc game time = do
  currentTime <- GT.getMonoTime
  let
    updatedGame = updateGameState game
    ut = time + currentTime
    reset = 0
  beginDrawing
  DT.drawTetris game
  endDrawing
  if GT.isTimeToUpdateGame ut
    then do
      putStrLn $ "running update" ++ show ut
      return (updatedGame, reset)
    else do
      putStrLn $ "time is " ++ show ut
      return (game, ut)

loadWindow :: IO WindowResources
loadWindow = do
  setTargetFPS 60
  initWindow sx sy "TETRIS"
  where
    (a,b) = C.windowSize
    -- add a little extra room for text (score)
    (sx,sy) = (a, b + C.metaSize)

mainDraw :: Bool -> TimeSpec -> Game -> IO ()
mainDraw True _ _ = return ()
mainDraw False time game = do
  stopDrawing <- windowShouldClose
  (updatedGameState, ct) <- drawFunc game time
  mainDraw stopDrawing ct updatedGameState

initGame :: Game
-- initGame = M.empty
initGame = [(Pos (2,2), Cell skyBlue Falling), (Pos (9, 15), Cell magenta Static)]

main :: IO ()
main = do
  w <- loadWindow
  mainDraw False 0 initGame
  closeWindow $ Just w
  return ()
