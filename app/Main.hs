{-# LANGUAGE PatternSynonyms #-}
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

updateGameState :: p -> p
updateGameState game = 
  game

drawFunc :: Game -> IO Game
drawFunc game = do
  beginDrawing
  DT.drawTetris game
  endDrawing
  return updatedGame
  where
    updatedGame = updateGameState game

loadWindow :: IO WindowResources
loadWindow = do
  setTargetFPS 60
  initWindow sx sy "TETRIS"
  where
    (a,b) = C.windowSize
    -- add a little extra room for text (score)
    (sx,sy) = (a, b + C.metaSize)

mainDraw :: Bool -> Game -> IO ()
mainDraw True _ = return ()
mainDraw False game = do
  stopDrawing <- windowShouldClose
  updatedGameState <- drawFunc game
  mainDraw stopDrawing updatedGameState

initGame :: Game
-- initGame = M.empty
initGame = [(Pos (2,2), Cell skyBlue Falling), (Pos (9, 15), Cell magenta Static)]

main :: IO ()
main = do
  w <- loadWindow
  mainDraw False initGame
  closeWindow $ Just w
  return ()
