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
import Raylib.Util.Colors (orange, white, magenta, green)
import Raylib.Internal(WindowResources)
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Types (Color)
import qualified Data.Map as M

newtype Position = Pos (Int,Int) deriving (Eq, Ord)
data CellState = Falling | Static
data TetrisCell = Empty | Cell Color CellState
type Game = M.Map Position TetrisCell


tetrisSize :: (Int, Int)
tetrisSize = (10,20)

tetrisBlockSize :: Position
tetrisBlockSize = Pos (30, 30)

drawTetrisBlock :: (Int, Int) -> Color -> IO ()
drawTetrisBlock (x,y) color = do
  drawRectangle x y width height color
  return ()
  where
    f (Pos (a,b)) = (a,b)
    (width, height) = f tetrisBlockSize

drawCell :: (Int, Int) -> Maybe TetrisCell -> IO ()
drawCell pos (Just (Cell color _)) = do
  drawTetrisBlock pos color
  return ()
drawCell _ _ = do return ()

drawTetrisBlockRow :: Int -> Int -> Game -> IO ()
drawTetrisBlockRow x y game
  | x <= 0 = return ()
  | otherwise = do
    drawCell (x,y) maybeCell
    drawTetrisBlockRow (x - 1) y game
    where
      maybeCell = M.lookup (Pos (x,y))  game

drawTetrisBlocks :: Int -> Game -> IO ()
drawTetrisBlocks y game
  | y <= 0 = return ()
  | otherwise = do
    drawTetrisBlockRow gx y game
    drawTetrisBlocks (y - 1) game
    return ()
    where
      (gx,_) = tetrisSize

drawStuff :: Game -> IO Game
drawStuff game = do
  clearBackground white
  -- drawText "woah woooh" 100 500 100 orange
  drawTetrisBlock (100,100) green
  drawTetrisBlock (300,200) magenta
  drawTetrisBlocks gy game
  return game
  where
    (_,gy) = tetrisSize

drawFunc :: Game -> IO Game
drawFunc game = do
  beginDrawing
  gs <- drawStuff game
  endDrawing
  return gs


loadWindow :: IO WindowResources
loadWindow = do
  setTargetFPS 60
  initWindow 500 800 "my-window"

mainDraw :: Bool -> Game -> IO ()
mainDraw True _ = return ()
mainDraw False game = do
  stopDrawing <- windowShouldClose
  updatedGameState <- drawFunc game
  mainDraw stopDrawing updatedGameState

initGame :: Game
-- initGame = M.empty
initGame = [(Pos (2,2), Cell magenta Falling)]

main :: IO ()
main = do
  w <- loadWindow
  mainDraw False initGame
  closeWindow $ Just w
  return ()
