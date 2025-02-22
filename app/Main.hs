{-# LANGUAGE PatternSynonyms #-}
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
import Data.Map (Map, empty)

newtype Position = Pos (Int,Int)
type Game = Map Position Color

tetrisBlockSize :: (Int,Int)
tetrisBlockSize = (30, 30)


drawTetrisBlock :: (Int, Int) -> Color -> IO ()
drawTetrisBlock (x,y) color = do
  drawRectangle x y width height color
  return ()
  where
    (width, height) = tetrisBlockSize

drawStuff :: Game -> IO Game
drawStuff game = do
  clearBackground white
  -- drawText "woah woooh" 100 500 100 orange
  drawTetrisBlock (100,100) green 
  drawTetrisBlock (300,200) magenta
  return game

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
  keepDrawing <- windowShouldClose 
  updatedGameState <- drawFunc game
  mainDraw keepDrawing updatedGameState

initGame :: Game
initGame = empty

main :: IO ()
main = do
  w <- loadWindow
  mainDraw False initGame
  closeWindow $ Just w
  return ()
