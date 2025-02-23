module DrawTetris where

import Raylib.Core (
  clearBackground)
import Raylib.Util.Colors (white, magenta, green)
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Types (Color)
import qualified Data.Map as M
import Types (Game, TetrisCell (Cell), Position(Pos))
import qualified Config as C



tetrisPosToWindowPos :: (Int,Int) -> (Int,Int)
tetrisPosToWindowPos (px,py) =
  pos
  where
    (wx,wy) = C.windowSize
    (mx,my) = C.tetrisSize
    ymult = div wy my
    xmult = div wx mx
    pos = (xmult * px, ymult * py)

drawTetrisBlock :: (Int, Int) -> Color -> IO ()
drawTetrisBlock pos color = do
  drawRectangle px py width height color
  return ()
  where
    f (Pos (a,b)) = (a,b)
    (width, height) = f C.tetrisBlockSize
    (px,py) = tetrisPosToWindowPos pos

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
      (gx,_) = C.tetrisSize

drawTetris :: Game -> IO ()
drawTetris game = do
  clearBackground white
  -- drawText "woah woooh" 100 500 100 orange
  -- drawTetrisBlock (3,10) green
  -- drawTetrisBlock (5,18) magenta
  drawTetrisBlocks gy game
  return ()
  where
    (_,gy) = C.tetrisSize