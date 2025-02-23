module Types (Position (Pos), CellState (Falling, Static), TetrisCell (Empty, Cell), Game) where

import qualified Data.Map as M
import Raylib.Types (Color)

newtype Position = Pos (Int,Int) deriving (Eq, Ord)
data CellState = Falling | Static
data TetrisCell = Empty | Cell Color CellState
type Game = M.Map Position TetrisCell