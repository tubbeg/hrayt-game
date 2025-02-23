module Types (Position (Pos), CellState (Falling, Static), TetrisCell (Empty, Cell), Game) where
import qualified Data.Map.Strict as Map
import Data.Map (Map())
import Raylib.Types (Color)

newtype Position = Pos (Int,Int) deriving (Eq, Ord)
data CellState = Falling | Static
data TetrisCell = Empty | Cell Color CellState
type Game = Map Position TetrisCell