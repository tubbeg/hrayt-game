module Config where

import qualified Types as T


windowSize :: (Int, Int)
windowSize = (500, 700)
metaSize :: Int
metaSize = 100

tetrisSize :: (Int, Int)
tetrisSize = (10,20)

tetrisBlockSize :: T.Position
tetrisBlockSize = T.Pos (30, 30)
