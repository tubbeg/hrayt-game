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
import Raylib.Util.Colors (orange, white)
import Raylib.Internal(WindowResources)



drawStuff :: IO ()
drawStuff = do
  clearBackground white
  drawText "woah woooh" 100 500 100 orange
  return ()

drawFunc :: IO ()
drawFunc = do
  beginDrawing
  drawStuff
  endDrawing
  return ()


loadWindow :: IO WindowResources
loadWindow = do
  setTargetFPS 60
  initWindow 600 800 "my-window"

mainDraw :: Bool  -> IO ()
mainDraw True = return ()
mainDraw False = do
  keepDrawing <- windowShouldClose 
  drawFunc
  mainDraw keepDrawing

main :: IO ()
main = do
  w <- loadWindow
  mainDraw False
  closeWindow $ Just w
  return ()
