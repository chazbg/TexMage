module EventHandlers where

import Control.Exception (onException)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Foldable (for_)
import System.Directory

-- Specify image files for the file open dialog.
imageFiles :: [(String, [String])]
imageFiles
   = [("Image files",["*.bmp","*.jpg","*.gif","*.png"])
     ,("Portable Network Graphics (*.png)",["*.png"])
     ,("BMP files (*.bmp)",["*.bmp"])
     ,("JPG files (*.jpg)",["*.jpg"])
     ,("GIF files (*.gif)",["*.gif"])
     ]

-- TODO: Move to another source file
getImageScale :: Size -> Size -> Size 
getImageScale imageSize windowSize
  = let 
      imageWidth   = sizeW imageSize
      imageHeight  = sizeH imageSize
      windowWidth  = sizeW windowSize
      windowHeight = sizeH windowSize
      newWidth     = (imageWidth  * min windowWidth windowHeight) `div` max imageWidth imageHeight
      newHeight    = (imageHeight * min windowWidth windowHeight) `div` max imageWidth imageHeight
    in Size newWidth newHeight
     
onOpen :: Frame a -> ScrolledWindow b -> Var (Maybe (Image ())) -> MenuItem c -> StatusField -> IO ()
onOpen f sw vImg mclose status
  = do 
      mbfname <- fileOpenDialog f False {- change current directory -} True "Open image" imageFiles "" ""
      for_ mbfname $ \fname -> openImage sw vImg mclose status fname

onClose :: ScrolledWindow a -> Var (Maybe (Image ())) -> MenuItem b -> StatusField -> IO ()
onClose sw vImg mclose status
  = do closeImage vImg
       set mclose [enabled := False]
       set sw     [virtualSize := sz 0 0]
       set status [text := ""]
       repaint sw

closeImage :: Var (Maybe (Image ())) -> IO ()
closeImage vImg
  = do 
      mbImg <- swap vImg value Nothing
      for_ mbImg $ \img -> objectDelete img

openImage :: ScrolledWindow a -> Var (Maybe (Image ())) -> MenuItem b -> StatusField -> String -> IO ()
openImage sw vImg mclose status fname
  = do -- load the new bitmap
       let img = image fname
       Size imgW imgH <- get img size
       Size clientW clientH  <- get sw clientSize
       imgScaled <- imageScale img (getImageScale (sz imgW imgH) (sz clientW clientH))
       closeImage vImg
       set vImg [value := Just imgScaled]
       set mclose [enabled := True]
       set status [text := fname]
       -- reset the scrollbars 
       bmsize <- get imgScaled size
       set sw [virtualSize := bmsize]
       repaint sw
   `onException` repaint sw

onPaint :: Var (Maybe (Image ())) -> DC () -> Rect -> IO ()
onPaint vImg dc _
  = do mbImg <- get vImg value
       case mbImg of
         Nothing -> return () 
         Just img -> drawImage dc img pointZero []

onPaintSecond :: Var (Maybe (Image ())) -> DC () -> Rect -> IO ()
onPaintSecond vImg dc _
  = do mbImg <- get vImg value
       case mbImg of
         Nothing -> return () 
         Just img -> drawImage dc img pointZero []

onProcess :: ScrolledWindow a -> Var (Maybe (Image ())) -> Var (Maybe (Image ())) -> StatusField -> IO ()
onProcess sw vImg vImgProcessed status
-- = for_ vImg $ \img -> return ()
  = do
      mbImg <- get vImg value
      for_ mbImg $ \img -> 
        do
          Size imgW imgH <- get img size
          pixelBuffer <- imageGetPixels img
          processedImg <- imageCreateFromPixels (sz imgW imgH) (map (\x -> colorFromInt (intFromColor x `div` 2)) pixelBuffer)
          set vImgProcessed [ value := Just processedImg ]
          createDirectoryIfMissing True "processed"
          _ <- imageSaveFile processedImg "processed/processed.png" (imageTypeFromFileName "processed/processed.png")
          set status [text := "TODO: Print saved image location"]
          repaint sw
          return ()
      return ()
    `onException` repaint sw
