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
     
onOpen :: Frame a -> Panel b -> Var (Maybe (Image ())) -> MenuItem c -> StatusField -> IO ()
onOpen f p vImg mclose status
  = do 
      mbfname <- fileOpenDialog f False {- change current directory -} True "Open image" imageFiles "" ""
      for_ mbfname $ \fname -> openImage p vImg mclose status fname

onClose :: Panel a -> Var (Maybe (Image ())) -> MenuItem b -> StatusField -> IO ()
onClose p vImg mclose status
  = do closeImage vImg
       set mclose [enabled := False]
       set p      [virtualSize := sz 0 0]
       img        <- get vImg value
       set p      [on paint := onPaint img]
       set status [text := ""]
       repaint p

closeImage :: Var (Maybe (Image ())) -> IO ()
closeImage vImg
  = do 
      mbImg <- swap vImg value Nothing
      for_ mbImg $ \img -> objectDelete img

openImage :: Panel a -> Var (Maybe (Image ())) -> MenuItem b -> StatusField -> String -> IO ()
openImage p vImg mclose status fname
  = do -- load the new bitmap
       let img    = image fname
       imgSize    <- get img size
       innerSize  <- get p clientSize
       imgScaled  <- imageScale img (getImageScale imgSize innerSize)
       closeImage vImg
       set vImg   [value := Just img]
       set mclose [enabled := True]
       set status [text := fname]
       set p      [on paint := onPaint (Just imgScaled)]
       repaint p
   `onException` repaint p

onPaint :: Maybe (Image ()) -> DC () -> Rect -> IO ()
onPaint mbImg dc _
  = case mbImg of
      Nothing -> return () 
      Just img -> drawImage dc img pointZero []

onProcess :: Panel a -> Var (Maybe (Image ())) -> Var (Maybe (Image ())) -> StatusField -> IO ()
onProcess p vImg vImgProcessed status
  = do
      mbImg <- get vImg value
      for_ mbImg $ \img -> 
        do
          let outputFile = "processed/processed.png"
          imgSize        <- get img size
          pixelBuffer    <- imageGetPixels img
          processedImg   <- imageCreateFromPixels imgSize (map (\x -> colorFromInt (intFromColor x `div` 2)) pixelBuffer)
          set vImgProcessed [ value := Just processedImg ]

          createDirectoryIfMissing True "processed"
          _              <- imageSaveFile processedImg outputFile (imageTypeFromFileName outputFile)
          fullOutputPath <- makeAbsolute outputFile
          set status     [ text := ("Processed file saved to " ++ fullOutputPath) ]

          -- use scaled image for preview
          innerSize      <- get p clientSize
          imgScaled      <- imageScale processedImg (getImageScale imgSize innerSize)
          set p          [on paint := onPaint (Just imgScaled)]
          repaint p
          return ()
      return ()
    `onException` repaint p
