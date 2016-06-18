module EventHandlers where

import Control.Exception (onException)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Foldable (for_)
import System.Directory
import Processing

-- Specify image files for the file open dialog.
imageFiles :: [(String, [String])]
imageFiles
   = [("Image files",["*.bmp","*.jpg","*.gif","*.png"])
     ,("Portable Network Graphics (*.png)",["*.png"])
     ,("BMP files (*.bmp)",["*.bmp"])
     ,("JPG files (*.jpg)",["*.jpg"])
     ,("GIF files (*.gif)",["*.gif"])
     ]

onOpen :: Frame a -> Panel b -> Var (Maybe (Image ())) -> MenuItem c -> StatusField -> IO ()
onOpen f p vImg mclose status
  = do 
      mbfname <- fileOpenDialog f False {- change current directory -} True "Open image" imageFiles "" ""
      for_ mbfname $ \fname -> openImage p vImg mclose status fname

onClose :: Panel a -> Panel b -> Var (Maybe (Image ())) -> MenuItem c -> StatusField -> IO ()
onClose p1 p2 vImg mclose status
  = do 
      closeImage vImg
      set mclose [enabled := False]
      set p1     [on paint := onPaint Nothing]
      set p2     [on paint := onPaint Nothing]
      set status [text := ""]
      repaint p1
      repaint p2


closeImage :: Var (Maybe (Image ())) -> IO ()
closeImage vImg = set vImg [value := Nothing]

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
  = for_ mbImg $ \img -> drawImage dc img pointZero []

onProcess :: Panel a -> Var (Maybe (Image ())) -> StatusField -> IO ()
onProcess p vImg status
  = do
      mbImg <- get vImg value
      for_ mbImg $ \img -> 
        do
          let outputFile = "processed/processed.png"
          imgSize        <- get img size
          pixelBuffer    <- imageGetPixels img
          processedImg   <- imageCreateFromPixels imgSize (sRGBtoLinear pixelBuffer)
          
          createDirectoryIfMissing True "processed"
          _              <- imageSaveFile processedImg outputFile (imageTypeFromFileName outputFile)
          fullOutputPath <- makeAbsolute outputFile
          set status     [ text := "Processed file saved to " ++ fullOutputPath ]

          -- use scaled image for preview
          innerSize      <- get p clientSize
          imgScaled      <- imageScale processedImg (getImageScale imgSize innerSize)
          set p          [on paint := onPaint (Just imgScaled)]
          repaint p
    `onException` repaint p
