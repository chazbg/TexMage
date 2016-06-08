module EventHandlers where

import Control.Exception (onException)
import Graphics.UI.WX
import Graphics.UI.WXCore

-- Specify image files for the file open dialog.
imageFiles
   = [("Image files",["*.bmp","*.jpg","*.gif","*.png"])
     ,("Portable Network Graphics (*.png)",["*.png"])
     ,("BMP files (*.bmp)",["*.bmp"])
     ,("JPG files (*.jpg)",["*.jpg"])
     ,("GIF files (*.gif)",["*.gif"])
     ]
     
onOpen :: Frame a -> ScrolledWindow b -> Var (Maybe (Bitmap ())) -> MenuItem c -> StatusField -> IO ()
onOpen f sw vbitmap mclose status
  = do mbfname <- fileOpenDialog f False {- change current directory -} True "Open image" imageFiles "" ""
       case mbfname of
         Nothing    -> return ()
         Just fname -> openImage sw vbitmap mclose status fname

onClose :: ScrolledWindow a -> Var (Maybe (Bitmap ())) -> MenuItem b -> StatusField -> IO ()
onClose sw vbitmap mclose status
  = do closeImage vbitmap
       set mclose [enabled := False]
       set sw     [virtualSize := sz 0 0]
       set status [text := ""]
       repaint sw

closeImage :: Var (Maybe (Bitmap ())) -> IO ()
closeImage vbitmap
  = do mbBitmap <- swap vbitmap value Nothing
       case mbBitmap of
         Nothing -> return ()
         Just bm -> objectDelete bm

openImage :: ScrolledWindow a -> Var (Maybe (Bitmap ())) -> MenuItem b -> StatusField -> String -> IO ()
openImage sw vbitmap mclose status fname
  = do -- load the new bitmap
       let img = image fname
       Size w h <- get img size
       imgScaled <- imageConvertToBitmap =<< imageScale img (sz (w `div` h * 500) 500)
       closeImage vbitmap
       set vbitmap [value := Just imgScaled]
       set mclose [enabled := True]
       set status [text := fname]
       -- reset the scrollbars 
       bmsize <- get imgScaled size
       set sw [virtualSize := bmsize]
       repaint sw
   `onException` repaint sw

onPaint :: Var (Maybe (Bitmap ())) -> DC () -> Rect -> IO ()
onPaint vbitmap dc viewArea
  = do mbBitmap <- get vbitmap value
       case mbBitmap of
         Nothing -> return () 
         Just bm -> drawBitmap dc bm pointZero False []