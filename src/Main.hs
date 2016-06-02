module Main where

import Control.Exception (onException)
import Graphics.UI.WX 

main :: IO ()
main
  = start imageViewer

-- Specify image files for the file open dialog.
imageFiles
   = [("Image files",["*.bmp","*.jpg","*.gif","*.png"])
     ,("Portable Network Graphics (*.png)",["*.png"])
     ,("BMP files (*.bmp)",["*.bmp"])
     ,("JPG files (*.jpg)",["*.jpg"])
     ,("GIF files (*.gif)",["*.gif"])
     ]


-- The image viewer.
imageViewer :: IO ()
imageViewer
  = do -- the main frame, we use 'fullRepaintOnResize' to prevent flicker on resize
       f      <- frame [text := "ImageViewer", picture := "images/image.jpg", fullRepaintOnResize := False]

       -- use a mutable variable to hold the image
       vbitmap <- variable [value := Nothing]

       -- add a scrollable window widget in the frame
       sw     <- scrolledWindow f [scrollRate := sz 10 10, on paint := onPaint vbitmap
                                  ,bgcolor := white, fullRepaintOnResize := False]

       -- create file menu
       file   <- menuPane      [text := "&File"]
       mclose <- menuItem file [text := "&Close\tCtrl+C", help := "Close the image", enabled := False]
       open   <- menuItem file [text := "&Open\tCtrl+O",  help := "Open an image"]
       menuLine file
       quit   <- menuQuit file [help := "Quit the demo"]

       -- create Help menu
       hlp    <- menuHelp      []
       about  <- menuAbout hlp [help := "About ImageViewer"]

       -- create Toolbar
       tbar   <- toolBar f []
       toolMenu tbar open  "Open"  "images/image.jpg" []

       -- create statusbar field
       status <- statusField   [text := "Welcome to the wxHaskell ImageViewer"]

       -- set the statusbar, menubar, layout, and add menu item event handlers
       -- note: set the layout before the menubar!
       set f [layout           := column 1 [hfill $ hrule 1  -- add divider between toolbar and scrolledWindow
                                           ,fill (widget sw)]
             ,statusBar        := [status]
             ,menuBar          := [file,hlp]
             ,outerSize        := sz 400 300    -- niceness
             ,on (menu about)  := infoDialog f "About ImageViewer" "This is a wxHaskell demo"
             ,on (menu quit)   := close f
             ,on (menu open)   := onOpen f sw vbitmap mclose status 
             ,on (menu mclose) := onClose  sw vbitmap mclose status

             -- nice close down, but no longer necessary as bitmaps are managed automatically.
             ,on closing       :~ \previous -> do{ closeImage vbitmap; previous }
             ]
  where
    onOpen :: Frame a -> ScrolledWindow b -> Var (Maybe (Bitmap ())) -> MenuItem c -> StatusField -> IO ()
    onOpen f sw vbitmap mclose status
      = do mbfname <- fileOpenDialog f False {- change current directory -} True "Open image" imageFiles "" ""
           case mbfname of
             Nothing    -> return ()
             Just fname -> openImage sw vbitmap mclose status fname f

    onClose sw vbitmap mclose status
      = do closeImage vbitmap
           set mclose [enabled := False]
           set sw     [virtualSize := sz 0 0]
           set status [text := ""]
           repaint sw

    closeImage vbitmap
      = do mbBitmap <- swap vbitmap value Nothing
           case mbBitmap of
             Nothing -> return ()
             Just bm -> objectDelete bm

    openImage sw vbitmap mclose status fname frame
      = do -- load the new bitmap
           bm <- bitmapCreateFromFile fname  -- can fail with exception
           closeImage vbitmap
           set vbitmap [value := Just bm]
           set mclose [enabled := True]
           set status [text := fname]
           -- reset the scrollbars 
           bmsize <- get bm size
           set sw [virtualSize := bmsize]
           set frame [outerSize := Size (sizeW bmsize + 400) (sizeH bmsize + 300)]
           repaint sw
       `onException` repaint sw

    onPaint vbitmap dc viewArea
      = do mbBitmap <- get vbitmap value
           case mbBitmap of
             Nothing -> return () 
             Just bm -> drawBitmap dc bm pointZero False []