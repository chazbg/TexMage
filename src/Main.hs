module Main where

import Control.Exception (onException)
import Graphics.UI.WX
import Graphics.UI.WXCore
import EventHandlers

main :: IO ()
main
  = start imageViewer

-- The image viewer.
imageViewer :: IO ()
imageViewer
  = do -- the main frame, we use 'fullRepaintOnResize' to prevent flicker on resize
       f      <- frame [text := "ImageViewer", picture := "images/image.jpg", fullRepaintOnResize := False]

       -- use a mutable variable to hold the image
       vbitmap <- variable [value := Nothing]

       -- add a scrollable window widget in the frame
       sw     <- scrolledWindow f [scrollRate := sz 10 10, 
                                   on paint := onPaint vbitmap,
                                   bgcolor := white, fullRepaintOnResize := False]

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
       set f [layout           := column 1 [hfill $ hrule 1,  -- add divider between toolbar and scrolledWindow
                                            fill (widget sw)],
              statusBar        := [status],
              menuBar          := [file, hlp],
              outerSize        := sz 800 800,    -- niceness
              on (menu about)  := infoDialog f "About ImageViewer" "This is a wxHaskell demo",
              on (menu quit)   := close f,
              on (menu open)   := onOpen f sw vbitmap mclose status,
              on (menu mclose) := onClose sw vbitmap mclose status,

             -- nice close down, but no longer necessary as bitmaps are managed automatically.
              on closing       :~ \previous -> do{ closeImage vbitmap; previous }
             ]
