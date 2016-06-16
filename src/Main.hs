module Main where

import Graphics.UI.WX
import EventHandlers

createPanelLayout :: Panel a -> Layout
createPanelLayout p = column 1 [ fill $ widget p ]

createPreviewLayout :: Panel a -> Panel b -> [Layout]
createPreviewLayout p1 p2 = [ grid 1 1 [ [ createPanelLayout p1, vfill $ vrule 1, createPanelLayout p2 ] ] ] 

createFrameLayout :: Panel a -> Panel b -> Layout
createFrameLayout p1 p2 = grid 1 1 [ [ hfill $ hrule 1 ], createPreviewLayout p1 p2 ]

main :: IO ()
main
  = start imageViewer

-- The image viewer.
imageViewer :: IO ()
imageViewer
  = do -- the main frame, we use 'fullRepaintOnResize' to prevent flicker on resize
       f      <- frameFixed [text := "TexMage", picture := "images/image.jpg", fullRepaintOnResize := False]

       -- use a mutable variable to hold the image
       vImg <- variable [value := Nothing]
       vImgProcessed <- variable [value := Nothing]

       -- add preview panels in the frame
       p1     <- panel f [ bgcolor := white, fullRepaintOnResize := False ]
       p2     <- panel f [ bgcolor := white, fullRepaintOnResize := False ]

       -- create file menu
       file   <- menuPane       [text := "&File"]
       mclose <- menuItem file  [text := "&Close\tCtrl+C", help := "Close the image", enabled := False]
       open   <- menuItem file  [text := "&Open\tCtrl+O",  help := "Open an image"]
       process <- menuItem file [text := "&Process\tCtrl+P",  help := "Process an image"]
       menuLine file
       quit   <- menuQuit file  [help := "Quit"]

       -- create Help menu
       hlp    <- menuHelp       []
       about  <- menuAbout hlp  [help := "About TexMage"]

       -- create Toolbar
       tbar   <- toolBar f []
       _      <- toolMenu tbar open  "Open"  "images/image.jpg" []
       _      <- toolMenu tbar process  "Process"  "images/image.jpg" []

       -- create statusbar field
       status <- statusField    [text := "Welcome to TexMage"]

       -- set the statusbar, menubar, layout, and add menu item event handlers
       -- note: set the layout before the menubar!
       set f [layout            := createFrameLayout p1 p2,
              statusBar         := [status],
              menuBar           := [file, hlp],
              outerSize         := sz 640 480,    -- niceness
              on (menu about)   := infoDialog f "About TexMage" "TODO",
              on (menu quit)    := close f,
              on (menu open)    := onOpen f p1 vImg mclose status,
              on (menu mclose)  := onClose p1 vImg mclose status,
              on (menu process) := onProcess p2 vImg vImgProcessed status,

             -- nice close down, but no longer necessary as bitmaps are managed automatically.
              on closing       :~ \previous -> do{ closeImage vImg; previous }
             ]
