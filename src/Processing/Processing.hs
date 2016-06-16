module Processing where

import Graphics.UI.WX

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

dummyProcessPixel :: Color -> Color
dummyProcessPixel c 
  = let 
      r = colorRed c
      g = colorGreen c
      b = colorBlue c
      processedR = r `div` 2
      processedG = g `div` 2
      processedB = b `div` 2
    in rgb processedR processedG processedB

dummyProcess :: [Color] -> [Color]
dummyProcess = map dummyProcessPixel

mix :: Float -> Float -> Float -> Float
mix a b t = (1.0 - t) * a + t * b

lessThanK0 :: Float -> Float
lessThanK0 x 
  = let 
      k0 = 0.04045
    in if x <= k0 then 1.0 else 0.0

linearRGBLessThanK0 :: Float -> Float
linearRGBLessThanK0 x 
  = let
      inversePhi = 1.0 / 12.92
    in inversePhi * x

linearRGBLargerThanK0 :: Float -> Float
linearRGBLargerThanK0 x 
  = let
      gamma = 2.4
      alpha = 0.055
    in (x + alpha) / (1.0 + alpha) ** gamma

linearRGB :: [Float] -> [Float]
linearRGB [] = []
linearRGB (x:xs)
  = let
      a = linearRGBLargerThanK0 x
      b = linearRGBLessThanK0 x
      t = lessThanK0 x
    in mix a b t : linearRGB xs

sRGBtoLinearPixel :: Color -> Color
sRGBtoLinearPixel c
  = let 
    channels = [ fromIntegral (colorRed c) / 255.0, 
                 fromIntegral (colorGreen c) / 255.0, 
                 fromIntegral (colorBlue c) / 255.0 ]
    processedChannels = linearRGB channels
    processedR = head processedChannels
    processedG = processedChannels !! 1
    processedB = processedChannels !! 2
  in rgb (round (processedR * 255.0)) (round (processedG * 255.0)) (round (processedB * 255.0))

sRGBtoLinear :: [Color] -> [Color]
sRGBtoLinear = map sRGBtoLinearPixel