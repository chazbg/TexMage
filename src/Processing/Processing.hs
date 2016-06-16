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

getChannels :: Color -> [Int]
getChannels c = [ colorRed c, colorGreen c, colorBlue c ]

normalizeColorChannel :: Int -> Float
normalizeColorChannel c = fromIntegral c / 255.0

denormalizeColorChannel :: Float -> Int 
denormalizeColorChannel c = round (c * 255)

dummyProcessChannel :: Int -> Int
dummyProcessChannel c = c `div` 2

dummyProcessPixel :: Color -> Color
dummyProcessPixel c
  = let 
      channels = map dummyProcessChannel (getChannels c)
      processedR = head channels
      processedG = channels !! 1
      processedB = channels !! 2
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
    channels = map normalizeColorChannel (getChannels c)
    processedChannels = map denormalizeColorChannel (linearRGB channels)
    processedR = head processedChannels
    processedG = processedChannels !! 1
    processedB = processedChannels !! 2
  in rgb processedR processedG processedB

sRGBtoLinear :: [Color] -> [Color]
sRGBtoLinear = map sRGBtoLinearPixel