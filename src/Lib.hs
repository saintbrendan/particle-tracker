module Lib
    ( centerRadius,
      isValid
    ) where

import Data.Complex (mkPolar)

centerRadius :: Float -> Float -> Float -> Float -> Float -> Float  -> (Float, Float, Float)
centerRadius x1 y1 x2 y2 x3 y3 =
    let x12 = x1 - x2
        x13 = x1 - x3
        y12 = y1 - y2
        y13 = y1 - y3
        y31 = y3 - y1
        y21 = y2 - y1
        x31 = x3 - x1
        x21 = x2 - x1
        sx13 = x1 * x1 - x3 * x3
        sy13 = y1 * y1 - y3 * y3
        sx21 = x2 * x2 - x1 * x1
        sy21 = y2 * y2 - y1 * y1
        fn = sx13 * x12 + sy13 * x12 + sx21 * x13 + sy21 * x13
        fd = 2 * (y31 * x12 - y21 * x13)
        f = fn / fd
        gn = sx13 * y12 + sy13 * y12 + sx21 * y13 + sy21 * y13
        gd = 2 * (x31 * y12 - x21 * y13)
        g = gn / gd
        c = x1 * x1 - y1 * y1 - 2 * g * x1 - 2 * f * y1
        x = -g
        y = -f
        r2 = x * x + y * y - c
        r = sqrt r2
    in
       (x, y, r)

isValid :: Int -> [Int] -> Bool
isValid numPixelsInCircle xs =
    True

toCartesian :: Float -> Float -> (Float, Float)
toCartesian magnitude phase =
    (magnitude * (cos phase), magnitude * (sin phase))

toPolar :: Float -> Float -> (Float, Float)
toPolar x y =
    (sqrt(x*x + y*y), atan(y/x))

isLinear :: Int -> [Int] -> Bool
isLinear numPixelsInCircle xs =
    let outerpoint = last xs
    in all (<=1) [ (abs (delta outerpoint x))  | x<-xs]
        where delta a b = ((a-b) + numPixelsInCircle) `mod` numPixelsInCircle
