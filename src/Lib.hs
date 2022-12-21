module Lib
  ( centerRadius,
    isValid,
  )
where

-- https://www.had2know.org/academics/circle-through-three-points.html#:~:text=r2%20%3D%20(x%20%2D%20h,y%2Dcoordinate%20of%20the%20center.
-- https://math.stackexchange.com/questions/213658/get-the-equation-of-a-circle-when-given-3-points
-- https://www.geeksforgeeks.org/equation-of-circle-when-three-points-on-the-circle-are-given/
centerRadius :: Float -> Float -> Float -> Float -> Float -> Float -> (Float, Float, Float)
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
   in (x, y, r)

isValidLinearOrArcTrack :: Int -> [Int] -> Float -> Float -> Float -> Bool
isValidLinearOrArcTrack numPixelsInCircle xs xc yc rc
  | (isInfinite xc) || (isInfinite yc) || (isInfinite rc) = isLinear numPixelsInCircle xs
  | otherwise = isArc numPixelsInCircle xs xc yc rc

isValidTrack :: Int -> [Int] -> [Int] -> Bool
isValidTrack numPixelsInCircle _ [] = True
isValidTrack numPixelsInCircle ts (tmid : ttail) =
  let phasemid = (2.0 * pi * (fromIntegral tmid) / (fromIntegral numPixelsInCircle))
      (xmid, ymid) = toCartesian (fromIntegral (((length ts) `div` 2) + 1)) phasemid
      touter = last ttail
      phaseouter = (2.0 * pi * (fromIntegral touter) / (fromIntegral numPixelsInCircle))
      (xouter, youter) = toCartesian (fromIntegral (length ts)) phaseouter
      (xc, yc, rc) = centerRadius 0.0 0.0 xmid ymid xouter youter
   in isValidLinearOrArcTrack numPixelsInCircle ts xc yc rc

isValid :: Int -> [Int] -> Bool
isValid numPixelsInCircle xs =
  let (xs1, xs2) = splitAt ((length xs) `div` 2) xs
   in (isLinear numPixelsInCircle xs) || (isValidTrack numPixelsInCircle xs xs2)

toCartesian :: Float -> Float -> (Float, Float)
toCartesian magnitude phase =
  (magnitude * (cos phase), magnitude * (sin phase))

toPolar :: Float -> Float -> (Float, Float)
toPolar x y =
  (sqrt (x * x + y * y), atan (y / x))

isLinear :: Int -> [Int] -> Bool
isLinear numPixelsInCircle xs =
  let outerpoint = last xs
   in all (\x -> (x <= 1) || (x == (numPixelsInCircle - 1))) [(abs (outerpoint - x)) | x <- xs]

isSubArch :: Int -> Int -> [Int] -> Float -> Float -> Float -> Bool
isSubArch _ _ [] _ _ _ = True
isSubArch detectorNum numPixelsInCircle (t : ts) cx cy cr =
  let circumference = 2.0 * pi * (fromIntegral detectorNum) :: Float
      minAccuracy = circumference / (fromIntegral numPixelsInCircle)
      phase = ((2.0 * pi) * (fromIntegral t) / (fromIntegral numPixelsInCircle))
      (x, y) = toCartesian (fromIntegral detectorNum) phase
  in ((distanceFromPointToCircle x y cx cy cr) < minAccuracy) && isSubArch (detectorNum + 1) numPixelsInCircle ts cx cy cr

isArc :: Int -> [Int] -> Float -> Float -> Float -> Bool
-- numPixelsInCircle is use to determine minimum accuracy for each detector
isArc numPixelsInCircle xs cx cy cr =
  isSubArch 1 numPixelsInCircle xs cx cy cr

-- https://www.petercollingridge.co.uk/tutorials/computational-geometry/circle-circle-intersections/
-- https://mathworld.wolfram.com/Circle-CircleIntersection.html
-- https://mathworld.wolfram.com/Circle-CircleIntersection.html#:~:text=Two%20circles%20may%20intersect%20in,known%20as%20the%20radical%20line.
-- https://math.stackexchange.com/questions/256100/how-can-i-find-the-points-at-which-two-circles-intersect
distanceFromPointToCircle :: Float -> Float -> Float -> Float -> Float -> Float
distanceFromPointToCircle px py cx cy cr =
  let xDelta = px - cx
      yDelta = py - cy
      d = sqrt (xDelta * xDelta + yDelta * yDelta)
   in abs $ d - cr