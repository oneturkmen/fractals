type Point = (Float, Float)

next :: Point -> Point -> Point
next (u, v) (x, y) = (x * x - y * y + u, 2 * x * y + v)

mandelbrot :: Point -> [Point]
mandelbrot p = iterate (next p) (0,0)

-- * Check if the the point so close to the origin
fairlyClose :: Point -> Bool
fairlyClose (u, v) = (u * u + v * v) < 100


-- * To check if the set belongs to the Mandelbrot set
-- * we have to check that each element in the sequence
-- * is close to the origin

-- * But! This is not computable function, because
-- * it may never reach an end
inMandelbrotSet :: Point -> Bool
inMandelbrotSet p = all fairlyClose (mandelbrot p)

-- * We need some kind of approximation function
-- * where we have the finite amount of elements 
-- * in the list. 
-- * Thus, if all of the elements of some finite set
-- * are in the Mandelbrot set, then we may assume
-- * that the set is Mandelbrot set
-- * Note! The bigger the value of "n", the more 
-- * accurate our approximation function is.
approxTest :: Int -> Point -> Bool
approxTest n p = all fairlyClose (take n (mandelbrot p))


-- * Let us have the pseudo color palettes to draw the
-- * images
chooseColor :: [color] -> [Point] -> color
chooseColor palette = (palette!!).length.take n.takeWhile fairlyClose
                      where n = length palette - 1

