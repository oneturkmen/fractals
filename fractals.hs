type Point = (Float, Float)
type Image color = Point -> color
type Grid a = [[a]]

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

-- * Combine fractal generator with palette
fracImage :: (Point -> [Point]) -> [color] -> Image color
fracImage fractal palette = chooseColor palette . fractal

-- * Describe grid that is of size 'r' by 'c'
-- * where r = number of rows
-- *       c = number of columns
grid :: Int -> Int -> Point -> Point -> Grid Point
grid c r (xmin, ymin) (xmax, ymax) 
  = [[(x,y) | x <- for c xmin xmax] | y <- for r ymin ymax]
  
-- * Define auxiliary function to take care of 
-- * evenly spaced values for x and y
for :: Int -> Float -> Float -> [Float]
for n min max = take n [min, min + delta ..]
                where delta = (max - min) / fromIntegral (n - 1)
                
-- * Sampling - iterate over the list of lists
sample :: Grid Point -> Image color -> Grid color
sample points image = map (map image) points

-- * Render
draw :: Grid Point -> (Point -> [Point]) -> [color] -> (Grid color -> image) -> image
draw points fractal palette render
  = render (sample points (fracImage fractal palette))

---------------------------------------------------------------
  
-- * Character-based images
charPalette :: [Char]
charPalette = " ,.'\"~:;o-!|?/<>X+={^O#%&@8*$"

-- * Use 'unlines' to process the rendering
charRender :: Grid Char -> IO ()
charRender = putStr . unlines

-- * Generate our first figure (Mandelbrot set)!
figure1 = draw points mandelbrot charPalette charRender
          where points = grid 75 40 (-2.25, -1.5) (0.75, 1.5)

-- * Julia set setup
julia :: Point -> Point -> [Point]
julia c = iterate (next c)

-- * Generate our second figure (Julia set)!
figure2 = draw points (julia (0.32, 0.043)) charPalette charRender
          where points = grid 75 40 ((-1.5), (-1.5)) (1.5, 1.5)

---------------------------------------------------------------

-- * Drawing with graphics
-- rgcPalette     :: [RGB]
-- graphicsWindow :: Int -> Int -> IO Window
-- setPixel       :: Window -> Int -> Int -> RGB -> IO ()
