import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

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
-- RENDERING WITH OpenGL
---------------------------------------------------------------
data Complex = C Float Float deriving (Show, Eq)

instance Num Complex where
  fromInteger n = C (fromIntegral n) (0.0)
  (C p q) * (C r s) = C (p*r - q*s) (q*r + p*s)
  (C p q) + (C r s) = C (p+r) (q+s)
  abs (C p q) = C (sqrt (p*p + q*q)) 0.0
  signum (C x y) = C (signum x) (0.0)

-- Functions to manipulate complex numbers
complex :: Float -> Float -> Complex
complex p q = C p q

real :: Complex -> Float
real (C p q) = p

im :: Complex -> Float
im (C p q) = q

magnitude :: Complex -> Float
magnitude = real . abs


-- Rendering part
width = 320 :: GLfloat -- remove
height = 320 :: GLfloat

points :: [(GLfloat, GLfloat, Color3 GLfloat)]
points = [ (x/width, y/height, colorFromValue $ mandel x y) |
              x <- [-width..width],
              y <- [-height..height]]

-- Given a point returns integer
mandel x y = mandelBackbone (complex r i) 0 64
  where
    r = 2.0 * x / width
    i = 2.0 * y / height


mandelBackbone :: Complex -> Complex -> Int -> Int
mandelBackbone c z 0 = 0
mandelBackbone c z n = if (magnitude z > 2) then n
                       else mandelBackbone c ((z*z)+c) (n-1)


colorFromValue n =
  let
    t :: Int -> GLfloat
    t i = 0.5 + 0.5 * cos (fromIntegral i / 10)
  in
    Color3 (t n) (t (n + 1)) (t (n - 1))

drawMandelbrot =
  renderPrimitive Points $ do
    mapM_ drawColoredPoint points
  where
    drawColoredPoint (x, y, c) = do
      color c
      vertex $ Vertex3 x y 0


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [ DoubleBuffered ]
  createWindow "Hello Mandelbrot!"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  loadIdentity
  preservingMatrix drawMandelbrot
  swapBuffers
