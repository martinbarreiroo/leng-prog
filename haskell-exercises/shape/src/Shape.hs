module Shape where
   
data Point = Point { x::Double, y:: Double} deriving (Eq, Show)

data Circle    = Circle    Point Double deriving (Eq, Show)
data Rectangle = Rectangle Point Point deriving (Eq, Show)


-- A point from a tuple Pair
point::(Double, Double) -> Point
point (x,y) = Point x y

-- The origin
origin::Point
origin = Point 0 0

-- Rectangle from a Tuple where (x0 y0) == origin
rectangle::(Double, Double) -> Rectangle
rectangle (x,y) = Rectangle origin (Point x y)

base::Rectangle -> Double
base (Rectangle _ (Point x _)) = x

height::Rectangle -> Double
height (Rectangle _ (Point _ y)) = y

-- Circle from radius
circle::Double -> Circle
circle r = Circle origin r 

-- Clase Shift

class Shift a where
   shift::a -> (Double, Double) -> a
   
instance Shift Point where
   shift (Point x y) (dx, dy) = Point (x+dx) (y+dy)
   
instance Shift Rectangle where
   shift (Rectangle (Point x1 y1) (Point x2 y2)) (dx, dy) = Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))
   
instance Shift Circle where
   shift (Circle (Point x y) r) (dx, dy)  = Circle (Point (x+dx) (y+dy)) r
   
-- Define the Surface class

class Surface a where
   surface::a -> Double

instance Surface Rectangle where
   surface r = base r * height r
   
instance Surface Circle where
   surface c = pi * r2
      where
         r2 = getRadius c * getRadius c


getRadius::Circle -> Double
getRadius (Circle _ r) = r
         