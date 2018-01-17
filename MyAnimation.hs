module MyAnimation where 

import Animation

width :: Double
width = 800

height :: Double
height = 600

frame :: Animation
frame = 
 withBorder (always black) (always 20) 
  (withoutPaint (rect (always (width)) (always (height))))
 `plus`
 translate (always (width / 2 - 8, 0 ))
  (rect (always (20)) (always (height)))
   
-- glass
glass :: Animation
glass =
 translucent (always (0.20))
  (withPaint (always teal)
   (rect (always (width)) (always (height))))

-- grid -- specify number of lines in the grid
grid :: Int -> Int -> Animation
grid i j = 
 combine[translate (always (0, (fromIntegral k) * height /(1 + (fromIntegral i)))) (rect (always (width)) (always (5))) | k <- [1..i]]
 `plus`
 combine[translate (always ((fromIntegral k) * width / (1 + (fromIntegral j)), 0)) (rect (always (5)) (always (height))) | k <- [1..j]]
   
-- mapple leaf
mappleLeafBody :: Animation
mappleLeafBody =
 withPaint (always red) 
  (polygon[(110, 0), (90, 40), (40, 30), (50, 40), (0, 80),
           (20, 90), (20, 120), (40, 110), (40, 130), (60, 100),
           (50, 150), (70, 140), (90, 170), (110, 140), (130, 160),
           (120, 110), (140, 120), (140, 110), (170, 120), (160, 90),
           (180, 80), (130, 40), (140, 30), (100, 40), (110, 10)])

mappleLeafStalk :: Animation
mappleLeafStalk =
 withPaint (always maroon) 
  (polygon[(110, 0), (90, 40), (100, 40), (110, 10)])

mappleLeaf :: Animation
mappleLeaf =  withBorder (always black) (always 2) (mappleLeafBody `plus` mappleLeafStalk)

-- birch leaf
birchLeafBody :: Animation
birchLeafBody = 
 (withPaint (always olive) 
  ((combine [translate (always (x, 0)) (circle(always (80))) | x <- [-45,45]])
   `plus` translate (always (0, -20)) (teeth 130 180)))
  
birchLeafStalk :: Animation
birchLeafStalk =
 translate (always (-95, -110))
  (withPaint (always maroon)   
    (polygon[(90, 40), (100, 40), (110, 10), (180, -50), (180, -60)]))

birchLeaf :: Animation
birchLeaf =
 translate (always (180, 180 )) (
  (withBorder (always black) (always 6) (birchLeafBody `plus` birchLeafStalk) )
  `plus`
  (birchLeafStalk `plus` birchLeafBody) ) -- overlap the borders of the shapes

-- function to create the teeth of birch leaf
teeth :: Double -> Double -> Animation
teeth x y = 
 (combine[ polygon [(0,0), (x, y), (-x, y)] | x <- [0, 10..x], y <- [0, 10..y],  x + y == maxxy])
 where
 maxxy = max x y
 
leaves :: Animation
leaves =
 fall birchLeaf [360, 320..120] 
 `plus`
 fall mappleLeaf [450, 400..50]
  
-- create the leave movement
-- leaves in front have a higher translation and rotation speed and larger dimension
fall :: Animation -> [Double] -> Animation
fall animation xs =
  combine [(translate (cycleSmooth (x / 10) [(((x * (-1)^(mod (ceiling x) 7))), -100 - (x / 2)), (350 + ((200 + x * (-1)^(mod (ceiling x) 3))), 700), (-250, 700), (-250, -250)])  
   (rotate (repeatSmooth 5 ( zip (map (*(x/300)) [1..18]) [5,10..90] ++ zip (map (*(x/300)) [19..37]) [85,80..5]  )) 
    (scale (always (50 / x, 50 / x)) animation))) | x <- xs]

-- entire picture
picture :: Animation 
picture = leaves `plus` glass `plus` (grid 3 5) `plus` frame

test :: IO ()                                                  
test = writeFile "MyAnimation.svg" (svg 800 600 picture) 

