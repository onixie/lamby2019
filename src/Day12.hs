module Day12 where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List

number :: ReadP Int
number = do
  sign <- option '+' (char '-')
  num  <- read <$> munch isDigit
  return $ case sign of
    '-' -> negate num
    _   -> num

x = string "<x=" >> number <* string ","
y = string " y=" >> number <* string ","
z = string " z=" >> number <* string ">"

moon = (,,) <$> x <*> y <*> z
moons = moon `sepBy` char '\n'

readPos = fst . last . readP_to_S moons <$> readFile "./data/Day12-input.txt"

gravity m [] = []
gravity m1@(x1,y1,z1) ((x2,y2,z2):ms) =
    (g x1 x2, g y1 y2, g z1 z2):gravity m1 ms

g ax1 ax2 =
  case ax1 `compare` ax2 of
    LT ->  1
    GT -> -1
    EQ ->  0

getx (x, _, _) = x
gety (_, y, _) = y
getz (_, _, z) = z

velocity gs = ( sum $ fmap getx gs, sum $ fmap gety gs, sum $ fmap getz gs )

move = velocity

data Moon = Moon { name :: String, pos :: (Int, Int, Int), vel :: (Int, Int, Int) } deriving (Show, Eq)
type System = [Moon]

simulate until = init >>= step 0
  where
    init = do
      pos1:pos2:pos3:pos4:_ <- readPos
      let io       = Moon "IO"       pos1 (0,0,0)
          europa   = Moon "Europa"   pos2 (0,0,0)
          ganymede = Moon "Ganymede" pos3 (0,0,0)
          callisto = Moon "Callisto" pos4 (0,0,0)
      return [io, europa, ganymede, callisto]

    step n s@(io:eu:ga:ca:_) = if n > until - 1 then return s else do
      -- print s
      let ioVel = velocity . (vel io:) . gravity (pos io) $ fmap pos [eu, ga, ca]
      let euVel = velocity . (vel eu:) . gravity (pos eu) $ fmap pos [io, ga, ca]
      let gaVel = velocity . (vel ga:) . gravity (pos ga) $ fmap pos [io, eu, ca]
      let caVel = velocity . (vel ca:) . gravity (pos ca) $ fmap pos [io, eu, ga]
      let ioNewPos = move [pos io, ioVel]
      let euNewPos = move [pos eu, euVel]
      let gaNewPos = move [pos ga, gaVel]
      let caNewPos = move [pos ca, caVel]
      step (n+1)
           [io{pos=ioNewPos, vel=ioVel}
           ,eu{pos=euNewPos, vel=euVel}
           ,ga{pos=gaNewPos, vel=gaVel}
           ,ca{pos=caNewPos, vel=caVel}]

totalE sys =
  sum $ map total sys
  where
    total Moon{pos=pos,vel=vel} = energy pos * energy vel
    energy (x, y, z) = abs x + abs y + abs z

day12Part1 = totalE <$> simulate 1000

-- naive impl: the coordinates of moon is not independent
simulate2 pick = do
    i <- init
    pat <- step 0 (fmap pos i) i pick
    print pat
    return . take 10 $ zipWith (-) (drop 1 pat) pat
  where
    init = do
      pos1:pos2:pos3:pos4:_ <- readPos
      let io       = Moon "IO"       pos1 (0,0,0)
          europa   = Moon "Europa"   pos2 (0,0,0)
          ganymede = Moon "Ganymede" pos3 (0,0,0)
          callisto = Moon "Callisto" pos4 (0,0,0)
      return [io, europa, ganymede, callisto]

    step n inits s@(io:eu:ga:ca:_) pick = if n > 5000 then return [] else do
      print n
      let ioVel = velocity . (vel io:) . gravity (pos io) $ fmap pos [eu, ga, ca]
      let euVel = velocity . (vel eu:) . gravity (pos eu) $ fmap pos [io, ga, ca]
      let gaVel = velocity . (vel ga:) . gravity (pos ga) $ fmap pos [io, eu, ca]
      let caVel = velocity . (vel ca:) . gravity (pos ca) $ fmap pos [io, eu, ga]
      let ioNewPos = move [pos io, ioVel]
      let euNewPos = move [pos eu, euVel]
      let gaNewPos = move [pos ga, gaVel]
      let caNewPos = move [pos ca, caVel]
      if pick inits == pick [ioNewPos, euNewPos, gaNewPos, caNewPos]
      then (n:) <$> step (n+1) inits
           [io{pos=ioNewPos, vel=ioVel}
           ,eu{pos=euNewPos, vel=euVel}
           ,ga{pos=gaNewPos, vel=gaVel}
           ,ca{pos=caNewPos, vel=caVel}] pick
      else step (n+1) inits
           [io{pos=ioNewPos, vel=ioVel}
           ,eu{pos=euNewPos, vel=euVel}
           ,ga{pos=gaNewPos, vel=gaVel}
           ,ca{pos=caNewPos, vel=caVel}] pick

-- x y z are orthogonal, so independent
simulate3 pick limit = do
    i <- init
    p <- step 0 (pick i) (pick i) (0,0,0,0)
    print  . take 10 $ p
    return . take 10 $ zipWith (-) (drop 1 p) p
  where
    init = do
      (x1,y1,z1):(x2,y2,z2):(x3,y3,z3):(x4,y4,z4):_ <- readPos
      return [(x1,x2,x3,x4),(y1,y2,y3,y4),(z1,z2,z3,z4)]
    step n orig (e1,e2,e3,e4) (v1,v2,v3,v4)= if n > limit then return [] else do
      --print n
      let g12 = g e1 e2
      let g13 = g e1 e3
      let g14 = g e1 e4
      let g23 = g e2 e3
      let g24 = g e2 e4
      let g34 = g e3 e4

      let nv@(nv1,nv2,nv3,nv4) = (v1+g12+g13+g14, v2-g12+g23+g24, v3-g13-g23+g34, v4-g14-g24-g34)
      let ne = (e1+nv1, e2+nv2, e3+nv3, e4+nv4)
      if ne == orig then (n:) <$> step (n+1) orig ne nv else step (n+1) orig ne nv

-- there is a pattern in the reoccurance
s init = scanl (+) init . concat . repeat

-- <x=-8, y=-10, z=0>
-- <x=5, y=5, z=10>
-- <x=2, y=-7, z=3>
-- <x=9, y=-8, z=-3>
s1' = s 16 [1, 17]
s2' = s 26 [1, 27]
s3' = s 42 [1, 43]
sx' = lcm 18 $ lcm 28 44 -- head [ e1 | e1 <- take 500 s1', e2 <- take 300 s2', e1 == e2, e3 <- take 200 s3', e2 == e3 ]

-- <x=-8, y=-10, z=0>
-- <x=5, y=5, z=10>
-- <x=2, y=-7, z=3>
-- <x=9, y=-8, z=-3>
s1 = s 2026 [1, 2027]
s2 = s 5896 [1, 5897]
s3 = s 4700 [1, 4701]
sx = lcm 2028 $ lcm 5898 4702  --head [ e1 | e1 <- take 5000000 s1, e2 <- take 2000000 s2, e1 == e2, e3 <- take 2000000 s3, e2 == e3 ]

s1'' = s 286330 [1,286331]
s2'' = s 108342 [1,108343]
s3'' = s 193050 [1,193051]
sx'' = lcm 286332 $ lcm 108344 193052 -- head [ e1 | e1 <- take 1000 s1'', e2 <- take 2000 s2'', e1 == e2, e3 <- take 1000 s3'', e2 == e3 ]

day12Part2 = sx''
