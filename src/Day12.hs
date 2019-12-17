module Day12 where

import Text.ParserCombinators.ReadP
import Data.Char

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

readPos = fst . last . readP_to_S moons <$> readFile "./data/Day12-sample.txt"

gravity m [] = []
gravity m1@(x1,y1,z1) ((x2,y2,z2):ms) =
    (g x1 x2, g y1 y2, g z1 z2):gravity m1 ms
  where
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

simulate2 = do
    i <- init
    step 0 (fmap pos i) i
  where
    init = do
      pos1:pos2:pos3:pos4:_ <- readPos
      let io       = Moon "IO"       pos1 (0,0,0)
          europa   = Moon "Europa"   pos2 (0,0,0)
          ganymede = Moon "Ganymede" pos3 (0,0,0)
          callisto = Moon "Callisto" pos4 (0,0,0)
      return [io, europa, ganymede, callisto]

    step n inits s@(io:eu:ga:ca:_) = do
      --print n
      let ioVel = velocity . (vel io:) . gravity (pos io) $ fmap pos [eu, ga, ca]
      let euVel = velocity . (vel eu:) . gravity (pos eu) $ fmap pos [io, ga, ca]
      let gaVel = velocity . (vel ga:) . gravity (pos ga) $ fmap pos [io, eu, ca]
      let caVel = velocity . (vel ca:) . gravity (pos ca) $ fmap pos [io, eu, ga]
      let ioNewPos = move [pos io, ioVel]
      let euNewPos = move [pos eu, euVel]
      let gaNewPos = move [pos ga, gaVel]
      let caNewPos = move [pos ca, caVel]
      if inits == [ioNewPos, euNewPos, gaNewPos, caNewPos]
      then return s
      else step (n+1) inits
           [io{pos=ioNewPos, vel=ioVel}
           ,eu{pos=euNewPos, vel=euVel}
           ,ga{pos=gaNewPos, vel=gaVel}
           ,ca{pos=caNewPos, vel=caVel}]
