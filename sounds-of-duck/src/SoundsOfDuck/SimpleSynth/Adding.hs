module SoundsOfDuck.SimpleSynth.Adding where

import SoundsOfDuck.Sound

concatSound :: [Sound] -> Sound
concatSound [] = Sound (const 0) 0
concatSound ((Sound f1 t1):s) =
  Sound (\t -> if (t < t1) then f1 t else f2 (t - t1)) (t1 + t2)
  where
    (Sound f2 t2) = concatSound s

addSound :: [Sound] -> Sound
addSound [] = Sound (const 0) 0
addSound ((Sound f1 t1):s) =
  Sound (\t -> (f1 t) + (f2 t)) (max t1 t2)
  where
    (Sound f2 t2) = addSound s

mapSound :: (Duration -> Float -> Float) -> Sound -> Sound
mapSound f (Sound f1 t1) =
  Sound (\t -> (f t) . f1 $ t) t1

liftSound :: ((Duration -> Float) -> (Duration -> Float)) -> Sound -> Sound
liftSound f (Sound g d) = (Sound (f g) d)

multiplySound :: [Sound] -> Sound
multiplySound [] = Sound (const 1) 0
multiplySound ((Sound f1 t1):s) =
  Sound (\t -> (f1 t) * (f2 t)) (max t1 t2)
  where
    (Sound f2 t2) = multiplySound s
    


amplifySound = (mapSound . const) . (*)

