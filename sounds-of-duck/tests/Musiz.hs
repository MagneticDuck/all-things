module Musiz where

import SoundsOfDuck

ssInstrument :: (Envelope Frequency) -> Duration -> Sound
ssInstrument e d =
  addSound
    [ amplifySound 0.5 $ oscillateSound sinusOsc e d
    , amplifySound 0.5 $ oscillateSound squareOsc e d ]

wavyBase :: (Envelope Frequency) -> Duration -> Sound
wavyBase e d = oscillateSound shapedSquareOsc f d
  where
    f t =
      ((100 * (((sin $ 2 * pi * t * f1) + 1) / 2) + (e t)), (((sin $ 2 * pi * t * f1) + 1) / 2))

f1 = 10

sound1 =
  addSound
    [ wavyBase (const $ freqFromString "c3") 10
    , concatSound [silenceSound 2, (wavyBase (const $ freqFromString "e3") 8)]
    , concatSound [silenceSound 4, (wavyBase (const $ freqFromString "g3") 6)]
    , concatSound [silenceSound 6, (wavyBase (const $ freqFromString "c4") 4)] ]

sound2 =
  wavyBase f 5
  where
    f t =
      (freqFromString "c4") + ((t / 5) * ((freqFromString "c5") - (freqFromString "c4")))
