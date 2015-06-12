module SoundsOfDuck.SimpleSynth.Oscillator where

import SoundsOfDuck.Sound
import SoundsOfDuck.Notation.Note
import SoundsOfDuck.SimpleSynth.Adding

type Envelope a = (Duration -> a)

data Oscillator a =
  Oscillator
    { getSampleAt :: a -> Duration -> Float
    , getPeriodLength :: a -> Float }
-- this data is used to define various "primitive" oscillators
-- like sinus and square. Usually a, the parameter that defines
-- its character over time, is just frequency, but it
-- can also include things like pulse width

sinusSample :: Frequency -> Duration -> Float
sinusSample f d =
  sin $ 2 * pi * f * d
sinusPeriod f = 
  (1/f) 
sinusOsc :: (Oscillator Frequency)
sinusOsc =
  Oscillator 
    { getSampleAt = sinusSample
    , getPeriodLength = sinusPeriod }
-- that's our first oscillator, a sinus wave
-- non-trivial to compute programatically, 
-- but possesses various important acoustic and
-- mathematical proprieties
-- mostly useless on its own due to its low loudness
-- and boring timbre, but can be used in additive synthesis

squareSample :: Frequency -> Duration -> Float
squareSample f d =
  let
    phase = 1/f
  in    
    if d < (phase/2) then
      1 
    else  
      if d > phase then
        squareSample f (d - phase)
      else  
        (-1)
squarePeriod f = 1/f
squareOsc :: (Oscillator Frequency)
squareOsc =
  Oscillator
    { getSampleAt = squareSample
    , getPeriodLength = squarePeriod }
-- simple to generate programmatically (in fact the most
-- obvious signal to generate), but practically non-existant 
-- outside the world of sound synthesis
-- good starting point for subtractive synthesis 

shapedSquareSample :: (Frequency, Float) -> Duration -> Float
shapedSquareSample (f, s) d =
  let
    phase = 1 / f
  in
    if d < ((phase / 2) * (2 * s)) then
      1
    else
      if d > phase then
        shapedSquareSample (f, s) (d - phase)
      else
        -1
shapedSquarePeriod (f, s) = 1/f
shapedSquareOsc :: (Oscillator (Frequency, Float))
shapedSquareOsc =
  Oscillator
    { getSampleAt = shapedSquareSample
    , getPeriodLength = shapedSquarePeriod }

phasesFromOscillatorEnvelope :: (Oscillator a) -> (Envelope a) -> Duration -> [a]
phasesFromOscillatorEnvelope o e d =
  let
    p = e 0
    tplen = getPeriodLength o p
    plen = min tplen d
  in
    if (tplen > d) then
      [p]
    else
      p : (phasesFromOscillatorEnvelope o (e . (+ plen)) (d - plen))

oscillateSound ::
  (Oscillator a) -> (Envelope a) -> Duration -> Sound
oscillateSound o e dur =
  let
    phases = phasesFromOscillatorEnvelope o e dur
    paramsToSound p =
      let f t = getSampleAt o p t in Sound f (getPeriodLength o p)
  in
    concatSound $ map paramsToSound phases

silenceSound :: Duration -> Sound
silenceSound = Sound (const 0) 
