module SoundsOfDuck.Sound where

type Duration = Float -- an amount of time in seconds

data Sound = 
  Sound (Duration -> Float) Duration
  -- this data represents a sound with a sample generation
  -- function that takes a time in seconds and returns a sample value
  -- between 1 and -1, and  a duration that
  -- defines the length of time in which the sound should be rendered
