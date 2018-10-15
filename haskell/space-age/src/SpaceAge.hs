module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthYearsInSeconds = 31557600

-- Earth: orbital period 365.25 Earth days, or 31557600 seconds
-- Mercury: orbital period 0.2408467 Earth years
-- Venus: orbital period 0.61519726 Earth years
-- Mars: orbital period 1.8808158 Earth years
-- Jupiter: orbital period 11.862615 Earth years
-- Saturn: orbital period 29.447498 Earth years
-- Uranus: orbital period 84.016846 Earth years
-- Neptune: orbital period 164.79132 Earth years

ageOn :: Planet -> Float -> Float
ageOn Earth secs = (secs / earthYearsInSeconds)    
ageOn Mercury secs = (secs / (earthYearsInSeconds * 0.2408467))    
ageOn Venus secs = (secs / (earthYearsInSeconds * 0.61519726))    
ageOn Mars secs = (secs / (earthYearsInSeconds * 1.8808158))    
ageOn Jupiter secs = (secs / (earthYearsInSeconds * 11.862615))    
ageOn Saturn secs = (secs / (earthYearsInSeconds * 29.447498))    
ageOn Uranus secs = (secs / (earthYearsInSeconds * 84.016846))    
ageOn Neptune secs = (secs / (earthYearsInSeconds * 164.79132))    