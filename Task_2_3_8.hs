{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Task_2_3_8 where

newtype Temperature a = Temperature Double
  deriving (Num,Show,Eq,Fractional)

data Celsius
data Fahrenheit
data Kelvin

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature c) = Temperature (c - 273.15)
