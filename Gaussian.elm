module Gaussian exposing (..)

import RandomUtils exposing (..)
import List exposing (..)
import Array as A
import Random as R

normalGen : R.Generator Float
normalGen = pair (R.float -1 1) (R.float -1 1) |> R.andThen
             (\(x1,x2) -> 
                  let w = x1*x1 + x2*x2
                  in if w>=1.0
                     then normalGen
                     else rReturn (x1 * (sqrt ((-2.0 * (logBase e w)) / w ))))
                      
gaussianGen : Float -> Float -> R.Generator Float
gaussianGen x y = R.map (\r -> y*r+x) normalGen
