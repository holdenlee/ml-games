module Permute exposing (..)

import Random as R
import List exposing (..)
import Array as A
import RandomUtils exposing (..)
import Maybe as M
import Debug exposing (..)

extractWithDefault : a -> Int -> A.Array a -> (a, A.Array a)
extractWithDefault d n arr = 
    let
        newArr = A.append (A.slice 0 n arr) (A.slice (n+1) (A.length arr) arr)
        x = M.withDefault d (A.get n arr)
    in
        (x,newArr)

rPermute : a -> A.Array a -> R.Generator (A.Array a)
rPermute d arr = if A.isEmpty arr
               then rReturn arr
               else (R.int 0 ((A.length arr)-1)) 
                   |> R.andThen 
                      (\i -> 
                           let (x, li1) = (extractWithDefault d i arr)
                               (x1,li11) = log (toString (x, li1)) (x,li1)
                           in rPermute d li11
                           |> R.andThen (\li2 -> rReturn (A.append (A.fromList [x1]) li2)))
