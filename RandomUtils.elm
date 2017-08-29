module RandomUtils exposing (..)

import Random as R

pair : R.Generator a -> R.Generator b -> R.Generator (a,b)
pair genA genB =
  R.map2 (,) genA genB

rReturn : a -> R.Generator a
rReturn x = R.map (always x) R.bool
