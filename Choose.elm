--module Choose exposing (..) where

import Html
--import Html.App as App
import Html.Events exposing (..)
import Random 
import List exposing (..)
import Array as A
--import Keyboard as K
import Time exposing (..)
import Char exposing (..)
import Platform.Sub as S
import Time exposing (..)
import Platform.Cmd as C
import Maybe as M

import Permute exposing (..)
import Gaussian exposing (..)

type alias GameState = {score : Float, rounds : Int, data : List (Int, Float), seed : Random.Seed}

type alias Settings = {totalRounds : Int, boxData : A.Array (Random.Generator Float), boxes : Int}
-- (Seed -> (Float, Seed))

type Model = Start Random.Seed | InProgress GameState Settings

type Msg = ClickBox Int 
         | StartGame
         -- | GetSeed
         | SetSeed Int
--         | GetSeed Seed

update : Msg -> Model -> (Model, C.Cmd Msg)
update msg m = 
    case (msg, m) of
        --(GetSeed, m) -> (m, Random.generate SetSeed (Random.int 0 Random.maxInt))
        (SetSeed n, m) -> (Start (Random.initialSeed n), C.none) 
        (StartGame, Start s) -> 
            let (settings, seed) = Random.step randomSettings s
            in (InProgress ({score = 0, rounds = 0, data = [], seed = seed}) settings, C.none)
        (StartGame, InProgress g s) -> let (settings, seed) = Random.step randomSettings (g.seed)
            in (InProgress ({score = 0, rounds = 0, data = [], seed = seed}) settings, C.none)
--(InProgress ({score = 0, rounds = 0, data = [], seed = Random.initialSeed 0} ) defaultSettings, C.none)
        --if g.rounds >= s.totalRounds then 
        (ClickBox n, InProgress g s) -> 
            let
                (ans, newSeed) = Random.step (M.withDefault (defaultGen) (A.get (n-1) s.boxData)) g.seed
            in (InProgress {g | rounds = g.rounds + 1,
                     data = g.data ++ [(n, ans)], 
                     score = g.score + ans,
                     seed = newSeed} s, C.none)
        _ -> (m, C.none)

init : (Model, Cmd Msg)
init = (Start (Random.initialSeed 0), Random.generate SetSeed (Random.int 0 Random.maxInt))
--GetSeed)
--{totalRounds = 10, boxData =A.empty, boxes = []}

--TODO: add boxData

view : Model -> Html.Html Msg
view m = 
    case m of
        Start n -> Html.div [] [Html.button [onClick StartGame] [Html.text "Start"]]
        InProgress g s -> 
            if g.rounds >= s.totalRounds then
                Html.div [] ([Html.button [onClick StartGame] [Html.text ("Restart")]]++[Html.text <| "Round: "++(toString (g.rounds+1))++ " , Score: "++(toString g.score)])
            else Html.div [] <|
                (map (\i -> Html.button [onClick (ClickBox i)] [Html.text ("Box "++(toString i))]) (range 1 (s.boxes)))++[Html.text <| "Round: "++(toString (g.rounds+1))++ " , Score: "++(toString g.score)]++
                    [Html.ol [] (reverse (map (\(i,j) -> Html.li [] [Html.text <| ("Box "++(toString i)++": "++(toString j))]) (g.data)))]

--[Html.text <| "Round: "++(toString g.rounds)++ " , Score: "++(toString g.score)]

--make a list of buttons and toString history and score.
numBoxes : Int
numBoxes = 5

defaultSettings : Settings
defaultSettings = {totalRounds = 50, boxData = A.fromList [], boxes = numBoxes}

randomSettings : Random.Generator Settings
randomSettings = rPermute 0 (A.fromList [2, 3, 3.5, 3.75, 4]) |> 
                 Random.map (\arr -> {totalRounds = 50, boxData = A.map (\mean -> gaussianGen mean 1) arr, boxes = numBoxes})


--{totalRounds = 50, boxData = A.fromList [], boxes = 5}

defaultGen : Random.Generator Float
defaultGen = Random.float 0 1

--fill in

--defaultGen

subscriptions : Model -> Sub Msg
subscriptions model = S.none

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

{-
main =
  Html.beginnerProgram { model = model, view = view, update = update }
-}
