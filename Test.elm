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

type Msg = ClickBox Int 
         | StartGame

view : Html.Html Msg
view = Html.button [onClick Start] [Html.text "Start"]
