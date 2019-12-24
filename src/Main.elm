module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main = Browser.sandbox {
    init = init,
    update = update,
    view = view }

type alias Model = Int

init : Model
init = 0

type alias Msg = (Model -> Model)

update : Msg -> Model -> Model
update msg model = msg model

increment : Model -> Model
increment x = x + 1

decrement : Model -> Model
decrement x = x - 1

view : Model -> Html Msg
view model =
  div []
   [ button [ onClick decrement ] [ text "-" ],
     div [] [ text (String.fromInt model) ],
     button [ onClick (increment >> increment) ] [ text "+" ] ]



