module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (attribute)


---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ button
            [ attribute "onclick" "window.hello('github').login()"
            ]
            [ text "Authenticate!" ]
        , button
            [ attribute "onclick" "window.hello('github').logout()"
            ]
            [ text "Log out" ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
