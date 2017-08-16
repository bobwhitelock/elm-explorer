module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (attribute)
import Json.Decode as Decode exposing (Decoder)


---- MODEL ----


type alias Model =
    { packages : Packages }


type alias Packages =
    List String


init : List String -> ( Model, Cmd Msg )
init packages =
    ( { packages = packages }, Cmd.none )



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


main : Program Packages Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
