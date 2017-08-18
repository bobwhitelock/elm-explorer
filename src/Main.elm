module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (attribute)
import Json.Decode as D exposing (Decoder)


---- MODEL ----


type alias Model =
    { packages : Result String (List Package) }


type alias Package =
    { name : String
    , dependencies : Dependencies
    }


type Dependencies
    = PackageNames (List String)
      -- Only errors retrieving `elm-package.json` appear to be 404s, due to:
      -- - main branch not called `master`;
      -- - package repo deleted;
      -- - `elm.json` now also appears valid.
    | Error String


init : D.Value -> ( Model, Cmd Msg )
init packages =
    ( { packages = decodePackages packages }, Cmd.none )


decodePackages : D.Value -> Result String (List Package)
decodePackages packagesJson =
    D.decodeValue packagesDecoder packagesJson


packagesDecoder : Decoder (List Package)
packagesDecoder =
    D.keyValuePairs elmPackageJsonDecoder
        |> D.map
            (List.map
                (\( name, dependencies ) -> Package name dependencies)
            )


elmPackageJsonDecoder : Decoder Dependencies
elmPackageJsonDecoder =
    D.oneOf
        [ D.field "dependencies"
            (D.keyValuePairs D.string
                |> D.map (List.map Tuple.first)
                |> D.map PackageNames
            )
        , D.string |> D.map Error
        ]



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.packages of
        Ok packages ->
            div []
                [ div []
                    [ button
                        [ attribute "onclick" "window.hello('github').login()"
                        ]
                        [ text "Authenticate!" ]
                    , button
                        [ attribute "onclick" "window.hello('github').logout()"
                        ]
                        [ text "Log out" ]
                    ]
                , div []
                    [ text (toString packages) ]
                ]

        Err message ->
            div [] [ text message ]



---- PROGRAM ----


main : Program D.Value Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
