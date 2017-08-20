port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Table


---- PORTS ----


port githubOauthSuccess : (String -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { packages : Result String (List Package)
    , githubAccessToken : Maybe String
    , tableState : Table.State
    , query : String
    }


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
    ( { packages = decodePackages packages
      , githubAccessToken = Nothing
      , tableState = Table.initialSort "Name"
      , query = ""
      }
    , Cmd.none
    )


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


encodeGraph : List Package -> E.Value
encodeGraph packages =
    E.object
        [ ( "nodes", encodeNodes packages )
        , ( "links", encodeLinks packages )
        ]


encodeNodes : List Package -> E.Value
encodeNodes packages =
    let
        encodePackage =
            \package ->
                E.object
                    [ ( "id", E.string package.name )
                    ]
    in
    E.list (List.map encodePackage packages)


encodeLinks : List Package -> E.Value
encodeLinks packages =
    let
        encodePackage =
            \package ->
                case package.dependencies of
                    PackageNames dependencies ->
                        List.map (encodePackageLink package.name) dependencies

                    Error _ ->
                        []

        encodePackageLink =
            \packageName ->
                \dependencyName ->
                    E.object
                        [ ( "source", E.string packageName )
                        , ( "target", E.string dependencyName )
                        , ( "value", E.int 1 )
                        ]
    in
    List.map encodePackage packages
        |> List.concat
        |> E.list



---- UPDATE ----


type Msg
    = GithubOauthSuccess String
    | SetTableState Table.State
    | SetQuery String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GithubOauthSuccess token ->
            ( { model | githubAccessToken = Just token }
            , Cmd.none
            )

        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.packages of
        Ok packages ->
            viewPackages model packages

        Err message ->
            div [] [ text message ]


viewPackages : Model -> List Package -> Html Msg
viewPackages model packages =
    let
        normalizedQuery =
            String.toLower model.query |> String.trim

        matchingPackages =
            List.filter (String.contains normalizedQuery << String.toLower << .name) packages
    in
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
            [ input
                [ placeholder "Search by name"
                , value model.query
                , onInput SetQuery
                ]
                []
            ]

        -- , div []
        --     [ text (toString packages) ]
        -- , div [] [ encodeGraph packages |> E.encode 4 |> text ]
        , div []
            [ Table.view tableConfig model.tableState matchingPackages ]
        ]


tableConfig : Table.Config Package Msg
tableConfig =
    Table.config
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ packageNameColumn .name
            , dependenciesColumn .dependencies
            ]
        }


packageNameColumn : (data -> String) -> Table.Column data Msg
packageNameColumn toName =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = \data -> viewPackageName (toName data)
        , sorter = Table.increasingOrDecreasingBy toName
        }


viewPackageName : String -> Table.HtmlDetails Msg
viewPackageName name =
    Table.HtmlDetails []
        [ a
            [ href (packageUrl name)
            , target "_blank"
            ]
            [ text name ]
        ]


packageUrl : String -> String
packageUrl packageName =
    "http://package.elm-lang.org/packages/" ++ packageName ++ "/latest/"


dependenciesColumn : (data -> Dependencies) -> Table.Column data Msg
dependenciesColumn toDependencies =
    Table.veryCustomColumn
        { name = "Dependencies"
        , viewData = \data -> viewDependencies (toDependencies data)
        , sorter = Table.unsortable
        }


viewDependencies : Dependencies -> Table.HtmlDetails Msg
viewDependencies dependencies =
    case dependencies of
        PackageNames names ->
            Table.HtmlDetails []
                [ ul [] (List.map dependencyListItem names) ]

        Error message ->
            Table.HtmlDetails []
                [ text message ]


dependencyListItem : String -> Html Msg
dependencyListItem name =
    li []
        [ button
            [ onClick (SetQuery name) ]
            [ text name ]
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    githubOauthSuccess GithubOauthSuccess



---- PROGRAM ----


main : Program D.Value Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
