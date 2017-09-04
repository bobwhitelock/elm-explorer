port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Table


---- PORTS ----


port githubOauthSuccess : (String -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { packages : PackagesData
    , tableState : Table.State
    , query : Query
    }


type PackagesData
    = InitialLoadErrored String
    | InitialDataLoaded (List InitialPackage)
    | AuthedWithGithub GithubAuthedData
    | GithubDataLoaded GithubLoadedData
    | GithubLoadErrored Http.Error


type alias GithubAuthedData =
    AuthedData InitialPackage


type alias GithubLoadedData =
    AuthedData Package


type alias AuthedData packageType =
    { githubAccessToken : String
    , packages : List packageType
    }


type alias InitialPackage =
    { name : String
    , dependencies : Dependencies
    }


type alias Package =
    { initialPackage : InitialPackage
    , stars : Maybe Int
    , topics : Maybe (List String)
    }


type alias GithubPackageData =
    { stars : Int
    , topics : List String
    }


type Dependencies
    = PackageNames (List String)
      -- Only errors retrieving `elm-package.json` appear to be 404s, due to:
      -- - main branch not called `master`;
      -- - package repo deleted;
      -- - `elm.json` now also appears valid.
    | Error String


type alias Query =
    { type_ : QueryType
    , string : String
    }


type QueryType
    = NameQuery
    | TopicQuery


init : D.Value -> ( Model, Cmd Msg )
init packages =
    ( { packages = decodePackages packages
      , tableState = Table.initialSort "Stars"
      , query = Query NameQuery ""
      }
    , Cmd.none
    )


decodePackages : D.Value -> PackagesData
decodePackages packagesJson =
    let
        decodeResult =
            D.decodeValue packagesDecoder packagesJson
    in
    case decodeResult of
        Ok initialPackages ->
            InitialDataLoaded initialPackages

        Err message ->
            InitialLoadErrored message


packagesDecoder : Decoder (List InitialPackage)
packagesDecoder =
    D.keyValuePairs elmPackageJsonDecoder
        |> D.map
            (List.map
                (\( name, dependencies ) -> InitialPackage name dependencies)
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


encodeGraph : List InitialPackage -> E.Value
encodeGraph packages =
    E.object
        [ ( "nodes", encodeNodes packages )
        , ( "links", encodeLinks packages )
        ]


encodeNodes : List InitialPackage -> E.Value
encodeNodes packages =
    let
        encodePackage =
            \package ->
                E.object
                    [ ( "id", E.string package.name )
                    ]
    in
    E.list (List.map encodePackage packages)


encodeLinks : List InitialPackage -> E.Value
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
    | LoadPackagesData (Result Http.Error (List (Maybe GithubPackageData)))
    | SetTableState Table.State
    | SetQuery Query
    | SetQueryType QueryType
    | ClearQuery


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GithubOauthSuccess token ->
            case model.packages of
                InitialDataLoaded packages ->
                    let
                        newModel =
                            { model
                                | packages =
                                    AuthedWithGithub
                                        { githubAccessToken = token
                                        , packages = packages
                                        }
                            }
                    in
                    ( newModel, requestPackagesData newModel )

                _ ->
                    ( model, Cmd.none )

        LoadPackagesData result ->
            case model.packages of
                AuthedWithGithub { packages, githubAccessToken } ->
                    case result of
                        Ok githubPackagesData ->
                            ( { model
                                | packages =
                                    GithubDataLoaded
                                        { githubAccessToken = githubAccessToken
                                        , packages = initialToFullPackages packages githubPackagesData
                                        }
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( { model
                                | packages = GithubLoadErrored error
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        SetQueryType queryType ->
            let
                currentQuery =
                    model.query

                newQuery =
                    { currentQuery | type_ = queryType }
            in
            ( { model | query = newQuery }
            , Cmd.none
            )

        -- XXX Change to `SetQueryString`?
        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        ClearQuery ->
            let
                currentQuery =
                    model.query

                newQuery =
                    { currentQuery | string = "" }
            in
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


initialToFullPackages : List InitialPackage -> List (Maybe GithubPackageData) -> List Package
initialToFullPackages initialPackages githubPackagesData =
    let
        combinePackageData =
            \initialPackage ->
                \githubData ->
                    case githubData of
                        Just data ->
                            Package initialPackage (Just data.stars) (Just data.topics)

                        Nothing ->
                            Package initialPackage Nothing Nothing
    in
    -- Need to reverse Github data as returned in returned in reverse order for
    -- some reason; doing this appears to match up each initial package with
    -- its Github data correctly - but may be better to explicitly do this to
    -- guarantee this?
    List.reverse githubPackagesData
        |> List.map2 combinePackageData initialPackages


requestPackagesData : Model -> Cmd Msg
requestPackagesData model =
    -- XXX Pass in just data needed so don't need `case` here?
    case model.packages of
        AuthedWithGithub { githubAccessToken, packages } ->
            Http.send LoadPackagesData (postForPackagesData githubAccessToken packages)

        _ ->
            Cmd.none


postForPackagesData : String -> List InitialPackage -> Http.Request (List (Maybe GithubPackageData))
postForPackagesData githubAccessToken packages =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("bearer " ++ githubAccessToken) ]
        , url = "https://api.github.com/graphql"
        , body = Http.jsonBody (packagesGraphqlQuery packages)
        , expect = Http.expectJson packagesDataDecoder
        , timeout = Nothing
        , withCredentials = False
        }


packagesGraphqlQuery : List InitialPackage -> E.Value
packagesGraphqlQuery packages =
    let
        packagesGraphql =
            List.map graphqlSnippetFor packages
                |> List.indexedMap
                    (\index ->
                        \snippet ->
                            aliasFor index ++ snippet
                    )
                |> String.concat

        aliasFor =
            \index -> "e" ++ toString index ++ ": "

        fragmentGraphql =
            """fragment GithubPackageData on Repository {
                owner {
                    login
              }
              name
              stargazers {
                  totalCount
              }
              repositoryTopics(first: 30) {
                  edges {
                      node {
                          topic {
                              name
                    }
                  }
                }
              }
            }"""

        queryData =
            "{" ++ packagesGraphql ++ "}" ++ fragmentGraphql
    in
    E.object
        [ ( "query"
          , E.string queryData
          )
        ]


graphqlSnippetFor : InitialPackage -> String
graphqlSnippetFor package =
    let
        packageNameParts =
            String.split "/" package.name

        owner =
            List.head packageNameParts

        name =
            List.tail packageNameParts
                |> Maybe.andThen List.head

        ownerAndName =
            Maybe.map2 (,) owner name
    in
    case ownerAndName of
        Just ( owner, name ) ->
            "repository(owner: \""
                ++ owner
                ++ "\", name: \""
                ++ name
                ++ "\") { ...GithubPackageData }"

        Nothing ->
            ""


packagesDataDecoder : Decoder (List (Maybe GithubPackageData))
packagesDataDecoder =
    D.field "data"
        (D.keyValuePairs
            (D.oneOf
                [ D.map2 GithubPackageData
                    (D.field "stargazers"
                        (D.field "totalCount" D.int)
                    )
                    (D.field "repositoryTopics"
                        (D.field
                            "edges"
                            (D.list
                                (D.field "node"
                                    (D.field "topic"
                                        (D.field "name" D.string)
                                    )
                                )
                            )
                        )
                    )
                    |> D.map Just
                , D.succeed Nothing
                ]
            )
            |> D.map (List.map Tuple.second)
        )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        packagesView =
            case model.packages of
                InitialDataLoaded _ ->
                    viewStartAuth

                InitialLoadErrored message ->
                    div [] [ text message ]

                AuthedWithGithub _ ->
                    viewRetrievingGithubData

                GithubDataLoaded { packages } ->
                    viewPackages model packages

                GithubLoadErrored _ ->
                    div [] [ text "Oh no" ]
    in
    div [] [ packagesView ]


viewStartAuth : Html Msg
viewStartAuth =
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


viewRetrievingGithubData : Html Msg
viewRetrievingGithubData =
    text "Loading data from GitHub..."


viewPackages : Model -> List Package -> Html Msg
viewPackages model packages =
    let
        matchingPackages =
            packagesMatchingQuery model.query packages

        numberPackagesShown =
            List.length matchingPackages |> toString
    in
    div []
        [ div []
            [ div []
                [ queryTypeRadio model.query NameQuery
                , queryTypeRadio model.query TopicQuery
                ]
            , input
                [ placeholder (descriptionForQueryType model.query.type_)
                , value model.query.string
                , onInput (Query model.query.type_ >> SetQuery)
                ]
                []
            , button [ onClick ClearQuery ] [ text "X" ]
            ]
        , div [] [ text (numberPackagesShown ++ " packages") ]

        -- , div []
        --     [ text (toString packages) ]
        -- , div [] [ encodeGraph packages |> E.encode 4 |> text ]
        , div []
            [ Table.view packagesTableConfig model.tableState matchingPackages ]
        ]


descriptionForQueryType : QueryType -> String
descriptionForQueryType queryType =
    case queryType of
        NameQuery ->
            "Search by Name"

        TopicQuery ->
            "Search by Topic"


queryTypeRadio : Query -> QueryType -> Html Msg
queryTypeRadio currentQuery queryType =
    label []
        [ input
            [ type_ "radio"
            , checked (currentQuery.type_ == queryType)
            , onClick (SetQueryType queryType)
            ]
            []
        , text (descriptionForQueryType queryType)
        ]


packagesMatchingQuery : Query -> List Package -> List Package
packagesMatchingQuery query packages =
    case query.type_ of
        NameQuery ->
            let
                normalizedQuery =
                    String.toLower query.string |> String.trim
            in
            List.filter
                (String.contains normalizedQuery << String.toLower << packageName)
                packages

        TopicQuery ->
            -- XXX handle this
            []


packageName : Package -> String
packageName package =
    package.initialPackage.name


packageDependencies : Package -> Dependencies
packageDependencies package =
    package.initialPackage.dependencies


packagesTableConfig : Table.Config Package Msg
packagesTableConfig =
    Table.config
        { toId = packageName
        , toMsg = SetTableState
        , columns =
            [ packageNameColumn packageName
            , starsColumn .stars
            , topicsColumn .topics
            , dependenciesColumn packageDependencies
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
            [ onClick (SetQuery (Query NameQuery name)) ]
            [ text name ]
        ]


starsColumn : (data -> Maybe Int) -> Table.Column data Msg
starsColumn toStars =
    let
        orderableStarValue =
            \data ->
                toStars data
                    |> Maybe.withDefault -1
    in
    Table.customColumn
        { name = "Stars"
        , viewData = \data -> viewStars (toStars data)
        , sorter = Table.decreasingOrIncreasingBy orderableStarValue
        }


viewStars : Maybe Int -> String
viewStars maybeStars =
    Maybe.map toString maybeStars
        |> Maybe.withDefault "Could not retrieve"


topicsColumn : (data -> Maybe (List String)) -> Table.Column data Msg
topicsColumn toTopics =
    Table.veryCustomColumn
        { name = "Topics"
        , viewData = \data -> viewTopics (toTopics data)
        , sorter = Table.unsortable
        }


viewTopics : Maybe (List String) -> Table.HtmlDetails Msg
viewTopics topics =
    case topics of
        Just topics ->
            Table.HtmlDetails [] [ viewTopicsList topics ]

        Nothing ->
            Table.HtmlDetails [] [ text "Could not retrieve" ]


viewTopicsList : List String -> Html Msg
viewTopicsList topics =
    let
        topicListItem =
            \topic -> li [] [ text topic ]
    in
    if List.isEmpty topics then
        text "None defined"
    else
        ul [] (List.map topicListItem topics)



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
