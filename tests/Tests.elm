module Tests exposing (..)

import Expect
import Json.Encode exposing (encode)
import Main exposing (..)
import String
import Test exposing (..)


all : Test
all =
    describe "Main"
        [ describe "encodeGraph"
            [ test "encoding" <|
                \() ->
                    Expect.equal
                        (encode 0 (encodeGraph [ testInitialPackage ]))
                        graphJson
            ]
        , describe "dependents"
            [ test "gives packages from potentials dependent on given package" <|
                let
                    dependent =
                        Package
                            "some-dependent/package"
                            [ testPackage.name ]
                            (Just 4)
                            Nothing

                    potentials =
                        [ testPackage
                        , dependent
                        , Package
                            "some-other/package"
                            []
                            Nothing
                            Nothing
                        ]
                in
                \() ->
                    Expect.equal
                        (dependents potentials testPackage)
                        [ dependent ]
            ]
        ]


testPackage : Package
testPackage =
    Package
        testInitialPackage.name
        testInitialPackage.dependencies
        (Just 5)
        (Just [ "stuff", "things" ])


testInitialPackage : InitialPackage
testInitialPackage =
    { name = "some-user/some-package"
    , dependencies = [ "some-dependency" ]
    }


graphJson : String
graphJson =
    """{"nodes":""" ++ nodesJson ++ ""","links":""" ++ linksJson ++ "}"


nodesJson : String
nodesJson =
    """[{"id":"some-user/some-package"}]"""


linksJson : String
linksJson =
    """[{"source":"some-user/some-package","target":"some-dependency","value":1}]"""
