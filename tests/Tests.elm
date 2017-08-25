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
                        (encode 0 (encodeGraph [ testPackage ]))
                        graphJson
            ]
        ]


testPackage : InitialPackage
testPackage =
    { name = "some-user/some-package"
    , dependencies = PackageNames [ "some-dependency" ]
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
