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
                        (encode 0 (encodeGraph testPackages))
                        graphJson
            ]
        ]


testPackages : List Package
testPackages =
    [ { name = "my-package"
      , dependencies = PackageNames [ "some-dependency" ]
      }
    ]


graphJson : String
graphJson =
    "{\"nodes\":" ++ nodesJson ++ ",\"links\":" ++ linksJson ++ "}"


nodesJson : String
nodesJson =
    "[{\"id\":\"my-package\"}]"


linksJson : String
linksJson =
    "[{\"source\":\"my-package\",\"target\":\"some-dependency\",\"value\":1}]"
