module Example exposing (..)

import Client exposing (removeLastWord)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Client Tests"
        [ describe "View Tests"
            [ test "RemoveLastWord will remove last word" <|
                \_ ->
                    let
                        input =
                            "This is a test"

                        expected =
                            "This is a"
                    in
                    Expect.equal expected (removeLastWord input)
            ]
        ]
