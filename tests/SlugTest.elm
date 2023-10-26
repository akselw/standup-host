module SlugTest exposing (..)

import Expect exposing (Expectation)
import Slug exposing (Error(..))
import Test exposing (..)


suite : Test
suite =
    describe "The Slug module"
        [ describe "Slug.fromString"
            -- Nest as many descriptions as you like.
            [ test "rejects strings with spaces" <|
                \_ ->
                    "test test"
                        |> Slug.fromString
                        |> Expect.equal (Err WrongFormat)
            , test "accepts strings with only letters" <|
                \_ ->
                    "test"
                        |> Slug.fromString
                        |> Expect.ok
            ]
        ]
