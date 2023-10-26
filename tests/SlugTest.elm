module SlugTest exposing (..)

import Expect exposing (Expectation)
import Slug exposing (Error(..))
import Test exposing (..)


suite : Test
suite =
    describe "The Slug module"
        [ describe "Slug.fromString"
            -- Nest as many descriptions as you like.
            [ test "accepts strings with only letters" <|
                \_ ->
                    "test"
                        |> Slug.fromString
                        |> Expect.ok
            , test "rejects strings with spaces" <|
                \_ ->
                    "test test"
                        |> Slug.fromString
                        |> Expect.equal (Err WrongFormat)
            , test "reject strings with leading dash" <|
                \_ ->
                    "-test"
                        |> Slug.fromString
                        |> Expect.equal (Err WrongFormat)
            , test "reject strings with trailing dash" <|
                \_ ->
                    "test-"
                        |> Slug.fromString
                        |> Expect.equal (Err WrongFormat)
            , test "reject strings with uppercase letters" <|
                \_ ->
                    "Test"
                        |> Slug.fromString
                        |> Expect.equal (Err WrongFormat)
            , test "reject strings with more than one dash after one another" <|
                \_ ->
                    "test--test"
                        |> Slug.fromString
                        |> Expect.equal (Err WrongFormat)
            , test "reject strings in blacklist: settings" <|
                \_ ->
                    "settings"
                        |> Slug.fromString
                        |> Expect.equal (Err SlugInBlacklist)
            , test "reject strings in blacklist: mine-team" <|
                \_ ->
                    "mine-team"
                        |> Slug.fromString
                        |> Expect.equal (Err SlugInBlacklist)
            ]
        ]
