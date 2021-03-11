module Dato exposing (Dato, fromPosix, toSeed, toString, tomorrow, yesterday)

import Random exposing (Seed)
import Time exposing (Month(..), Posix)


type Dato
    = Dato Posix


fromPosix : Posix -> Dato
fromPosix posix =
    Dato posix



--- NAVIGATION ---


døgnIMillis =
    24 * 60 * 60 * 1000


tomorrow : Dato -> Dato
tomorrow (Dato posix) =
    posix
        |> Time.posixToMillis
        |> (+) døgnIMillis
        |> Time.millisToPosix
        |> fromPosix


yesterday : Dato -> Dato
yesterday (Dato posix) =
    posix
        |> Time.posixToMillis
        |> (-) døgnIMillis
        |> Time.millisToPosix
        |> fromPosix



--- FORMATERING ---


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "januar"

        Feb ->
            "februar"

        Mar ->
            "mars"

        Apr ->
            "april"

        May ->
            "mai"

        Jun ->
            "juni"

        Jul ->
            "juli"

        Aug ->
            "august"

        Sep ->
            "september"

        Oct ->
            "oktober"

        Nov ->
            "november"

        Dec ->
            "desember"


toString : Dato -> String
toString (Dato posix) =
    let
        dag =
            posix
                |> Time.toDay Time.utc
                |> String.fromInt

        måned =
            posix
                |> Time.toMonth Time.utc
                |> monthToString

        år =
            posix
                |> Time.toYear Time.utc
                |> String.fromInt
    in
    dag ++ ". " ++ måned ++ " " ++ år



--- RANDOM SEED ---


toSeed : Dato -> Seed
toSeed (Dato posix) =
    posix
        |> dateToInt
        |> Random.initialSeed


dateToInt : Posix -> Int
dateToInt posix =
    let
        year =
            Time.toYear Time.utc posix

        month =
            posix
                |> Time.toMonth Time.utc
                |> monthToInt

        day =
            Time.toDay Time.utc posix
    in
    Debug.log "int" (year * 10000 + month * 100 + day)


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
