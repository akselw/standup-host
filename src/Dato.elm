module Dato exposing (Dato, fromPosix, nesteArbeidsdag, toSeed, toString, toUkedagString)

import Random exposing (Seed)
import Time exposing (Month(..), Posix, Weekday(..))


type Dato
    = Dato Posix


fromPosix : Posix -> Dato
fromPosix posix =
    Dato posix



--- NAVIGATION ---


døgnIMillis =
    24 * 60 * 60 * 1000


nesteArbeidsdag : Dato -> Dato
nesteArbeidsdag (Dato posix) =
    posix
        |> Time.posixToMillis
        |> (+) (dagerTilNesteArbeidsdag posix * døgnIMillis)
        |> Time.millisToPosix
        |> fromPosix


dagerTilNesteArbeidsdag : Posix -> Int
dagerTilNesteArbeidsdag posix =
    case Time.toWeekday Time.utc posix of
        Mon ->
            1

        Tue ->
            1

        Wed ->
            1

        Thu ->
            1

        Fri ->
            3

        Sat ->
            2

        Sun ->
            1



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


toUkedagString : Dato -> String
toUkedagString (Dato posix) =
    case Time.toWeekday Time.utc posix of
        Mon ->
            "Mandag"

        Tue ->
            "Tirsdag"

        Wed ->
            "Onsdag"

        Thu ->
            "Torsdag"

        Fri ->
            "Fredag"

        Sat ->
            "Lørdag"

        Sun ->
            "Søndag"



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
    year * 10000 + month * 100 + day


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
