module Route exposing (Route(..), fromUrl, parser)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | Team String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Team (s "team" </> string)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    url
        |> Parser.parse parser
