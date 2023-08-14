module Pages.MinSide exposing (Model, Msg, page)

import AccessToken exposing (AccessToken)
import Api
import Auth
import Css
import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Url.Builder as Url
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared.apiKey
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type Model
    = Loading
    | Failure Http.Error
    | Success (List Team)


type alias Team =
    { name : String
    , shortname : String
    }


init : Auth.User -> DatabaseApiToken -> () -> ( Model, Effect Msg )
init user apiKey () =
    ( Loading
    , Api.getFromDatabase
        { apiKey = apiKey
        , table = "team"
        , query =
            [ Url.string "owner_id" ("eq." ++ AccessToken.userId user.accessToken)
            , Url.string "select" "name, shortname"
            ]
        , expect = Http.expectJson GetTeamsResponse (Json.Decode.list teamDecoder)
        }
    )


teamDecoder : Decoder Team
teamDecoder =
    Json.Decode.succeed Team
        |> required "name" Json.Decode.string
        |> required "shortname" Json.Decode.string



-- UPDATE


type Msg
    = GetTeamsResponse (Result Http.Error (List Team))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GetTeamsResponse (Ok teams) ->
            ( Success teams
            , Effect.none
            )

        GetTeamsResponse (Err error) ->
            ( Failure error
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Min side"
    , body =
        case model of
            Loading ->
                []

            Failure error ->
                []

            Success teams ->
                [ h2 [] [ text "Mine team" ]
                , div [ Attributes.css [ Css.displayFlex, Css.flexDirection Css.column ] ]
                    (teams
                        |> List.map viewTeam
                    )
                ]
    }


viewTeam : Team -> Html msg
viewTeam team =
    a [ Attributes.href (Route.Path.toString (Route.Path.Team_ { team = team.shortname })) ]
        [ text team.name ]
