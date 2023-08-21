module Pages.Team_.Admin exposing (Model, Msg, page)

import AccessToken exposing (AccessToken)
import Api
import Auth
import Css
import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Team exposing (Team)
import Teammedlem exposing (Teammedlem)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route { team : String } -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared.apiKey route.params.team user.accessToken
        , update = update shared.apiKey user.accessToken
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type Model
    = Loading
    | Failure Team.Error
    | NotTeamOwner Team
    | TeamOwner TeamOwnerModel


type alias TeamOwnerModel =
    { team : Team
    , medlemmer : List TeamMedlemState
    }


type TeamMedlemState
    = InitialMedlemState Teammedlem


init : DatabaseApiToken -> String -> AccessToken -> () -> ( Model, Effect Msg )
init apiKey shortName accessToken () =
    ( Loading
    , Api.getTeam HentTeamResponse apiKey shortName
    )



-- UPDATE


type Msg
    = HentTeamResponse (Result Team.Error Team)


update : DatabaseApiToken -> AccessToken -> Msg -> Model -> ( Model, Effect Msg )
update apiKey accessToken msg model =
    case msg of
        HentTeamResponse (Ok team) ->
            ( if Team.hasOwner team (AccessToken.userId accessToken) then
                TeamOwner
                    { team = team
                    , medlemmer = initTeammedlemmer team
                    }

              else
                NotTeamOwner team
            , Effect.none
            )

        HentTeamResponse (Err error) ->
            ( Failure error
            , Effect.none
            )


initTeammedlemmer : Team -> List TeamMedlemState
initTeammedlemmer team =
    team
        |> Team.medlemmer
        |> List.map InitialMedlemState



-- VIEW


view : Model -> View Msg
view model =
    { title = ""
    , body =
        case model of
            Loading ->
                []

            Failure _ ->
                [ text "Det skjedde en feil ved lastingen av siden" ]

            NotTeamOwner _ ->
                [ text "Du er ikke eier av dette teamet." ]

            TeamOwner teamOwnerModel ->
                viewTeamOwner teamOwnerModel
    }


viewTeamOwner : TeamOwnerModel -> List (Html Msg)
viewTeamOwner model =
    [ viewInnstillinger model
    , viewTeammedlemmer model.medlemmer
    ]


viewInnstillinger : TeamOwnerModel -> Html Msg
viewInnstillinger model =
    text ""


viewTeammedlemmer : List TeamMedlemState -> Html Msg
viewTeammedlemmer medlemmer =
    pre [ Attributes.css [ Css.whiteSpace Css.normal ] ] [ text (Debug.toString medlemmer) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
