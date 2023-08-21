module Pages.Team_.Admin exposing (Model, Msg, page)

import AccessToken exposing (AccessToken)
import Api
import Auth
import Css
import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)
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
    , medlemmer : List ( Teammedlem, TeamMedlemState )
    }


type TeamMedlemState
    = InitialMedlemState


init : DatabaseApiToken -> String -> AccessToken -> () -> ( Model, Effect Msg )
init apiKey shortName accessToken () =
    ( Loading
    , Api.getTeam HentTeamResponse apiKey shortName
    )



-- UPDATE


type Msg
    = HentTeamResponse (Result Team.Error Team)
    | SuccessMsg SuccessMsg


type SuccessMsg
    = RedigerKnappTrykket Teammedlem
    | SlettKnappTrykket Teammedlem


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

        SuccessMsg successMsg ->
            case model of
                TeamOwner teamOwnerModel ->
                    let
                        ( newModel, effect ) =
                            successUpdate apiKey accessToken successMsg teamOwnerModel
                    in
                    ( TeamOwner newModel, effect )

                _ ->
                    ( model
                    , Effect.none
                    )


initTeammedlemmer : Team -> List ( Teammedlem, TeamMedlemState )
initTeammedlemmer team =
    team
        |> Team.medlemmer
        |> List.map (\medlem -> ( medlem, InitialMedlemState ))


successUpdate : DatabaseApiToken -> AccessToken -> SuccessMsg -> TeamOwnerModel -> ( TeamOwnerModel, Effect Msg )
successUpdate apiKey accessToken msg model =
    case msg of
        RedigerKnappTrykket teammedlem ->
            ( model
            , Effect.none
            )

        SlettKnappTrykket teammedlem ->
            ( model
            , Effect.none
            )



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
                    |> List.map (Html.map SuccessMsg)
    }


viewTeamOwner : TeamOwnerModel -> List (Html SuccessMsg)
viewTeamOwner model =
    [ viewInnstillinger model
    , viewTeammedlemmer model.medlemmer
    ]


viewInnstillinger : TeamOwnerModel -> Html SuccessMsg
viewInnstillinger model =
    text ""


viewTeammedlemmer : List ( Teammedlem, TeamMedlemState ) -> Html SuccessMsg
viewTeammedlemmer medlemmer =
    div
        [ Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "8px"
            ]
        ]
        (List.map viewTeammedlem medlemmer)


viewTeammedlem : ( Teammedlem, TeamMedlemState ) -> Html SuccessMsg
viewTeammedlem ( medlem, medlemState ) =
    div [ Attributes.css [ Css.padding (Css.px 16) ] ]
        [ case medlemState of
            InitialMedlemState ->
                div []
                    [ text (Teammedlem.navn medlem)
                    , button [ onClick (RedigerKnappTrykket medlem) ] [ text "Rediger" ]
                    , button [ onClick (SlettKnappTrykket medlem) ] [ text "Slett" ]
                    ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
