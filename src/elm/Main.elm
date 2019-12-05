module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation as Nav
import Env exposing (Env)
import Flags
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Pages exposing (Page)
import Ports exposing (alert, logError, logStandardApiError)
import Route exposing (Route, pathFor)
import StandardApi
import StandardApi.Schema exposing (Schema)
import Task
import Uniform
import Uniform.Attributes
import Uniform.Icons
import Url exposing (Url)


type alias Model =
    { env : Env
    , page : Page
    }


type Msg
    = UrlChange Url
    | UrlRequest UrlRequest
    | EnvMsg Env.InternalMsg
    | PagesMsg Pages.InternalMsg
    | SetEnvApi StandardApi.Config


main : Program Decode.Value Model Msg
main =
    application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init jsonFlags url navKey =
    let
        apiConfig =
            Flags.decodeConfig url jsonFlags
                |> Result.toMaybe

        ( env, envCmd ) =
            Env.init navKey url apiConfig
                |> Env.fetchSchema

        route =
            Route.parseUrl url
    in
    changeRoute route
        { env = env
        , page = Pages.Loading
        }
        |> Tuple.mapSecond
            (\cmd ->
                Cmd.batch
                    [ Cmd.map envTranslator envCmd
                    , cmd
                    ]
            )


changeRoute : Route -> Model -> ( Model, Cmd Msg )
changeRoute route model =
    let
        ( page, pageCmd ) =
            Pages.changeRoute model.env route
    in
    ( { model | page = page }, Cmd.map pagesTranslator pageCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ env } as model) =
    case msg of
        UrlChange url ->
            changeUrl url model

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.env.navKey (Url.toString url) )

                -- Remove when https://github.com/elm/browser/issues/34 is resolved
                Browser.External "" ->
                    ( model, Cmd.none )

                Browser.External href ->
                    ( model, Nav.load href )

        EnvMsg envMsg ->
            let
                ( newEnv, envCmd ) =
                    Env.update envMsg model.env
            in
            ( { model | env = newEnv }, Cmd.map envTranslator envCmd )

        PagesMsg pageMsg ->
            let
                ( page, pageCmd ) =
                    Pages.update pageMsg env model.page
            in
            ( { model | page = page }, Cmd.map pagesTranslator pageCmd )

        SetEnvApi api ->
            ( { model | env = { env | api = api } }, Ports.setApiConfig (Just api) )


view : Model -> Document Msg
view model =
    let
        { title, body } =
            Pages.view model.env model.page
    in
    { title = title
    , body =
        Uniform.mainNav header
            [ Uniform.Attributes.invert ]
            [ ul []
                [ li [] [ a [ href "/" ] [ text "StandardAPI" ] ]
                , li []
                    [ a
                        [ classList [ ( "active", routesTabActive model ) ], href (Route.pathFor Route.Routes) ]
                        [ text "Routes" ]
                    ]
                , li []
                    [ a
                        [ classList [ ( "active", modelsTabActive model ) ]
                        , href (Route.pathFor Route.Models)
                        ]
                        [ text "Models" ]
                    ]
                ]
            , Uniform.dropdown div
                [ Uniform.Attributes.invert ]
                [ Uniform.Icons.menu []
                , ul []
                    [ li []
                        [ a [ href (pathFor Route.Settings) ] [ text "Settings" ]
                        ]
                    ]
                ]
            ]
            :: List.map (Html.map pagesTranslator) body
    }


routesTabActive : Model -> Bool
routesTabActive { page } =
    case page of
        Pages.Route _ ->
            True

        Pages.Routes _ ->
            True

        _ ->
            False


modelsTabActive : Model -> Bool
modelsTabActive { page } =
    case page of
        Pages.Model _ ->
            True

        Pages.Models _ ->
            True

        _ ->
            False


envTranslator : Env.Translator Msg
envTranslator =
    Env.translator { internal = EnvMsg }


pagesTranslator : Pages.Translator Msg
pagesTranslator =
    Pages.translator { internal = PagesMsg, setEnvApi = SetEnvApi }


changeUrl : Url -> Model -> ( Model, Cmd Msg )
changeUrl url model =
    changeRoute (Route.parseUrl url) model
