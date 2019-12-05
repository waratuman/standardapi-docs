module Pages exposing
    ( InternalMsg
    , Page(..)
    , Translator
    , changeRoute
    , init
    , subscriptions
    , translator
    , update
    , view
    )

import Browser exposing (Document)
import Env exposing (Env)
import Html
import Pages.Model
import Pages.Models
import Pages.NotFound as NotFound
import Pages.Root as Root
import Pages.Route
import Pages.Routes as Routes
import Pages.Settings as Settings
import Route exposing (Route)
import StandardApi
import Uniform exposing (loader)



-- import Html.Attributes


type Page
    = Loading
    | NotFound
    | Model Pages.Model.Model
    | Models Pages.Models.Model
    | Route Pages.Route.Model
    | Routes Routes.Model
    | Root Root.Model
    | Settings Settings.Model


type Msg
    = InternalMsg InternalMsg
    | SetEnvApi StandardApi.Config


type InternalMsg
    = ModelMsg Pages.Model.InternalMsg
    | ModelsMsg Pages.Models.InternalMsg
    | RouteMsg Pages.Route.InternalMsg
    | RoutesMsg Routes.InternalMsg
    | RootMsg Root.InternalMsg
    | SettingsMsg Settings.InternalMsg


type alias Translator msg =
    Msg -> msg


translator :
    { a | internal : InternalMsg -> msg, setEnvApi : StandardApi.Config -> msg }
    -> Translator msg
translator { internal, setEnvApi } msg =
    case msg of
        InternalMsg a ->
            internal a

        SetEnvApi api ->
            setEnvApi api


init : Env -> Route -> ( Page, Cmd Msg )
init env route =
    case route of
        Route.NotFound ->
            ( NotFound, Cmd.none )

        Route.Model name ->
            initPage Model modelTranslator (Pages.Model.init env name)

        Route.Models ->
            initPage Models modelsTranslator (Pages.Models.init env)

        Route.Route method path ->
            initPage Route routeTranslator (Pages.Route.init env method path)

        Route.Routes ->
            initPage Routes routesTranslator (Routes.init env)

        Route.Root ->
            initPage Root rootTranslator (Root.init env)

        Route.Settings ->
            initPage Settings settingsTranslator (Settings.init env)


initPage : (a -> Page) -> (b -> Msg) -> ( a, Cmd b ) -> ( Page, Cmd Msg )
initPage page toMsg initFn =
    Tuple.mapBoth
        page
        (Cmd.map toMsg)
        initFn


changeRoute : Env -> Route -> ( Page, Cmd Msg )
changeRoute env route =
    case route of
        Route.NotFound ->
            ( NotFound, Cmd.none )

        Route.Model name ->
            initPage Model modelTranslator (Pages.Model.init env name)

        Route.Models ->
            initPage Models modelsTranslator (Pages.Models.init env)

        Route.Route method path ->
            initPage Route routeTranslator (Pages.Route.init env method path)

        Route.Routes ->
            initPage Routes routesTranslator (Routes.init env)

        Route.Root ->
            initPage Root rootTranslator (Root.init env)

        Route.Settings ->
            initPage Settings settingsTranslator (Settings.init env)


subscriptions : Page -> Sub Msg
subscriptions page =
    Sub.none


update : InternalMsg -> Env -> Page -> ( Page, Cmd Msg )
update msg env page =
    case ( msg, page ) of
        ( ModelMsg subMsg, Model model ) ->
            Pages.Model.update subMsg env model
                |> Tuple.mapBoth Model (Cmd.map modelTranslator)

        ( ModelsMsg subMsg, Models model ) ->
            Pages.Models.update subMsg env model
                |> Tuple.mapBoth Models (Cmd.map modelsTranslator)

        ( RouteMsg subMsg, Route model ) ->
            Pages.Route.update subMsg env model
                |> Tuple.mapBoth Route (Cmd.map routeTranslator)

        ( RoutesMsg subMsg, Routes model ) ->
            Routes.update subMsg env model
                |> Tuple.mapBoth Routes (Cmd.map routesTranslator)

        ( RootMsg subMsg, Root model ) ->
            Root.update subMsg env model
                |> Tuple.mapBoth Root (Cmd.map rootTranslator)

        ( SettingsMsg subMsg, Settings model ) ->
            Settings.update subMsg env model
                |> Tuple.mapBoth Settings (Cmd.map settingsTranslator)

        _ ->
            ( page, Cmd.none )


viewPage : (a -> Msg) -> Document a -> Document Msg
viewPage toMsg pageView =
    let
        { title, body } =
            pageView
    in
    { title = title
    , body = List.map (Html.map toMsg) body
    }


view : Env -> Page -> Document Msg
view env page =
    case page of
        NotFound ->
            NotFound.view

        Loading ->
            { title = "Loading…"
            , body = [ loader [] ]
            }

        Model model ->
            viewPage modelTranslator (Pages.Model.view env model)

        Models model ->
            viewPage modelsTranslator (Pages.Models.view env model)

        Route model ->
            viewPage routeTranslator (Pages.Route.view env model)

        Routes model ->
            viewPage routesTranslator (Routes.view env model)

        Root model ->
            viewPage rootTranslator (Root.view env model)

        Settings model ->
            viewPage settingsTranslator (Settings.view env model)


modelTranslator : Pages.Model.Translator Msg
modelTranslator =
    Pages.Model.translator
        { internal = ModelMsg >> InternalMsg }


modelsTranslator : Pages.Models.Translator Msg
modelsTranslator =
    Pages.Models.translator
        { internal = ModelsMsg >> InternalMsg }


rootTranslator : Root.Translator Msg
rootTranslator =
    Root.translator
        { internal = RootMsg >> InternalMsg }


routeTranslator : Pages.Route.Translator Msg
routeTranslator =
    Pages.Route.translator
        { internal = RouteMsg >> InternalMsg }


routesTranslator : Routes.Translator Msg
routesTranslator =
    Routes.translator
        { internal = RoutesMsg >> InternalMsg }


settingsTranslator : Settings.Translator Msg
settingsTranslator =
    Settings.translator
        { internal = SettingsMsg >> InternalMsg
        , setEnvApi = SetEnvApi
        }
