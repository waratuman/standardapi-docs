module Pages.Routes exposing
    ( InternalMsg
    , Model
    , Msg(..)
    , Translator
    , init
    , translator
    , update
    , view
    )

import Browser exposing (Document)
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Extra exposing (attributeList)
import Ports exposing (alert, logError, logStandardApiError)
import Route exposing (Route(..), pathFor)
import StandardApi
import StandardApi.Schema exposing (Schema)
import Uniform exposing (loader)


type alias Model =
    { search : String }


type Msg
    = InternalMsg InternalMsg


type InternalMsg
    = SetSearch String


type alias Translator msg =
    Msg -> msg


translator :
    { a
        | internal : InternalMsg -> msg
    }
    -> Translator msg
translator { internal } msg =
    case msg of
        InternalMsg a ->
            internal a


init : Env -> ( Model, Cmd Msg )
init env =
    ( { search = "" }
    , Cmd.none
    )


update : InternalMsg -> Env -> Model -> ( Model, Cmd Msg )
update msg env model =
    case msg of
        SetSearch search ->
            ( { model | search = String.trim search }, Cmd.none )


view : Env -> Model -> Document Msg
view env model =
    { title = "[StandardAPI] Routes"
    , body =
        [ Uniform.card div
            [ id "routes" ]
            [ Uniform.cardHeader []
                [ Uniform.textInput
                    [ placeholder "Search Routes"
                    , value model.search
                    , onInput (SetSearch >> InternalMsg)
                    ]
                    []
                ]
            , Uniform.cardBody []
                (case env.schema of
                    Just schema ->
                        cardBodyView model schema

                    Nothing ->
                        [ case env.schemaTracker of
                            Just _ ->
                                loader []

                            Nothing ->
                                case env.schemaError of
                                    Just err ->
                                        text (StandardApi.errorToString err)

                                    Nothing ->
                                        text "unkown error loading schema"
                        ]
                )
            ]
        ]
    }


cardBodyView : Model -> Schema -> List (Html Msg)
cardBodyView model schema =
    let
        routes =
            case model.search of
                "" ->
                    schema.routes

                _ ->
                    let
                        search =
                            String.toLower model.search
                    in
                    List.filter (\r -> String.contains search (String.toLower r.path))
                        schema.routes
    in
    [ Uniform.table
        []
        [ tbody []
            (routes
                |> List.map
                    (\route ->
                        let
                            link =
                                Route.pathFor (Route route.method route.path)
                        in
                        tr []
                            [ td []
                                [ a [ href link ] [ text route.method ]
                                ]
                            , td []
                                [ a [ href link ] [ text route.path ]
                                ]
                            , td []
                                [ case route.model of
                                    Nothing ->
                                        a [ href link ] []

                                    Just m ->
                                        a [ href link ]
                                            [ a [ href (Route.pathFor (Route.Model m.name)) ]
                                                [ text m.name
                                                ]
                                            ]
                                ]
                            ]
                    )
            )
        ]
    ]
