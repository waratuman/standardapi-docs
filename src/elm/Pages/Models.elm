module Pages.Models exposing
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
import Browser.Navigation as Nav
import Dict
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
    { title = "[StandardAPI] Models"
    , body =
        [ Uniform.card div
            [ id "models" ]
            [ Uniform.cardHeader []
                [ Uniform.textInput
                    [ placeholder "Search Models"
                    , value model.search
                    , onInput (SetSearch >> InternalMsg)
                    ]
                    []
                ]
            , Uniform.cardBody []
                (case env.schema of
                    Just schema ->
                        let
                            models =
                                case model.search of
                                    "" ->
                                        schema.models

                                    _ ->
                                        let
                                            search =
                                                String.toLower model.search
                                        in
                                        Dict.filter (\k _ -> String.contains search (String.toLower k))
                                            schema.models
                        in
                        [ Uniform.table
                            []
                            [ tbody []
                                (Dict.keys models
                                    |> List.map
                                        (\name ->
                                            let
                                                link =
                                                    Route.pathFor (Route.Model name)
                                            in
                                            tr []
                                                [ td []
                                                    [ a [ href link ] [ text name ]
                                                    ]
                                                ]
                                        )
                                )
                            ]
                        ]

                    Nothing ->
                        case env.schemaTracker of
                            Just _ ->
                                [ loader [] ]

                            Nothing ->
                                case env.schemaError of
                                    Just err ->
                                        [ text (StandardApi.errorToString err) ]

                                    Nothing ->
                                        [ text "unkown error loading schema" ]
                )
            , Uniform.cardFooter []
                []
            ]
        ]
    }
