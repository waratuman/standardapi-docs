module Env exposing (..)

import Browser.Navigation as Nav
import Ports exposing (alert, logStandardApiError)
import StandardApi
import StandardApi.Schema exposing (Schema)
import Url exposing (Url)


type alias Env =
    { navKey : Nav.Key
    , api : StandardApi.Config
    , schema : Maybe Schema
    , schemaTracker : Maybe String
    , schemaError : Maybe StandardApi.Error
    }


type Msg
    = InternalMsg InternalMsg


type InternalMsg
    = FetchSchema
    | ReceivedSchema (Result StandardApi.Error StandardApi.Schema.Schema)


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


init : Nav.Key -> Url -> Maybe StandardApi.Config -> Env
init navKey url config =
    { navKey = navKey
    , api =
        Maybe.withDefault
            { url = { url | path = "", host = "localhost", port_ = Just 3000 }
            , headers =
                [ ( "Api-Key", "0decde2c28925d5fe288c7bf45fc66ff" )
                ]
            , format = "application/json"
            , version = "0.1.0"
            , timeout = Nothing
            }
            config
    , schema = Nothing
    , schemaTracker = Nothing
    , schemaError = Nothing
    }


fetchSchema : Env -> ( Env, Cmd Msg )
fetchSchema =
    update FetchSchema


update : InternalMsg -> Env -> ( Env, Cmd Msg )
update msg env =
    case msg of
        FetchSchema ->
            case env.schemaTracker of
                Just _ ->
                    ( env, Cmd.none )

                Nothing ->
                    let
                        tracker =
                            Just "env/schema"
                    in
                    ( { env | schemaTracker = tracker }
                    , StandardApi.schemaRequest env.api
                        { msg = ReceivedSchema >> InternalMsg
                        , tracker = tracker
                        }
                    )

        ReceivedSchema (Ok schema) ->
            ( { env
                | schema = Just schema
                , schemaTracker = Nothing
                , schemaError = Nothing
              }
            , Cmd.none
            )

        ReceivedSchema (Err err) ->
            ( { env
                | schema = Nothing
                , schemaTracker = Nothing
                , schemaError = Just err
              }
            , Cmd.batch
                [ logStandardApiError err
                ]
            )
