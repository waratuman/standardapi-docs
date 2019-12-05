module Pages.Root exposing
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
import Markdown
import Ports exposing (alert, logError, logStandardApiError)
import Route exposing (Route, pathFor)
import StandardApi
import StandardApi.Schema exposing (Schema)
import Uniform exposing (loader)


type alias Model =
    {}


type Msg
    = InternalMsg InternalMsg


type InternalMsg
    = None


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
    ( {}
    , Cmd.none
    )


update : InternalMsg -> Env -> Model -> ( Model, Cmd Msg )
update msg env model =
    case msg of
        None ->
            ( model, Cmd.none )


view : Env -> Model -> Document Msg
view env model =
    { title = "StandardAPI"
    , body =
        [ Uniform.card div
            [ id "root" ]
            [ Uniform.cardHeader []
                [ h1 [] [ text "Schema" ]
                ]
            , Uniform.cardBody []
                (case env.schema of
                    Just schema ->
                        [ Markdown.toHtml [] schema.comment ]

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
