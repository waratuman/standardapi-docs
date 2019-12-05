module Pages.Settings exposing
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
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Extra exposing (attributeList)
import List.Extra as List
import Route exposing (Route, pathFor)
import StandardApi
import Task
import Uniform
import Uniform.Attributes exposing (green)
import Uniform.Icons
import Url


type alias Model =
    { url : String
    , headers : List ( String, String )
    , errors :
        { url : List String
        , headers : List String
        }
    }


type Msg
    = InternalMsg InternalMsg
    | SetEnvApi StandardApi.Config


type InternalMsg
    = Submit
    | SetUrl String
    | SetHeaderName Int String
    | SetHeaderValue Int String
    | AddHeader
    | RemoveHeader Int


type alias Translator msg =
    Msg -> msg


translator :
    { a
        | internal : InternalMsg -> msg
        , setEnvApi : StandardApi.Config -> msg
    }
    -> Translator msg
translator { internal, setEnvApi } msg =
    case msg of
        InternalMsg a ->
            internal a

        SetEnvApi api ->
            setEnvApi api


init : Env -> ( Model, Cmd msg )
init ({ api } as env) =
    ( { url = Url.toString api.url
      , headers = api.headers
      , errors =
            { url = []
            , headers = []
            }
      }
    , Cmd.none
    )


update : InternalMsg -> Env -> Model -> ( Model, Cmd Msg )
update msg env model =
    case msg of
        SetUrl url ->
            ( { model | url = url }, Cmd.none )

        SetHeaderName i n ->
            let
                headers =
                    List.updateAt i (\( _, v ) -> ( n, v )) model.headers
            in
            ( { model | headers = headers }, Cmd.none )

        SetHeaderValue i v ->
            let
                headers =
                    List.updateAt i (\( n, _ ) -> ( n, v )) model.headers
            in
            ( { model | headers = headers }, Cmd.none )

        AddHeader ->
            ( { model | headers = model.headers ++ [ ( "", "" ) ] }, Cmd.none )

        RemoveHeader i ->
            ( { model | headers = List.removeAt i model.headers }, Cmd.none )

        Submit ->
            let
                errors =
                    model.errors
            in
            case Url.fromString model.url of
                Nothing ->
                    ( { model | errors = { errors | url = [ "Invalid URL" ] } }, Cmd.none )

                Just url ->
                    let
                        oldApi =
                            env.api

                        api =
                            { oldApi | url = url, headers = model.headers }
                    in
                    ( model
                    , Cmd.batch
                        [ Task.perform SetEnvApi (Task.succeed api)
                        , Nav.pushUrl env.navKey "/"
                        ]
                    )


view : Env -> Model -> Document Msg
view env ({ errors } as model) =
    { title = "[StandardAPI] Settings"
    , body =
        [ Html.map InternalMsg <|
            Uniform.card Uniform.form
                [ id "settings", onSubmit Submit ]
                [ Uniform.cardHeader []
                    [ h1 [] [ text "Settings" ]
                    ]
                , Uniform.cardBody []
                    [ Uniform.formGroup
                        (attributeList [ ( "data-uniform-error", List.head errors.url ) ])
                        [ Uniform.textInput
                            [ value model.url
                            , onInput SetUrl
                            ]
                            []
                            |> Uniform.label "Url" []
                        ]
                    , Uniform.formGroup
                        (class "headers"
                            :: attributeList [ ( "data-uniform-error", List.head errors.headers ) ]
                        )
                        [ div []
                            [ Uniform.emptyLabel "Headers"
                                []
                            , Uniform.button button
                                [ type_ "button"
                                , onClick AddHeader
                                ]
                                [ Uniform.Icons.add [] ]
                            ]
                        , case model.headers of
                            [] ->
                                div [ class "disabled" ]
                                    [ text "No headers"
                                    ]

                            headers ->
                                ul []
                                    (List.indexedMap
                                        (\i ( n, v ) ->
                                            li []
                                                [ Uniform.textInput
                                                    [ value n
                                                    , onInput (SetHeaderName i)
                                                    , placeholder "Name"
                                                    ]
                                                    []
                                                , Uniform.textInput
                                                    [ value v
                                                    , onInput (SetHeaderValue i)
                                                    , placeholder "Value"
                                                    ]
                                                    []
                                                , Uniform.button button
                                                    [ type_ "button"
                                                    , onClick (RemoveHeader i)
                                                    ]
                                                    [ Uniform.Icons.remove [] ]
                                                ]
                                        )
                                        headers
                                    )
                        ]
                    ]
                , Uniform.cardFooter []
                    [ Uniform.formGroup []
                        [ Uniform.submitInput [ green ] []
                        ]
                    ]
                ]
        ]
    }
