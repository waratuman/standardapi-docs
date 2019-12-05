module Pages.Route exposing
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
import List.Extra as List
import Ports exposing (alert, logError, logStandardApiError)
import Route exposing (Route, pathFor)
import StandardApi
import StandardApi.Schema exposing (Schema)
import Uniform exposing (loader)


type alias Model =
    { method : String
    , path : String
    }


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


init : Env -> String -> String -> ( Model, Cmd Msg )
init env method path =
    ( { method = method
      , path = path
      }
    , Cmd.none
    )


update : InternalMsg -> Env -> Model -> ( Model, Cmd Msg )
update msg env model =
    case msg of
        None ->
            ( model, Cmd.none )


view : Env -> Model -> Document Msg
view env model =
    { title = "[StandardAPI] Route " ++ model.method ++ " " ++ model.path
    , body =
        [ Uniform.card div
            [ id "route" ]
            (env.schema
                |> Maybe.andThen
                    (\schema ->
                        List.find (\r -> r.method == model.method && r.path == model.path)
                            schema.routes
                    )
                |> Maybe.map (cardView model)
                |> Maybe.withDefault
                    [ Uniform.cardHeader []
                        [ h1 [] [ text (model.method ++ " " ++ model.path) ] ]
                    , Uniform.cardBody []
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
                    ]
            )
        ]
    }


cardView : Model -> StandardApi.Schema.Route -> List (Html Msg)
cardView model route =
    [ Uniform.cardHeader []
        (h1 [] [ text (model.method ++ " " ++ model.path) ]
            :: (route.model
                    |> Maybe.map (\m -> [ a [ href (Route.pathFor (Route.Model m.name)) ] [ text m.name ] ])
                    |> Maybe.withDefault []
               )
        )
    , Uniform.cardBody []
        []
    ]
