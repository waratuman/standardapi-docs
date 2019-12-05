module Pages.Model exposing
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
import Set exposing (Set)
import StandardApi
import StandardApi.Schema exposing (Schema)
import Uniform exposing (loader)


type alias Model =
    { name : String
    , expandedAttributes : Set String
    }


type Msg
    = InternalMsg InternalMsg


type InternalMsg
    = ToggleColumn String


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


init : Env -> String -> ( Model, Cmd Msg )
init env name =
    ( { name = name
      , expandedAttributes = Set.empty
      }
    , Cmd.none
    )


update : InternalMsg -> Env -> Model -> ( Model, Cmd Msg )
update msg env model =
    case msg of
        ToggleColumn name ->
            let
                expandedAttributes =
                    if Set.member name model.expandedAttributes then
                        Set.remove name model.expandedAttributes

                    else
                        Set.insert name model.expandedAttributes
            in
            ( { model | expandedAttributes = expandedAttributes }, Cmd.none )


view : Env -> Model -> Document Msg
view env model =
    { title = "[StandardAPI] Model " ++ model.name
    , body =
        [ Uniform.card div
            [ id "model" ]
            [ Uniform.cardHeader [] [ h1 [] [ text model.name ] ]
            , Uniform.cardBody [] (cardBodyView env model)
            , Uniform.cardFooter [] []
            ]
        ]
    }


cardBodyView : Env -> Model -> List (Html Msg)
cardBodyView env model =
    case env.schema of
        Just schema ->
            case Dict.get model.name schema.models of
                Nothing ->
                    [ text "Not Found " ]

                Just m ->
                    [ Markdown.toHtml [ class "comment" ] m.comment
                    , Uniform.table
                        []
                        [ thead []
                            [ tr [] [ th [ colspan 3 ] [ text "Attributes" ] ]
                            , tr [ class "subheader" ]
                                [ th [] [ text "Name" ]
                                , th [] [ text "Type" ]
                                , th [] []
                                ]
                            ]
                        , tbody []
                            (Dict.foldr
                                (\name c acc ->
                                    tr [ onClick (ToggleColumn name |> InternalMsg) ]
                                        [ td [] [ text name ]
                                        , td []
                                            [ text
                                                (c.type_
                                                    ++ (if c.array then
                                                            "[]"

                                                        else
                                                            ""
                                                       )
                                                )
                                            ]
                                        , td []
                                            (if c.primaryKey then
                                                [ Uniform.pill div [ class "-fill -small" ] [ text "Primary Key" ] ]

                                             else
                                                []
                                            )
                                        ]
                                        :: (if Set.member name model.expandedAttributes then
                                                tr [ class "expanded-definition" ]
                                                    [ td [ colspan 3 ]
                                                        [ Markdown.toHtml [ class "comment" ] c.comment
                                                        , dl []
                                                            [ dt []
                                                                [ text "Default Value" ]
                                                            , dd
                                                                []
                                                                [ text (c.default |> Maybe.withDefault "") ]
                                                            , dt []
                                                                [ text "Null" ]
                                                            , dd []
                                                                [ text
                                                                    (if c.null then
                                                                        "True"

                                                                     else
                                                                        "False"
                                                                    )
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                    :: acc

                                            else
                                                acc
                                           )
                                )
                                []
                                m.attributes
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
