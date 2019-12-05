module Html.Extra exposing (attributeList)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Html.Events exposing (stopPropagationOn, targetValue)
import Json.Decode as Decode


attributeList : List ( String, Maybe String ) -> List (Attribute msg)
attributeList =
    List.filterMap
        (\( name, value ) ->
            Maybe.map (\v -> attribute name v) value
        )
