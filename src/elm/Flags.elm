module Flags exposing (..)

import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Json.Encode.Extra as Encode
import StandardApi exposing (Config)
import Url exposing (Url)


encodeConfig : Config -> Encode.Value
encodeConfig { url, headers, timeout, format, version } =
    Encode.object
        [ ( "url", Encode.string (Url.toString url) )
        , ( "headers"
          , Encode.list (\( k, v ) -> Encode.list Encode.string [ k, v ])
                headers
          )
        , ( "timeout", Encode.maybe Encode.float timeout )
        , ( "format", Encode.string format )
        , ( "version", Encode.string version )
        ]


configDecoder : Url -> Decode.Decoder Config
configDecoder defaultUrl =
    Decode.succeed Config
        |> Pipeline.required "url"
            (Decode.andThen
                (\v ->
                    case Url.fromString v of
                        Just url ->
                            Decode.succeed url

                        Nothing ->
                            Decode.succeed defaultUrl
                )
                Decode.string
            )
        |> Pipeline.required "headers"
            (Decode.list
                (Decode.map2 (\k v -> ( k, v ))
                    (Decode.index 0 Decode.string)
                    (Decode.index 1 Decode.string)
                )
            )
        |> Pipeline.required "timeout" (Decode.maybe Decode.float)
        |> Pipeline.required "format" Decode.string
        |> Pipeline.required "version" Decode.string


decodeConfig : Url -> Decode.Value -> Result Decode.Error Config
decodeConfig =
    configDecoder >> Decode.field "apiConfig" >> Decode.decodeValue
