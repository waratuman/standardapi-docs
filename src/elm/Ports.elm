port module Ports exposing
    ( alert
    , log
    , logError
    , logStandardApiError
    , setApiConfig
    )

import Json.Encode as Encode
import Json.Encode.Extra as Encode
import StandardApi
import Url


port alert : String -> Cmd msg


port log : String -> Cmd msg


port logError : String -> Cmd msg


logStandardApiError : StandardApi.Error -> Cmd msg
logStandardApiError err =
    case err of
        StandardApi.BadUrl url ->
            logError <| "BadUrl \"" ++ url ++ "\""

        StandardApi.Timeout ->
            logError "Timeout."

        StandardApi.NetworkError ->
            logError "NetworkError"

        StandardApi.BadStatus status body ->
            logError <| "BadStatus " ++ String.fromInt status ++ "\n" ++ body

        StandardApi.BadBody body ->
            logError <| "BadBody \"" ++ body ++ "\""


setApiConfig : Maybe StandardApi.Config -> Cmd msg
setApiConfig settings =
    case settings of
        Just s ->
            encodeConfig s
                |> storeApiConfig

        Nothing ->
            deleteApiConfig ()


port storeApiConfig : Encode.Value -> Cmd msg


port deleteApiConfig : () -> Cmd msg


encodeConfig : StandardApi.Config -> Encode.Value
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
