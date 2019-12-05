module Route exposing (..)

import Url exposing (Url)
import Url.Builder exposing (QueryParameter)
import Url.Parser exposing (..)
import Url.Parser.Query as Query


type Route
    = NotFound
    | Model String
    | Models
    | Route String String
    | Routes
    | Root
    | Settings


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Model (s "models" </> string)
        , map Models (s "models")
        , map Routes (s "routes")
        , map
            (\method path ->
                String.join "/" path
                    |> String.append "/"
                    |> Route method
            )
            (s "routes" </> string </> star)
        , map Root top
        , map Settings (s "settings")
        ]


parseUrl : Url -> Route
parseUrl url =
    case parse parser url of
        Just r ->
            r

        Nothing ->
            NotFound


pathFor : Route -> String
pathFor r =
    case r of
        NotFound ->
            Url.Builder.absolute [ "not-found" ] []

        Model name ->
            Url.Builder.absolute [ "models", name ] []

        Models ->
            Url.Builder.absolute [ "models" ] []

        Route method path ->
            let
                path_ =
                    if String.startsWith "/" path then
                        String.dropLeft 1 path

                    else
                        path
            in
            Url.Builder.absolute [ "routes", method, path_ ] []

        Routes ->
            Url.Builder.absolute [ "routes" ] []

        Root ->
            Url.Builder.absolute [] []

        Settings ->
            Url.Builder.absolute [ "settings" ] []


star : Parser (List String -> a) a
star =
    starHelp 10


starHelp : Int -> Parser (List String -> a) a
starHelp maxDepth =
    if maxDepth < 1 then
        map [] top

    else
        oneOf
            [ map [] top
            , map (\str li -> str :: li) (string </> starHelp2 (maxDepth - 1))
            ]


starHelp2 : Int -> Parser (List String -> a) a
starHelp2 maxDepth =
    if maxDepth < 1 then
        map [] top

    else
        oneOf
            [ map [] top
            , map (\str li -> str :: li) (string </> starHelp (maxDepth - 1))
            ]
