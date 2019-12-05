module Pages.NotFound exposing (view)

import Browser exposing (Document)
import Html exposing (..)


view : Document msg
view =
    { title = "[StandardAPI] Not Found"
    , body =
        [ text "Not Found"
        ]
    }
