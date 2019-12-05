module Util exposing (Progress(..))

import StandardApi


type Progress a
    = Error StandardApi.Error
    | Loading
    | Success a
