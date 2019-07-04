module Cv.Forerkort exposing (Forerkort, decode, fraDato, id, klasse, utloperDato)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Forerkort
    = Forerkort ForerkortInfo


type alias ForerkortInfo =
    { id : String
    , klasse : String
    , fraDato : Maybe String
    , utloperDato : Maybe String
    }


id : Forerkort -> String
id (Forerkort info) =
    info.id


klasse : Forerkort -> String
klasse (Forerkort info) =
    info.klasse


fraDato : Forerkort -> Maybe String
fraDato (Forerkort info) =
    info.fraDato


utloperDato : Forerkort -> Maybe String
utloperDato (Forerkort info) =
    info.utloperDato



---- Decoder ----


decode : Decoder Forerkort
decode =
    decodeBackendData
        |> map Forerkort


decodeBackendData : Decoder ForerkortInfo
decodeBackendData =
    succeed ForerkortInfo
        |> required "id" string
        |> required "klasse" string
        |> required "fraDato" (nullable string)
        |> required "utloperDato" (nullable string)
