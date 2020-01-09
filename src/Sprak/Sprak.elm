module Sprak.Sprak exposing (Språk, decode, id, muntlig, skriftlig, sprak)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Språk
    = Språk SpråkInfo


type alias SpråkInfo =
    { id : String
    , sprak : Maybe String
    , muntlig : Maybe String
    , skriftlig : Maybe String
    }


id : Språk -> String
id (Språk info) =
    info.id


sprak : Språk -> Maybe String
sprak (Språk info) =
    info.sprak


muntlig : Språk -> Maybe String
muntlig (Språk info) =
    info.muntlig


skriftlig : Språk -> Maybe String
skriftlig (Språk info) =
    info.skriftlig



---- Decoder ----


decode : Decoder Språk
decode =
    decodeBackendData
        |> map Språk


decodeBackendData : Decoder SpråkInfo
decodeBackendData =
    succeed SpråkInfo
        |> required "id" string
        |> required "sprak" (nullable string)
        |> required "muntlig" (nullable string)
        |> required "skriftlig" (nullable string)
