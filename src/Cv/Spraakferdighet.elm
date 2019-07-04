module Cv.Spraakferdighet exposing (Spraakferdighet, decode, id, muntlig, skriftlig, sprak)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Spraakferdighet
    = Spraakferdighet SpraakferdighetInfo


type alias SpraakferdighetInfo =
    { id : String
    , sprak : Maybe String
    , muntlig : Maybe String
    , skriftlig : Maybe String
    }


id : Spraakferdighet -> String
id (Spraakferdighet info) =
    info.id


sprak : Spraakferdighet -> Maybe String
sprak (Spraakferdighet info) =
    info.sprak


muntlig : Spraakferdighet -> Maybe String
muntlig (Spraakferdighet info) =
    info.muntlig


skriftlig : Spraakferdighet -> Maybe String
skriftlig (Spraakferdighet info) =
    info.skriftlig



---- Decoder ----


decode : Decoder Spraakferdighet
decode =
    decodeBackendData
        |> map Spraakferdighet


decodeBackendData : Decoder SpraakferdighetInfo
decodeBackendData =
    succeed SpraakferdighetInfo
        |> required "id" string
        |> required "sprak" (nullable string)
        |> required "muntlig" (nullable string)
        |> required "skriftlig" (nullable string)
