module Poststed exposing (Poststed, decode, fraPersonalia, kode, sted)

import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Personalia exposing (Personalia)


type Poststed
    = Poststed PoststedInfo


type alias PoststedInfo =
    { kode : String
    , sted : String
    }


kode : Poststed -> String
kode (Poststed info) =
    info.kode


sted : Poststed -> String
sted (Poststed info) =
    info.sted


fraPersonalia : Personalia -> Maybe Poststed
fraPersonalia personalia =
    Maybe.map2
        (\postnummer poststed -> Poststed { kode = postnummer, sted = poststed })
        (Personalia.postnummer personalia)
        (Personalia.poststed personalia)



--- DECODE ---


decode : Decoder Poststed
decode =
    succeed PoststedInfo
        |> required "kode" Json.Decode.string
        |> required "term" Json.Decode.string
        |> Json.Decode.map Poststed
