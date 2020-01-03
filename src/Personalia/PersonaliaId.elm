module Personalia.PersonaliaId exposing (PersonaliaId, decode, encode, toString)

import Json.Decode exposing (Decoder)
import Json.Encode


type PersonaliaId
    = PersonaliaId String


toString : PersonaliaId -> String
toString (PersonaliaId id) =
    id


decode : Decoder PersonaliaId
decode =
    Json.Decode.string
        |> Json.Decode.map PersonaliaId


encode : PersonaliaId -> Json.Encode.Value
encode (PersonaliaId id) =
    Json.Encode.string id
