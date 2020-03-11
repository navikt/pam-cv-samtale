module Jobbprofil.Omrade exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


type Omrade
    = Omrade OmradeInfo


type alias OmradeInfo =
    { tittel : String
    , konseptId : Int
    , kode : String
    }


tittel : Omrade -> String
tittel (Omrade info) =
    info.tittel


konseptId : Omrade -> Int
konseptId (Omrade info) =
    info.konseptId


fraEnkeltElementer : String -> Int -> String -> Omrade
fraEnkeltElementer label konsept kode =
    Omrade
        { konseptId = konsept
        , tittel = label
        , kode = kode
        }


decode : Decoder Omrade
decode =
    decodeBackendData
        |> map Omrade


decodeBackendData : Decoder OmradeInfo
decodeBackendData =
    map3 OmradeInfo
        (at [ "tittel" ] string)
        (at [ "konseptid" ] int)
        (at [ "kode" ] string)


encode : Omrade -> Json.Encode.Value
encode (Omrade info) =
    Json.Encode.object
        [ ( "konseptid", Json.Encode.int info.konseptId )
        , ( "tittel", Json.Encode.string info.tittel )
        , ( "kode", Json.Encode.string info.kode )
        ]



--- Decode Jobbprofilgeografi ---


type alias JobbprofilGeografiInfo =
    { tittel : Maybe String
    , konseptid : Maybe Int
    , kode : Maybe String
    }


decodeJobbprofilGeografi : Decoder Omrade
decodeJobbprofilGeografi =
    geografiInfoDecoder
        |> Json.Decode.andThen jobbprofilTilOmrådeDecoder


geografiInfoDecoder : Decoder JobbprofilGeografiInfo
geografiInfoDecoder =
    succeed JobbprofilGeografiInfo
        |> required "tittel" (nullable string)
        |> required "konseptid" (nullable int)
        |> required "kode" (nullable string)


jobbprofilTilOmrådeDecoder : JobbprofilGeografiInfo -> Decoder Omrade
jobbprofilTilOmrådeDecoder geografiInfo =
    case
        Maybe.map3 fraEnkeltElementer
            geografiInfo.tittel
            geografiInfo.konseptid
            geografiInfo.kode
    of
        Just value ->
            succeed value

        Nothing ->
            fail "Decoding av område feilet."
