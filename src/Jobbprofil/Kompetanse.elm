module Jobbprofil.Kompetanse exposing
    ( Kompetanse
    , decode
    , decodeJobbprofilKompetanse
    , encode
    , konseptId
    , label
    )

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


type Kompetanse
    = Kompetanse KompetanseInfo


type alias KompetanseInfo =
    { konseptid : Int
    , label : String
    }


fraEnkeltElementer : String -> Int -> Kompetanse
fraEnkeltElementer label_ konsept =
    Kompetanse
        { konseptid = konsept
        , label = label_
        }


label : Kompetanse -> String
label (Kompetanse info) =
    info.label


konseptId : Kompetanse -> Int
konseptId (Kompetanse info) =
    info.konseptid


encode : Kompetanse -> Json.Encode.Value
encode (Kompetanse info) =
    Json.Encode.object
        [ ( "konseptid", Json.Encode.int info.konseptid )
        , ( "tittel", Json.Encode.string info.label )
        ]


decode : Decoder Kompetanse
decode =
    decodeBackendData
        |> map Kompetanse


decodeBackendData : Decoder KompetanseInfo
decodeBackendData =
    succeed KompetanseInfo
        |> required "konseptId" int
        |> required "label" string



--- Decode Jobbprofilkompetanse ---


type alias JobbprofilKompetanseInfo =
    { tittel : Maybe String
    , konseptid : Maybe Int
    }


decodeJobbprofilKompetanse : Decoder Kompetanse
decodeJobbprofilKompetanse =
    decodeKompetanseInfo
        |> Json.Decode.andThen jobbprofilTilKompetanseDecoder


decodeKompetanseInfo : Decoder JobbprofilKompetanseInfo
decodeKompetanseInfo =
    succeed JobbprofilKompetanseInfo
        |> required "tittel" (nullable string)
        |> required "konseptid" (nullable int)


jobbprofilTilKompetanseDecoder : JobbprofilKompetanseInfo -> Decoder Kompetanse
jobbprofilTilKompetanseDecoder info =
    case
        Maybe.map2 fraEnkeltElementer
            info.tittel
            info.konseptid
    of
        Just value ->
            succeed value

        Nothing ->
            fail "Decoding av kompetanse feilet."
