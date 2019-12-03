module AndreSider.HeaderInfo exposing (HeaderInfo, decode, fornavn, navn, underOppfølging)

import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)


type HeaderInfo
    = HeaderInfo
        { fornavn : String
        , etternavn : String
        , underOppfølging : Bool
        }



--- INNHOLD ---


fornavn : HeaderInfo -> String
fornavn (HeaderInfo info) =
    info.fornavn


navn : HeaderInfo -> String
navn (HeaderInfo info) =
    [ info.fornavn, info.etternavn ]
        |> List.filter (not << String.isEmpty << String.trim)
        |> String.join " "


underOppfølging : HeaderInfo -> Bool
underOppfølging (HeaderInfo info) =
    info.underOppfølging



--- DECODING ---


decode : Decoder HeaderInfo
decode =
    decodeBackendData
        |> Json.Decode.map tilHeaderInfo


decodeBackendData : Decoder BackendData
decodeBackendData =
    succeed BackendData
        |> required "fornavn" (Json.Decode.nullable Json.Decode.string)
        |> required "etternavn" (Json.Decode.nullable Json.Decode.string)
        |> required "underOppfolging" Json.Decode.bool


tilHeaderInfo : BackendData -> HeaderInfo
tilHeaderInfo backendData =
    HeaderInfo
        { fornavn =
            backendData.fornavn
                |> Maybe.withDefault ""
        , etternavn =
            backendData.etternavn
                |> Maybe.withDefault ""
        , underOppfølging = backendData.underOppfolging
        }


type alias BackendData =
    { fornavn : Maybe String
    , etternavn : Maybe String
    , underOppfolging : Bool
    }
