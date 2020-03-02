module Jobbprofil.Stilling exposing (..)

import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (required)


type alias JobbprofilStillingInfo =
    { tittel : Maybe String
    , konseptid : Maybe Int
    , styrk08 : Maybe String
    }


decodeJobbprofilStilling : Decoder Yrke
decodeJobbprofilStilling =
    stillingInfoDecoder
        |> Json.Decode.andThen decodeYrke


stillingInfoDecoder : Decoder JobbprofilStillingInfo
stillingInfoDecoder =
    succeed JobbprofilStillingInfo
        |> required "tittel" (nullable string)
        |> required "konseptid" (nullable int)
        |> required "styrk08" (nullable string)


konseptIdToString : Maybe Int -> Maybe String
konseptIdToString id_ =
    Just (String.fromInt (Maybe.withDefault 1 id_))


decodeYrke : JobbprofilStillingInfo -> Decoder Yrke
decodeYrke stillingsInfo =
    case
        Maybe.map3 Yrke.fraString
            stillingsInfo.tittel
            stillingsInfo.styrk08
            (konseptIdToString stillingsInfo.konseptid)
    of
        Just value ->
            succeed value

        Nothing ->
            fail "Decoding av stilling feilet."
