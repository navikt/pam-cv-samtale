module Skjema.Sprak exposing (Ferdighet(..), SpråkSkjema, SpråkSkjemaInfo, encode, init, norskFørstespråk, språkNavn)

import Cv.Spraakferdighet as Spraakferdighet exposing (Spraakferdighet)
import Json.Encode
import SpråkKode exposing (SpråkKode)


type SpråkSkjema
    = SpråkSkjema SpråkSkjemaInfo


type alias SpråkSkjemaInfo =
    { språk : SpråkKode
    , muntlig : Ferdighet
    , skriftlig : Ferdighet
    }


type Ferdighet
    = Nybegynner
    | Godt
    | VeldigGodt
    | Førstespråk


ferdighetTilString : Ferdighet -> String
ferdighetTilString ferdighet =
    case ferdighet of
        Nybegynner ->
            "NYBEGYNNER"

        Godt ->
            "GODT"

        VeldigGodt ->
            "VELDIG_GODT"

        Førstespråk ->
            "FOERSTESPRAAK"


norskFørstespråk =
    SpråkSkjema
        { språk = SpråkKode.norsk
        , muntlig = Førstespråk
        , skriftlig = Førstespråk
        }


språkNavn : SpråkSkjema -> String
språkNavn (SpråkSkjema info) =
    SpråkKode.term info.språk


init : SpråkSkjemaInfo -> SpråkSkjema
init info =
    SpråkSkjema info


encode : SpråkSkjema -> Json.Encode.Value
encode (SpråkSkjema info) =
    Json.Encode.object
        [ ( "sprak", Json.Encode.string (SpråkKode.term info.språk) )
        , ( "muntlig", Json.Encode.string (ferdighetTilString info.muntlig) )
        , ( "skriftlig", Json.Encode.string (ferdighetTilString info.skriftlig) )
        ]
