module Skjema.Fagdokumentasjon exposing (FagdokumentasjonSkjema(..), FagdokumentasjonSkjemaInfo, beskrivelse, encode, fagdokumentasjonType, init, konseptid, tittel)

import Cv.Fagdokumentasjon as Fagdokumentasjon exposing (Fagdokumentasjon, FagdokumentasjonType(..))
import Json.Encode


type FagdokumentasjonSkjema
    = FagdokumentasjonSkjema FagdokumentasjonSkjemaInfo


type alias FagdokumentasjonSkjemaInfo =
    { fagdokumentasjonType : FagdokumentasjonType
    , tittel : String
    , konseptId : String
    , beskrivelse : String
    }


fagdokumentasjonType : FagdokumentasjonSkjema -> FagdokumentasjonType
fagdokumentasjonType (FagdokumentasjonSkjema info) =
    info.fagdokumentasjonType


tittel : FagdokumentasjonSkjema -> String
tittel (FagdokumentasjonSkjema info) =
    info.tittel


konseptid : FagdokumentasjonSkjema -> String
konseptid (FagdokumentasjonSkjema info) =
    info.konseptId


beskrivelse : FagdokumentasjonSkjema -> String
beskrivelse (FagdokumentasjonSkjema info) =
    info.beskrivelse



-- ENCODE --


encode : FagdokumentasjonSkjema -> String -> FagdokumentasjonType -> Json.Encode.Value
encode (FagdokumentasjonSkjema info) id fagtype =
    Json.Encode.object
        [ ( "id", Json.Encode.string id )
        , ( "tittel", Json.Encode.string info.tittel )
        , ( "konseptId", Json.Encode.string info.konseptId )
        , ( "beskrivelse", Json.Encode.string info.beskrivelse )
        , ( "type", encodeFagdokumentasjonType fagtype )
        ]


encodeFagdokumentasjonType : FagdokumentasjonType -> Json.Encode.Value
encodeFagdokumentasjonType fagtype =
    case fagtype of
        SvennebrevFagbrev ->
            Json.Encode.string "SVENNEBREV_FAGBREV"

        Mesterbrev ->
            Json.Encode.string "MESTERBREV"

        Autorisasjon ->
            Json.Encode.string "AUTORISASJON"



--INIT --


init : FagdokumentasjonType -> String -> String -> String -> FagdokumentasjonSkjema
init skjemaType skjemaTittel skjemaKonseptid skjemaBeskrivelse =
    FagdokumentasjonSkjema
        { tittel = skjemaTittel
        , konseptId = skjemaKonseptid
        , beskrivelse = skjemaBeskrivelse
        , fagdokumentasjonType = skjemaType
        }
