module Fagdokumentasjon.Skjema exposing
    ( FagdokumentasjonSkjema
    , ValidertFagdokumentasjonSkjema
    , beskrivelse
    , beskrivelseFraValidertSkjema
    , encode
    , fagdokumentasjonType
    , feilmeldingTypeahead
    , gjørFeilmeldingKonseptSynlig
    , init
    , initValidertSkjema
    , konseptFraValidertSkjema
    , konseptStringFraValidertSkjema
    , oppdaterBeskrivelse
    , oppdaterKonsept
    , tilSkjema
    , validertSkjema
    )

import Cv.Fagdokumentasjon exposing (Fagdokumentasjon, FagdokumentasjonType(..))
import Fagdokumentasjon.Konsept as Konsept exposing (Konsept)
import Json.Encode


type FagdokumentasjonSkjema
    = UvalidertSkjema UvalidertSkjemaInfo


type alias UvalidertSkjemaInfo =
    { fagdokumentasjonType : FagdokumentasjonType
    , visFeilmeldingTypeahead : Bool
    , konsept : Maybe Konsept
    , beskrivelse : String
    }



--INIT --


init : FagdokumentasjonType -> Konsept -> String -> FagdokumentasjonSkjema
init skjemaType konsept_ beskrivelse_ =
    UvalidertSkjema
        { konsept = Just konsept_
        , visFeilmeldingTypeahead = False
        , beskrivelse = beskrivelse_
        , fagdokumentasjonType = skjemaType
        }


initValidertSkjema : FagdokumentasjonType -> Konsept -> String -> ValidertFagdokumentasjonSkjema
initValidertSkjema skjemaType konsept_ beskrivelse_ =
    ValidertSkjema
        { konsept = konsept_
        , beskrivelse = beskrivelse_
        , fagdokumentasjonType = skjemaType
        }


tilSkjema : ValidertFagdokumentasjonSkjema -> FagdokumentasjonSkjema
tilSkjema (ValidertSkjema info) =
    UvalidertSkjema
        { konsept = Just info.konsept
        , visFeilmeldingTypeahead = False
        , beskrivelse = info.beskrivelse
        , fagdokumentasjonType = info.fagdokumentasjonType
        }



--- INNHOLD ---


beskrivelse : FagdokumentasjonSkjema -> String
beskrivelse (UvalidertSkjema info) =
    info.beskrivelse


beskrivelseFraValidertSkjema : ValidertFagdokumentasjonSkjema -> String
beskrivelseFraValidertSkjema (ValidertSkjema info) =
    info.beskrivelse


konseptStringFraValidertSkjema : ValidertFagdokumentasjonSkjema -> String
konseptStringFraValidertSkjema (ValidertSkjema info) =
    Konsept.label info.konsept


konseptFraValidertSkjema : ValidertFagdokumentasjonSkjema -> Konsept
konseptFraValidertSkjema (ValidertSkjema info) =
    info.konsept


fagdokumentasjonType : FagdokumentasjonSkjema -> FagdokumentasjonType
fagdokumentasjonType (UvalidertSkjema info) =
    info.fagdokumentasjonType



--- OPPDATERING ---


oppdaterBeskrivelse : String -> FagdokumentasjonSkjema -> FagdokumentasjonSkjema
oppdaterBeskrivelse beskrivelse_ (UvalidertSkjema info) =
    UvalidertSkjema { info | beskrivelse = beskrivelse_ }


oppdaterKonsept : FagdokumentasjonSkjema -> Maybe Konsept -> FagdokumentasjonSkjema
oppdaterKonsept (UvalidertSkjema info) konsept_ =
    UvalidertSkjema { info | konsept = konsept_ }



--- FEILMELDINGER ---


feilmeldingTypeahead : FagdokumentasjonSkjema -> Maybe String
feilmeldingTypeahead (UvalidertSkjema info) =
    if info.visFeilmeldingTypeahead && info.konsept == Nothing then
        info.fagdokumentasjonType
            |> feilmeldingstekstIkkeValgtKonsept
            |> Just

    else
        Nothing


feilmeldingstekstIkkeValgtKonsept : FagdokumentasjonType -> String
feilmeldingstekstIkkeValgtKonsept fagdokumentasjonType_ =
    case fagdokumentasjonType_ of
        SvennebrevFagbrev ->
            "Velg et svennebrev/fagbrev fra listen med forslag som kommer opp"

        Mesterbrev ->
            "Velg et mesterbrev fra listen med forslag som kommer opp"

        Autorisasjon ->
            "Velg en autorisasjon fra listen med forslag som kommer opp"


gjørFeilmeldingKonseptSynlig : Bool -> FagdokumentasjonSkjema -> FagdokumentasjonSkjema
gjørFeilmeldingKonseptSynlig synlig (UvalidertSkjema info) =
    -- Skal alltid vises etter onBlur/onSubmit, så hvis den noen gang har vært True, skal den alltid fortsette å være True
    UvalidertSkjema { info | visFeilmeldingTypeahead = synlig || info.visFeilmeldingTypeahead }



--- VALIDERING ---


type ValidertFagdokumentasjonSkjema
    = ValidertSkjema ValidertSkjemaInfo


type alias ValidertSkjemaInfo =
    { fagdokumentasjonType : FagdokumentasjonType
    , konsept : Konsept
    , beskrivelse : String
    }


validertSkjema : FagdokumentasjonSkjema -> Maybe ValidertFagdokumentasjonSkjema
validertSkjema (UvalidertSkjema info) =
    case info.konsept of
        Nothing ->
            Nothing

        Just konsept_ ->
            if String.length info.beskrivelse > 200 then
                Nothing

            else
                Just
                    (ValidertSkjema
                        { fagdokumentasjonType = info.fagdokumentasjonType
                        , konsept = konsept_
                        , beskrivelse = info.beskrivelse
                        }
                    )



-- ENCODE --


encode : ValidertFagdokumentasjonSkjema -> Json.Encode.Value
encode (ValidertSkjema info) =
    Json.Encode.object
        [ ( "tittel", Json.Encode.string (Konsept.label info.konsept) )
        , ( "beskrivelse", Json.Encode.string info.beskrivelse )
        , ( "type", encodeFagdokumentasjonType info.fagdokumentasjonType )
        , ( "konseptId"
          , info.konsept
                |> Konsept.konseptId
                |> String.fromInt
                |> Json.Encode.string
          )
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
