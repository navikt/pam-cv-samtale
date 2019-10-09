module Skjema.Fagdokumentasjon exposing
    ( FagdokumentasjonSkjema
    , TypeaheadFelt(..)
    , ValidertFagdokumentasjonSkjema
    , beskrivelse
    , beskrivelseFraValidertSkjema
    , encode
    , fagdokumentasjonType
    , init
    , initValidertSkjema
    , konsept
    , konseptFraValidertSkjema
    , mapTypeaheadState
    , oppdaterBeskrivelse
    , oppdaterKonseptFelt
    , tilSkjema
    , validertSkjema
    , velgAktivtKonseptITypeahead
    , velgKonsept
    )

import Cv.Fagdokumentasjon as Fagdokumentasjon exposing (Fagdokumentasjon, FagdokumentasjonType(..))
import Json.Encode
import Konsept exposing (Konsept)
import Typeahead.TypeaheadState as TypeaheadState exposing (TypeaheadState)


type FagdokumentasjonSkjema
    = UvalidertSkjema UvalidertSkjemaInfo


type alias UvalidertSkjemaInfo =
    { fagdokumentasjonType : FagdokumentasjonType
    , konsept : TypeaheadFelt
    , beskrivelse : String
    }


type TypeaheadFelt
    = KonseptIkkeValgt (TypeaheadState Konsept)
    | KonseptValgt Konsept


type ValidertFagdokumentasjonSkjema
    = ValidertSkjema ValidertSkjemaInfo


type alias ValidertSkjemaInfo =
    { fagdokumentasjonType : FagdokumentasjonType
    , konsept : Konsept
    , beskrivelse : String
    }


beskrivelse : FagdokumentasjonSkjema -> String
beskrivelse (UvalidertSkjema info) =
    info.beskrivelse


beskrivelseFraValidertSkjema : ValidertFagdokumentasjonSkjema -> String
beskrivelseFraValidertSkjema (ValidertSkjema info) =
    info.beskrivelse


konsept : FagdokumentasjonSkjema -> TypeaheadFelt
konsept (UvalidertSkjema info) =
    info.konsept


konseptFraValidertSkjema : ValidertFagdokumentasjonSkjema -> String
konseptFraValidertSkjema (ValidertSkjema info) =
    Konsept.label info.konsept


fagdokumentasjonType : FagdokumentasjonSkjema -> FagdokumentasjonType
fagdokumentasjonType (UvalidertSkjema info) =
    info.fagdokumentasjonType


oppdaterBeskrivelse : String -> FagdokumentasjonSkjema -> FagdokumentasjonSkjema
oppdaterBeskrivelse beskrivelse_ (UvalidertSkjema info) =
    UvalidertSkjema { info | beskrivelse = beskrivelse_ }


oppdaterKonseptFelt : FagdokumentasjonSkjema -> String -> FagdokumentasjonSkjema
oppdaterKonseptFelt (UvalidertSkjema info) konseptTekst =
    case info.konsept of
        KonseptIkkeValgt typeaheadState ->
            UvalidertSkjema
                { info
                    | konsept =
                        typeaheadState
                            |> TypeaheadState.updateValue konseptTekst
                            |> KonseptIkkeValgt
                }

        KonseptValgt _ ->
            UvalidertSkjema
                { info
                    | konsept =
                        TypeaheadState.init konseptTekst
                            |> KonseptIkkeValgt
                }


mapTypeaheadState : FagdokumentasjonSkjema -> (TypeaheadState Konsept -> TypeaheadState Konsept) -> FagdokumentasjonSkjema
mapTypeaheadState (UvalidertSkjema info) funksjon =
    case info.konsept of
        KonseptValgt _ ->
            UvalidertSkjema info

        KonseptIkkeValgt typeaheadState ->
            UvalidertSkjema
                { info
                    | konsept =
                        typeaheadState
                            |> funksjon
                            |> KonseptIkkeValgt
                }


velgAktivtKonseptITypeahead : FagdokumentasjonSkjema -> FagdokumentasjonSkjema
velgAktivtKonseptITypeahead (UvalidertSkjema info) =
    case info.konsept of
        KonseptValgt _ ->
            UvalidertSkjema info

        KonseptIkkeValgt typeaheadState ->
            case TypeaheadState.getActive typeaheadState of
                Just active ->
                    UvalidertSkjema { info | konsept = KonseptValgt active }

                Nothing ->
                    UvalidertSkjema info


velgKonsept : Konsept -> FagdokumentasjonSkjema -> FagdokumentasjonSkjema
velgKonsept konsept_ (UvalidertSkjema info) =
    UvalidertSkjema { info | konsept = KonseptValgt konsept_ }


validertSkjema : FagdokumentasjonSkjema -> Maybe ValidertFagdokumentasjonSkjema
validertSkjema (UvalidertSkjema info) =
    case info.konsept of
        KonseptIkkeValgt _ ->
            Nothing

        KonseptValgt konsept_ ->
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


tilSkjema : ValidertFagdokumentasjonSkjema -> FagdokumentasjonSkjema
tilSkjema (ValidertSkjema info) =
    UvalidertSkjema
        { fagdokumentasjonType = info.fagdokumentasjonType
        , konsept = KonseptValgt info.konsept
        , beskrivelse = info.beskrivelse
        }



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



--INIT --


init : FagdokumentasjonType -> Konsept -> String -> FagdokumentasjonSkjema
init skjemaType konsept_ beskrivelse_ =
    UvalidertSkjema
        { konsept = KonseptValgt konsept_
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
