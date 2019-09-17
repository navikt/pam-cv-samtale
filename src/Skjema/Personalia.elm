module Skjema.Personalia exposing
    ( Felt(..)
    , PersonaliaSkjema
    , ValidertPersonaliaSkjema
    , encode
    , epost
    , epostFeilmelding
    , etternavn
    , etternavnFeilmelding
    , fodselsdato
    , fornavn
    , fornavnFeilmelding
    , gateadresse
    , init
    , oppdaterFelt
    , oppdaterPoststed
    , postnummer
    , postnummerFeilmelding
    , poststed
    , telefon
    , telefonFeilmelding
    , validerSkjema
    )

import Json.Encode
import Personalia exposing (Personalia)
import Poststed exposing (Poststed)


type PersonaliaSkjema
    = PersonaliaSkjema SkjemaInfo


type ValidertPersonaliaSkjema
    = ValidertPersonaliaSkjema SkjemaInfo


type alias SkjemaInfo =
    { fornavn : String
    , etternavn : String
    , fodselsdato : String
    , epost : String
    , telefon : String
    , gateadresse : String
    , postnummer : String
    , poststed : Maybe Poststed
    }


type Felt
    = Fornavn
    | Etternavn
    | Fodelsdato
    | Epost
    | Telefon
    | Gateadresse
    | Postnummer


fornavn : PersonaliaSkjema -> String
fornavn (PersonaliaSkjema info) =
    info.fornavn


etternavn : PersonaliaSkjema -> String
etternavn (PersonaliaSkjema info) =
    info.etternavn


epost : PersonaliaSkjema -> String
epost (PersonaliaSkjema info) =
    info.epost


telefon : PersonaliaSkjema -> String
telefon (PersonaliaSkjema info) =
    info.telefon


fodselsdato : PersonaliaSkjema -> String
fodselsdato (PersonaliaSkjema info) =
    info.fodselsdato


gateadresse : PersonaliaSkjema -> String
gateadresse (PersonaliaSkjema info) =
    info.gateadresse


postnummer : PersonaliaSkjema -> String
postnummer (PersonaliaSkjema info) =
    info.postnummer


poststed : PersonaliaSkjema -> String
poststed (PersonaliaSkjema info) =
    info.poststed
        |> Maybe.map Poststed.sted
        |> Maybe.withDefault ""


oppdaterFelt : Felt -> PersonaliaSkjema -> String -> PersonaliaSkjema
oppdaterFelt felt (PersonaliaSkjema info) input =
    case felt of
        Fornavn ->
            PersonaliaSkjema { info | fornavn = input }

        Etternavn ->
            PersonaliaSkjema { info | etternavn = input }

        Fodelsdato ->
            PersonaliaSkjema { info | fodselsdato = input }

        Epost ->
            PersonaliaSkjema { info | epost = input }

        Telefon ->
            PersonaliaSkjema { info | telefon = input }

        Gateadresse ->
            PersonaliaSkjema { info | gateadresse = input }

        Postnummer ->
            PersonaliaSkjema { info | postnummer = input }


oppdaterPoststed : Poststed -> PersonaliaSkjema -> PersonaliaSkjema
oppdaterPoststed poststed_ (PersonaliaSkjema info) =
    if Poststed.kode poststed_ == info.postnummer then
        PersonaliaSkjema { info | poststed = Just poststed_ }

    else
        PersonaliaSkjema info



--- VALIDERING ---


fornavnFeilmelding : PersonaliaSkjema -> Maybe String
fornavnFeilmelding (PersonaliaSkjema info) =
    if String.length (String.trim info.fornavn) == 0 then
        Just "Vennligst fyll inn et fornavn"

    else
        Nothing


etternavnFeilmelding : PersonaliaSkjema -> Maybe String
etternavnFeilmelding (PersonaliaSkjema info) =
    if String.length (String.trim info.etternavn) == 0 then
        Just "Vennligst fyll inn et etternavn"

    else
        Nothing


epostFeilmelding : PersonaliaSkjema -> Maybe String
epostFeilmelding (PersonaliaSkjema info) =
    if String.length (String.trim info.epost) == 0 then
        Just "Vennligst fyll inn en e-postadresse"

    else if not (String.contains "@" info.epost && String.contains "." info.epost) then
        Just "E-post er ikke gyldig. Sjekk om den inneholder @ og punktum."

    else
        Nothing


telefonFeilmelding : PersonaliaSkjema -> Maybe String
telefonFeilmelding (PersonaliaSkjema info) =
    let
        trimmedTelefon =
            String.replace " " "" info.telefon
    in
    if String.length trimmedTelefon == 0 then
        Just "Vennligst fyll inn et telefonnummer"

    else if String.toInt trimmedTelefon == Nothing then
        Just "Telefonnummer må kun inneholde tall"

    else if not (String.length trimmedTelefon == 8) then
        Just "Telefonnummer må bestå av 8 siffer"

    else
        Nothing


postnummerFeilmelding : PersonaliaSkjema -> Maybe String
postnummerFeilmelding (PersonaliaSkjema info) =
    if String.length info.postnummer == 0 then
        Nothing

    else if String.length info.postnummer /= 4 || String.toInt info.postnummer == Nothing then
        Just "Kun 4 siffer"

    else
        Nothing


validerSkjema : PersonaliaSkjema -> Maybe ValidertPersonaliaSkjema
validerSkjema ((PersonaliaSkjema info) as skjema) =
    if fornavnFeilmelding skjema /= Nothing then
        Nothing

    else if etternavnFeilmelding skjema /= Nothing then
        Nothing

    else if telefonFeilmelding skjema /= Nothing then
        Nothing

    else if epostFeilmelding skjema /= Nothing then
        Nothing

    else if postnummerFeilmelding skjema /= Nothing then
        Nothing

    else
        Just
            (ValidertPersonaliaSkjema
                { fornavn = info.fornavn
                , etternavn = info.etternavn
                , fodselsdato = info.fodselsdato
                , epost = info.epost
                , telefon = info.telefon
                , gateadresse = info.gateadresse
                , postnummer = info.postnummer
                , poststed =
                    case info.poststed of
                        Just poststed_ ->
                            if Poststed.kode poststed_ == info.postnummer then
                                Just poststed_

                            else
                                Nothing

                        Nothing ->
                            Nothing
                }
            )



--- INIT OG ENCODING ---


init : Personalia -> PersonaliaSkjema
init personalia =
    PersonaliaSkjema
        { fornavn = Personalia.fornavn personalia |> Maybe.withDefault ""
        , etternavn = Personalia.etternavn personalia |> Maybe.withDefault ""
        , fodselsdato = Personalia.fodselsdato personalia |> Maybe.withDefault ""
        , epost = Personalia.epost personalia |> Maybe.withDefault ""
        , telefon = Personalia.telefon personalia |> Maybe.withDefault ""
        , gateadresse = Personalia.gateadresse personalia |> Maybe.withDefault ""
        , postnummer = Personalia.postnummer personalia |> Maybe.withDefault ""
        , poststed = Poststed.fraPersonalia personalia
        }


encode : ValidertPersonaliaSkjema -> String -> Json.Encode.Value
encode (ValidertPersonaliaSkjema info) id =
    Json.Encode.object
        [ ( "id", Json.Encode.string id )
        , ( "fornavn", Json.Encode.string info.fornavn )
        , ( "etternavn", Json.Encode.string info.etternavn )
        , ( "fodselsdato", Json.Encode.string info.fodselsdato )
        , ( "epost", Json.Encode.string info.epost )
        , ( "telefon", Json.Encode.string info.telefon )
        , ( "gateadresse", Json.Encode.string info.gateadresse )
        , ( "postnummer", Json.Encode.string info.postnummer )
        , ( "land", Json.Encode.string "Norge" )
        , ( "lenker", Json.Encode.string "" )
        , ( "poststed"
          , info.poststed
                |> Maybe.map Poststed.sted
                |> Maybe.withDefault ""
                |> Json.Encode.string
          )
        ]
