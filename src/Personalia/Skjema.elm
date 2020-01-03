module Personalia.Skjema exposing
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
    , gjørAlleFeilmeldingerSynlig
    , gjørFeilmeldingSynligForFelt
    , init
    , oppdaterFelt
    , oppdaterPoststed
    , postnummer
    , postnummerFeilmelding
    , poststed
    , telefon
    , telefonFeilmelding
    , tilUvalidertSkjema
    , validerSkjema
    )

import Json.Encode
import Personalia.Personalia as Personalia exposing (Personalia)
import Personalia.PersonaliaId as PersonaliaId exposing (PersonaliaId)
import Personalia.Poststed as Poststed exposing (Poststed)


type PersonaliaSkjema
    = PersonaliaSkjema SkjemaInfo


type alias SkjemaInfo =
    { id : PersonaliaId
    , fornavn : String
    , etternavn : String
    , fodselsdato : String
    , epost : String
    , telefon : String
    , gateadresse : String
    , postnummer : String
    , poststed : Maybe Poststed
    , visFeilmeldingFornavn : Bool
    , visFeilmeldingEtternavn : Bool
    , visFeilmeldingEpost : Bool
    , visFeilmeldingTelefon : Bool
    , visFeilmeldingPostnummer : Bool
    }



--- INIT ---


init : Personalia -> PersonaliaSkjema
init personalia =
    PersonaliaSkjema
        { id = Personalia.id personalia
        , fornavn = Personalia.fornavn personalia |> Maybe.withDefault ""
        , etternavn = Personalia.etternavn personalia |> Maybe.withDefault ""
        , fodselsdato = Personalia.fodselsdato personalia |> Maybe.withDefault ""
        , epost = Personalia.epost personalia |> Maybe.withDefault ""
        , telefon = Personalia.telefon personalia |> Maybe.withDefault ""
        , gateadresse = Personalia.gateadresse personalia |> Maybe.withDefault ""
        , postnummer = Personalia.postnummer personalia |> Maybe.withDefault ""
        , poststed = Poststed.fraPersonalia personalia
        , visFeilmeldingFornavn = False
        , visFeilmeldingEtternavn = False
        , visFeilmeldingEpost = False
        , visFeilmeldingTelefon = False
        , visFeilmeldingPostnummer = False
        }


tilUvalidertSkjema : ValidertPersonaliaSkjema -> PersonaliaSkjema
tilUvalidertSkjema (ValidertPersonaliaSkjema info) =
    PersonaliaSkjema
        { id = info.id
        , fornavn = info.fornavn
        , etternavn = info.etternavn
        , fodselsdato = info.fodselsdato
        , epost = info.epost
        , telefon = info.telefon
        , gateadresse = info.gateadresse
        , postnummer = info.postnummer
        , poststed = info.poststed
        , visFeilmeldingFornavn = False
        , visFeilmeldingEtternavn = False
        , visFeilmeldingEpost = False
        , visFeilmeldingTelefon = False
        , visFeilmeldingPostnummer = False
        }



--- INNHOLD ---


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
    case info.poststed of
        Just poststed_ ->
            if Poststed.kode poststed_ == info.postnummer then
                Poststed.sted poststed_

            else
                ""

        Nothing ->
            ""



--- OPPDATERING ---


type Felt
    = Fornavn
    | Etternavn
    | Epost
    | Telefon
    | Gateadresse
    | Postnummer


oppdaterFelt : Felt -> PersonaliaSkjema -> String -> PersonaliaSkjema
oppdaterFelt felt (PersonaliaSkjema info) input =
    case felt of
        Fornavn ->
            PersonaliaSkjema { info | fornavn = input }

        Etternavn ->
            PersonaliaSkjema { info | etternavn = input }

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



--- FEILMELDINGER ---


fornavnFeilmelding : PersonaliaSkjema -> Maybe String
fornavnFeilmelding (PersonaliaSkjema info) =
    if info.visFeilmeldingFornavn then
        validerFornavn info.fornavn

    else
        Nothing


validerFornavn : String -> Maybe String
validerFornavn fornavn_ =
    if String.length (String.trim fornavn_) == 0 then
        Just "Vennligst fyll inn et fornavn"

    else
        Nothing


etternavnFeilmelding : PersonaliaSkjema -> Maybe String
etternavnFeilmelding (PersonaliaSkjema info) =
    if info.visFeilmeldingEtternavn then
        validerEtternavn info.etternavn

    else
        Nothing


validerEtternavn : String -> Maybe String
validerEtternavn etternavn_ =
    if String.length (String.trim etternavn_) == 0 then
        Just "Vennligst fyll inn et etternavn"

    else
        Nothing


epostFeilmelding : PersonaliaSkjema -> Maybe String
epostFeilmelding (PersonaliaSkjema info) =
    if info.visFeilmeldingEpost then
        validerEpost info.epost

    else
        Nothing


validerEpost : String -> Maybe String
validerEpost epost_ =
    if String.length (String.trim epost_) == 0 then
        Just "Vennligst fyll inn en e-postadresse"

    else if not (String.contains "@" epost_ && String.contains "." epost_) then
        Just "E-post er ikke gyldig. Sjekk om den inneholder @ og punktum."

    else
        Nothing


telefonFeilmelding : PersonaliaSkjema -> Maybe String
telefonFeilmelding (PersonaliaSkjema info) =
    if info.visFeilmeldingTelefon then
        validerTelefon info.telefon

    else
        Nothing


validerTelefon : String -> Maybe String
validerTelefon telefon_ =
    let
        trimmedTelefon =
            String.replace " " "" telefon_
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
    if info.visFeilmeldingPostnummer then
        validerPostnummer info.postnummer

    else
        Nothing


validerPostnummer : String -> Maybe String
validerPostnummer postnummer_ =
    if String.length postnummer_ == 0 then
        Nothing

    else if String.length postnummer_ /= 4 || String.toInt postnummer_ == Nothing then
        Just "Kun 4 siffer"

    else
        Nothing


gjørFeilmeldingSynligForFelt : Felt -> PersonaliaSkjema -> PersonaliaSkjema
gjørFeilmeldingSynligForFelt felt (PersonaliaSkjema info) =
    case felt of
        Fornavn ->
            PersonaliaSkjema { info | visFeilmeldingFornavn = True }

        Etternavn ->
            PersonaliaSkjema { info | visFeilmeldingEtternavn = True }

        Epost ->
            PersonaliaSkjema { info | visFeilmeldingEpost = True }

        Telefon ->
            PersonaliaSkjema { info | visFeilmeldingTelefon = True }

        Gateadresse ->
            PersonaliaSkjema info

        Postnummer ->
            PersonaliaSkjema { info | visFeilmeldingPostnummer = True }


gjørAlleFeilmeldingerSynlig : PersonaliaSkjema -> PersonaliaSkjema
gjørAlleFeilmeldingerSynlig (PersonaliaSkjema info) =
    PersonaliaSkjema
        { info
            | visFeilmeldingFornavn = True
            , visFeilmeldingEtternavn = True
            , visFeilmeldingEpost = True
            , visFeilmeldingTelefon = True
            , visFeilmeldingPostnummer = True
        }



--- VALIDERING ---


type ValidertPersonaliaSkjema
    = ValidertPersonaliaSkjema ValidertSkjemaInfo


type alias ValidertSkjemaInfo =
    { id : PersonaliaId
    , fornavn : String
    , etternavn : String
    , fodselsdato : String
    , epost : String
    , telefon : String
    , gateadresse : String
    , postnummer : String
    , poststed : Maybe Poststed
    }


validerSkjema : PersonaliaSkjema -> Maybe ValidertPersonaliaSkjema
validerSkjema (PersonaliaSkjema info) =
    if validerFornavn info.fornavn /= Nothing then
        Nothing

    else if validerEtternavn info.etternavn /= Nothing then
        Nothing

    else if validerTelefon info.telefon /= Nothing then
        Nothing

    else if validerEpost info.epost /= Nothing then
        Nothing

    else if validerPostnummer info.postnummer /= Nothing then
        Nothing

    else
        Just
            (ValidertPersonaliaSkjema
                { id = info.id
                , fornavn = info.fornavn
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



--- ENCODING ---


encode : ValidertPersonaliaSkjema -> Json.Encode.Value
encode (ValidertPersonaliaSkjema info) =
    Json.Encode.object
        [ ( "id", PersonaliaId.encode info.id )
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
