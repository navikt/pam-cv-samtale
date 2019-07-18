module Skjema.Personalia exposing (Felt(..), PersonaliaSkjema, encode, epost, etternavn, fodselsdato, fornavn, gateadresse, init, oppdaterFelt, postnummer, poststed, telefon)

import Json.Encode
import Personalia exposing (Personalia)


type PersonaliaSkjema
    = PersonaliaSkjema PersonaliaSkjemaInfo


type alias PersonaliaSkjemaInfo =
    { fornavn : String
    , etternavn : String
    , fodselsdato : String
    , epost : String
    , telefon : String
    , gateadresse : String
    , postnummer : String
    , poststed : String
    }


type Felt
    = Fornavn
    | Etternavn
    | Fodelsdato
    | Epost
    | Telefon
    | Gateadresse
    | Postnummer
    | Poststed


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
        , poststed = Personalia.poststed personalia |> Maybe.withDefault ""
        }


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

        Poststed ->
            PersonaliaSkjema { info | poststed = input }


encode : PersonaliaSkjema -> String -> Json.Encode.Value
encode (PersonaliaSkjema info) id =
    Json.Encode.object
        [ ( "id", Json.Encode.string id )
        , ( "fornavn", Json.Encode.string info.fornavn )
        , ( "etternavn", Json.Encode.string info.etternavn )
        , ( "fodselsdato", Json.Encode.string info.fodselsdato )
        , ( "epost", Json.Encode.string info.epost )
        , ( "telefon", Json.Encode.string info.telefon )
        , ( "gateadresse", Json.Encode.string info.gateadresse )
        , ( "postnummer", Json.Encode.string info.postnummer )
        , ( "poststed", Json.Encode.string info.poststed )
        , ( "land", Json.Encode.string "Norge" )
        , ( "lenker", Json.Encode.string "" )
        ]
