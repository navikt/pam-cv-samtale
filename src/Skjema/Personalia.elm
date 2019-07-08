module Skjema.Personalia exposing (PersonaliaSkjema, fornavn)

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


type Adresse
    = Adresse AdresseInfo


type alias AdresseInfo =
    {}


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
