module PersonaliaSkjema exposing (PersonaliaSkjema, fornavn)


type PersonaliaSkjema
    = PersonaliaSkjema PersonaliaSkjemaInfo


type alias PersonaliaSkjemaInfo =
    { fornavn : String
    , etternavn : String
    , fodselsdato : String
    , epost : String
    , telefonnummer : String
    , adresse : Adresse
    }


type Adresse
    = Adresse AdresseInfo


type alias AdresseInfo =
    { gateadresse : String
    , postnummer : String
    , poststed : String
    }


fornavn : PersonaliaSkjema -> String
fornavn (PersonaliaSkjema info) =
    info.fornavn


etternavn : PersonaliaSkjema -> String
etternavn (PersonaliaSkjema info) =
    info.etternavn


epost : PersonaliaSkjema -> String
epost (PersonaliaSkjema info) =
    info.epost


telefonnummer : PersonaliaSkjema -> String
telefonnummer (PersonaliaSkjema info) =
    info.telefonnummer


fodselsdato : PersonaliaSkjema -> String
fodselsdato (PersonaliaSkjema info) =
    info.fodselsdato


adresse : PersonaliaSkjema -> Adresse
adresse (PersonaliaSkjema info) =
    info.adresse
