module Jobbprofil.JobbprofilValg exposing (..)


type SeksjonValg
    = OppstartValg
    | OmfangValg
    | AnsettelsesformValg
    | ArbeidstidValg


type alias ValgElement =
    { label : Maybe String
    , value : String
    }


label : ValgElement -> String
label elem =
    case elem.label of
        Nothing ->
            ""

        Just verdi ->
            verdi


value : ValgElement -> String
value elem =
    elem.value


hentValg : SeksjonValg -> List ValgElement
hentValg seksjonValg =
    case seksjonValg of
        -- RADIO BUTTON --
        OppstartValg ->
            [ { label = Just "Jeg kan begynne nå", value = "LEDIG_NAA" }
            , { label = Just "Jeg har 3 måneder oppsigelse", value = "ETTER_TRE_MND" }
            , { label = Just "Jeg kan begynne etter nærmere avtale", value = "ETTER_AVTALE" }
            ]

        -- CHECKBOXES --
        OmfangValg ->
            [ { label = Just "Heltid", value = "HELTID" }
            , { label = Just "Deltid", value = "DELTID" }
            ]

        AnsettelsesformValg ->
            [ { label = Just "Fast", value = "FAST" }
            , { label = Just "Vikariat", value = "VIKARIAT" }
            , { label = Just "Engasjement", value = "ENGASJEMENT" }
            , { label = Just "Prosjekt", value = "PROSJEKT" }
            , { label = Just "Sesong", value = "SESONG" }
            , { label = Just "Trainee", value = "TRAINEE" }
            , { label = Just "Lærling", value = "LAERLING" }
            , { label = Just "Selvstendig næringsdrivende", value = "SELVSTENDIG_NAERINGSDRIVENDE" }
            , { label = Just "Feriejobb", value = "FERIEJOBB" }
            , { label = Just "Annet", value = "ANNET" }
            ]

        ArbeidstidValg ->
            [ { label = Just "Dag", value = "DAGTID" }
            , { label = Just "Kveld", value = "KVELD" }
            , { label = Just "Natt", value = "NATT" }
            , { label = Just "Lørdag", value = "LOERDAG" }
            , { label = Just "Søndag", value = "SOENDAG" }
            , { label = Just "Skift", value = "SKIFT" }
            , { label = Just "Vakt", value = "VAKT" }
            , { label = Just "Turnus", value = "TURNUS" }
            ]


arbeidstidValg : List String
arbeidstidValg =
    [ "Dag", "Kveld", "Natt" ]


arbeidsdagValg : List String
arbeidsdagValg =
    [ "Lørdag", "Søndag" ]


arbeidstidsordningValg : List String
arbeidstidsordningValg =
    [ "Skift", "Vakt", "Turnus" ]


arbeidstidTilBackendString : String -> String
arbeidstidTilBackendString tid =
    case tid of
        "Skift" ->
            "SKIFT"

        "Vakt" ->
            "VAKT"

        "Turnus" ->
            "TURNUS"

        "Lørdag" ->
            "LOERDAG"

        "Søndag" ->
            "SOENDAG"

        "Dag" ->
            "DAGTID"

        "Kveld" ->
            "KVELD"

        "Natt" ->
            "NATT"

        _ ->
            ""


ansettelsesFormTilBackendString : String -> String
ansettelsesFormTilBackendString input =
    case input of
        "Selvstendig næringsdrivende" ->
            "SELVSTENDIG_NAERINGSDRIVENDE"

        "Lærling" ->
            "LAERLING"

        _ ->
            String.toUpper input


oppstartTilBackendString : String -> String
oppstartTilBackendString input =
    case input of
        "Jeg kan begynne nå" ->
            "LEDIG_NAA"

        "Jeg har 3 måneder oppsigelse" ->
            "ETTER_TRE_MND"

        "Jeg kan begynne etter nærmere avtale" ->
            "ETTER_AVTALE"

        _ ->
            ""
