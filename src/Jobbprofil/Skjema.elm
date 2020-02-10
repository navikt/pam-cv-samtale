module Jobbprofil.Skjema exposing
    ( JobbprofilSkjema
    , ValidertJobbprofilSkjema
    , initValidert
    )

import Jobbprofil.Jobbprofil
    exposing
        ( GeografiInfo
        , KompetanseInfo
        , StillingInfo
        , StillingKladdInfo
        )


type JobbprofilSkjema
    = JobbprofilSkjema JobbprofilSkjemaInfo


type alias JobbprofilSkjemaInfo =
    { id : Maybe Int
    , aktiv : Bool
    , stillingliste : List StillingInfo
    , stillingKladdListe : List StillingKladdInfo
    , kompetanseliste : List KompetanseInfo
    , geografiliste : List GeografiInfo
    , ansettelsesformliste : List String
    , arbeidstidliste : List String
    , arbeidsdagerliste : List String
    , arbeidstidsordningliste : List String
    , omfangsliste : List String
    , oppstart : Maybe String
    , tillatÅViseFeilmeldingStillingliste : Bool
    , tillatÅViseFeilmeldingKompetanseliste : Bool
    , tillatÅViseFeilmeldingGeografiliste : Bool
    , tillatÅViseFeilmeldingOppstart : Bool
    }


type ValidertJobbprofilSkjema
    = ValidertJobbprofilSkjema ValidertJobbprofilSkjemaInfo


type alias ValidertJobbprofilSkjemaInfo =
    { id : Maybe Int
    , aktiv : Bool
    , stillingliste : List StillingInfo
    , stillingKladdListe : List StillingKladdInfo
    , kompetanseliste : List KompetanseInfo
    , geografiliste : List GeografiInfo
    , ansettelsesformliste : List String
    , arbeidstidliste : List String
    , arbeidsdagerliste : List String
    , arbeidstidsordningliste : List String
    , omfangsliste : List String
    , oppstart : Maybe String
    }


type SeksjonValg
    = OppstartValg
    | OmfangValg
    | ArbeidsdagerValg
    | AnsettelsesformValg
    | ArbeidstidsOrdningValg
    | ArbeidstidValg


type alias ValgElement =
    { label : String
    , value : String
    }


getOptions : SeksjonValg -> List ValgElement
getOptions seksjonValg =
    case seksjonValg of
        OppstartValg ->
            [ { label = "Jeg kan begynne nå", value = "LEDIG_NAA" }
            , { label = "Jeg har 3 måneder oppsigelse", value = "ETTER_TRE_MND" }
            , { label = "Jeg kan begynne etter nærmere avtale", value = "ETTER_AVTALE" }
            ]

        OmfangValg ->
            [ { label = "Heltid", value = "HELTID" }
            , { label = "Deltid", value = "DELTID" }
            ]

        ArbeidsdagerValg ->
            [ { label = "Lørdag", value = "LOERDAG" }
            , { label = "Søndag", value = "SOENDAG" }
            ]

        AnsettelsesformValg ->
            [ { label = "Fast", value = "FAST" }
            , { label = "Vikariat", value = "VIKARIAT" }
            , { label = "Engasjement", value = "ENGASJEMENT" }
            , { label = "Prosjekt", value = "PROSJEKT" }
            , { label = "Sesong", value = "SESONG" }
            , { label = "Trainee", value = "TRAINEE" }
            , { label = "Lærling", value = "LAERLING" }
            , { label = "Selvstendig næringsdrivende", value = "SELVSTENDIG_NAERINGSDRIVENDE" }
            , { label = "Feriejobb", value = "FERIEJOBB" }
            , { label = "Annet", value = "ANNET" }
            ]

        ArbeidstidsOrdningValg ->
            [ { label = "Skift", value = "SKIFT" }
            , { label = "Vakt", value = "VAKT" }
            , { label = "Turnus", value = "TURNUS" }
            ]

        ArbeidstidValg ->
            [ { label = "Dag", value = "DAGTID" }
            , { label = "Kveld", value = "KVELD" }
            , { label = "Natt", value = "NATT" }
            ]



--- INIT ---


init : JobbprofilSkjemaInfo -> JobbprofilSkjema
init info =
    JobbprofilSkjema info


initValidert : ValidertJobbprofilSkjemaInfo -> ValidertJobbprofilSkjema
initValidert validertJobbprofilSkjemaInfo =
    ValidertJobbprofilSkjema validertJobbprofilSkjemaInfo



--- INNHOLD ---


stillingListeFraSkjema : JobbprofilSkjema -> List StillingInfo
stillingListeFraSkjema (JobbprofilSkjema info) =
    info.stillingliste


stillingKladdListeFraSkjema : JobbprofilSkjema -> List StillingKladdInfo
stillingKladdListeFraSkjema (JobbprofilSkjema info) =
    info.stillingKladdListe


kompetanseListeFraSkjema : JobbprofilSkjema -> List KompetanseInfo
kompetanseListeFraSkjema (JobbprofilSkjema info) =
    info.kompetanseliste


geografiListeFraSkjema : JobbprofilSkjema -> List GeografiInfo
geografiListeFraSkjema (JobbprofilSkjema info) =
    info.geografiliste


ansettelsesformListeFraSkjema : JobbprofilSkjema -> List String
ansettelsesformListeFraSkjema (JobbprofilSkjema info) =
    info.ansettelsesformliste


arbeidstidListeFraSkjema : JobbprofilSkjema -> List String
arbeidstidListeFraSkjema (JobbprofilSkjema info) =
    info.arbeidstidliste


arbeidstidordningListeFraSkjema : JobbprofilSkjema -> List String
arbeidstidordningListeFraSkjema (JobbprofilSkjema info) =
    info.arbeidstidsordningliste


omfangsListeFraSkjema : JobbprofilSkjema -> List String
omfangsListeFraSkjema (JobbprofilSkjema info) =
    info.omfangsliste


oppstartFraSkjema : JobbprofilSkjema -> Maybe String
oppstartFraSkjema (JobbprofilSkjema info) =
    info.oppstart



--- OPPDATERING ---


leggTilOmfang : JobbprofilSkjema -> String -> JobbprofilSkjema
leggTilOmfang (JobbprofilSkjema info) omfang =
    JobbprofilSkjema { info | omfangsliste = List.append info.omfangsliste [ omfang ] }



{-
   fjernOmfang : JobbprofilSkjema -> String -> JobbprofilSkjema
   fjernOmfang (JobbprofilSkjema info) omfang =
       JobbprofilSkjema { info | omfangsliste = List.filter (\it -> it not omfang) info.omfangsliste }
-}


oppdaterOppstart : JobbprofilSkjema -> Maybe String -> JobbprofilSkjema
oppdaterOppstart (JobbprofilSkjema info) oppstart =
    JobbprofilSkjema { info | oppstart = oppstart }



--- VALIDER ---
--- ENCODE ---
