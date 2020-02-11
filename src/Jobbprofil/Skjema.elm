module Jobbprofil.Skjema exposing
    ( JobbprofilSkjema
    , ValidertJobbprofilSkjema
    , fraJobbprofil
    , initValidert
    )

import Jobbprofil.Jobbprofil as Jobbprofil exposing (GeografiInfo, Jobbprofil, KompetanseInfo, StillingInfo, StillingKladdInfo)


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
        -- RADIO BUTTON --
        OppstartValg ->
            [ { label = "Jeg kan begynne nå", value = "LEDIG_NAA" }
            , { label = "Jeg har 3 måneder oppsigelse", value = "ETTER_TRE_MND" }
            , { label = "Jeg kan begynne etter nærmere avtale", value = "ETTER_AVTALE" }
            ]

        -- CHECKBOXES --
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


fraJobbprofil : Jobbprofil -> JobbprofilSkjema
fraJobbprofil jobbprofil =
    JobbprofilSkjema
        { id = Just (Jobbprofil.id jobbprofil)
        , aktiv = Jobbprofil.aktiv jobbprofil
        , stillingliste = Jobbprofil.stillingliste jobbprofil
        , stillingKladdListe = Jobbprofil.stillingKladdListe jobbprofil
        , kompetanseliste = Jobbprofil.kompetanseliste jobbprofil
        , geografiliste = Jobbprofil.geografiliste jobbprofil
        , ansettelsesformliste = Jobbprofil.ansettelsesformliste jobbprofil
        , arbeidstidliste = Jobbprofil.arbeidstidliste jobbprofil
        , arbeidsdagerliste = Jobbprofil.arbeidsdagerliste jobbprofil
        , arbeidstidsordningliste = Jobbprofil.arbeidstidsordningliste jobbprofil
        , omfangsliste = Jobbprofil.omfangsliste jobbprofil
        , oppstart = Jobbprofil.oppstart jobbprofil
        , tillatÅViseFeilmeldingStillingliste = False
        , tillatÅViseFeilmeldingKompetanseliste = False
        , tillatÅViseFeilmeldingGeografiliste = False
        , tillatÅViseFeilmeldingOppstart = False
        }



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


leggTilStilling : JobbprofilSkjema -> StillingInfo -> JobbprofilSkjema
leggTilStilling (JobbprofilSkjema info) stilling =
    JobbprofilSkjema { info | stillingliste = List.append info.stillingliste [ stilling ] }


fjernStilling : JobbprofilSkjema -> StillingInfo -> JobbprofilSkjema
fjernStilling (JobbprofilSkjema info) stilling =
    JobbprofilSkjema { info | stillingliste = List.filter (\it -> it.id /= stilling.id) info.stillingliste }


leggTilStillingKladd : JobbprofilSkjema -> StillingKladdInfo -> JobbprofilSkjema
leggTilStillingKladd (JobbprofilSkjema info) stillingKladd =
    JobbprofilSkjema { info | stillingKladdListe = List.append info.stillingKladdListe [ stillingKladd ] }


fjernStillingKladd : JobbprofilSkjema -> StillingKladdInfo -> JobbprofilSkjema
fjernStillingKladd (JobbprofilSkjema info) stillingKladd =
    JobbprofilSkjema { info | stillingKladdListe = List.filter (\it -> it.id /= stillingKladd.id) info.stillingKladdListe }


leggTilKompetanse : JobbprofilSkjema -> KompetanseInfo -> JobbprofilSkjema
leggTilKompetanse (JobbprofilSkjema info) kompetanse =
    JobbprofilSkjema { info | kompetanseliste = List.append info.kompetanseliste [ kompetanse ] }


fjernKompetanse : JobbprofilSkjema -> KompetanseInfo -> JobbprofilSkjema
fjernKompetanse (JobbprofilSkjema info) kompetanse =
    JobbprofilSkjema { info | kompetanseliste = List.filter (\it -> it.id /= kompetanse.id) info.kompetanseliste }


leggTilGeografi : JobbprofilSkjema -> GeografiInfo -> JobbprofilSkjema
leggTilGeografi (JobbprofilSkjema info) geografi =
    JobbprofilSkjema { info | geografiliste = List.append info.geografiliste [ geografi ] }


fjernGeografi : JobbprofilSkjema -> GeografiInfo -> JobbprofilSkjema
fjernGeografi (JobbprofilSkjema info) geografi =
    JobbprofilSkjema { info | geografiliste = List.filter (\it -> it.id /= geografi.id) info.geografiliste }


leggTilOmfang : JobbprofilSkjema -> String -> JobbprofilSkjema
leggTilOmfang (JobbprofilSkjema info) omfang =
    JobbprofilSkjema { info | omfangsliste = List.append info.omfangsliste [ omfang ] }


fjernOmfang : JobbprofilSkjema -> String -> JobbprofilSkjema
fjernOmfang (JobbprofilSkjema info) omfang =
    JobbprofilSkjema { info | omfangsliste = List.filter (\it -> it /= omfang) info.omfangsliste }


leggTilAnsettelsesForm : JobbprofilSkjema -> String -> JobbprofilSkjema
leggTilAnsettelsesForm (JobbprofilSkjema info) ansettelsesForm =
    JobbprofilSkjema { info | ansettelsesformliste = List.append info.ansettelsesformliste [ ansettelsesForm ] }


fjernAnsettelsesForm : JobbprofilSkjema -> String -> JobbprofilSkjema
fjernAnsettelsesForm (JobbprofilSkjema info) ansettelsesForm =
    JobbprofilSkjema { info | ansettelsesformliste = List.filter (\it -> it /= ansettelsesForm) info.ansettelsesformliste }


oppdaterOppstart : JobbprofilSkjema -> Maybe String -> JobbprofilSkjema
oppdaterOppstart (JobbprofilSkjema info) oppstart =
    JobbprofilSkjema { info | oppstart = oppstart }



--- VALIDER ---
--- ENCODE ---
