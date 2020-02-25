module Jobbprofil.Skjema exposing (..)

import Arbeidserfaring.Yrke exposing (Yrke)
import Jobbprofil.Jobbprofil as Jobbprofil exposing (GeografiInfo, Jobbprofil, KompetanseInfo, StillingInfo, StillingKladdInfo)
import Jobbprofil.Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade exposing (Omrade)
import Jobbprofil.StegInfo exposing (KompetanseStegInfo)
import Maybe.Extra


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
    }


type ValidertSkjema
    = ValidertSkjema ValidertSkjemaInfo


type alias ValidertSkjemaInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List String
    , arbeidstider : List String
    , ansettelsesformer : List String
    , oppstart : String
    , kompetanser : List Kompetanse
    }


type UvalidertSkjema
    = UvalidertSkjema UvalidertSkjemaInfo


type alias UvalidertSkjemaInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List String
    , arbeidstider : List String
    , ansettelsesformer : List String
    , oppstart : String
    , kompetanser : List Kompetanse
    , visYrkerFeilmelding : Bool
    , visKompetanserFeilmelding : Bool
    , visOmraderFeilmelding : Bool
    , visOppstartFeilmelding : Bool
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



--- INIT ---


init : JobbprofilSkjemaInfo -> JobbprofilSkjema
init info =
    JobbprofilSkjema info


initValidert : ValidertJobbprofilSkjemaInfo -> ValidertJobbprofilSkjema
initValidert validertJobbprofilSkjemaInfo =
    ValidertJobbprofilSkjema validertJobbprofilSkjemaInfo


tilValidertSkjema : KompetanseStegInfo -> ValidertSkjema
tilValidertSkjema info =
    ValidertSkjema
        { yrker = info.yrker
        , omrader = info.omrader
        , omfanger = info.omfanger
        , arbeidstider = info.arbeidstider
        , ansettelsesformer = info.ansettelsesformer
        , oppstart = info.oppstart
        , kompetanser = info.kompetanser
        }


tilUvalidertSkjema : ValidertSkjema -> UvalidertSkjema
tilUvalidertSkjema (ValidertSkjema skjema) =
    UvalidertSkjema
        { yrker = skjema.yrker
        , omrader = skjema.omrader
        , omfanger = skjema.omfanger
        , arbeidstider = skjema.arbeidstider
        , ansettelsesformer = skjema.ansettelsesformer
        , oppstart = skjema.oppstart
        , kompetanser = skjema.kompetanser
        , visYrkerFeilmelding = False
        , visKompetanserFeilmelding = False
        , visOmraderFeilmelding = False
        , visOppstartFeilmelding = False
        }


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
        }



--- INNHOLD ---


stillingListeFraSkjema : JobbprofilSkjema -> List StillingInfo
stillingListeFraSkjema (JobbprofilSkjema info) =
    info.stillingliste


stillingSammendragFraSkjema : JobbprofilSkjema -> String
stillingSammendragFraSkjema info =
    stillingListeFraSkjema info
        |> List.map (\it -> it.tittel)
        |> Maybe.Extra.values
        |> String.join ", "


stillingKladdListeFraSkjema : JobbprofilSkjema -> List StillingKladdInfo
stillingKladdListeFraSkjema (JobbprofilSkjema info) =
    info.stillingKladdListe


kompetanseListeFraSkjema : JobbprofilSkjema -> List KompetanseInfo
kompetanseListeFraSkjema (JobbprofilSkjema info) =
    info.kompetanseliste


kompetanseSammendragFraSkjema : JobbprofilSkjema -> String
kompetanseSammendragFraSkjema info =
    kompetanseListeFraSkjema info
        |> List.map (\it -> it.tittel)
        |> Maybe.Extra.values
        |> String.join ", "


geografiListeFraSkjema : JobbprofilSkjema -> List GeografiInfo
geografiListeFraSkjema (JobbprofilSkjema info) =
    info.geografiliste


geografiSammendragFraSkjema : JobbprofilSkjema -> String
geografiSammendragFraSkjema info =
    geografiListeFraSkjema info
        |> List.map (\it -> it.tittel)
        |> Maybe.Extra.values
        |> String.join ", "


ansettelsesformListeFraSkjema : JobbprofilSkjema -> List String
ansettelsesformListeFraSkjema (JobbprofilSkjema info) =
    info.ansettelsesformliste


ansettelsesformSammendragFraSkjema : JobbprofilSkjema -> String
ansettelsesformSammendragFraSkjema info =
    ansettelsesformListeFraSkjema info
        |> listeSammendragFraSkjema (hentValg AnsettelsesformValg) " - "


arbeidstidListeFraSkjema : JobbprofilSkjema -> List String
arbeidstidListeFraSkjema (JobbprofilSkjema info) =
    info.arbeidstidliste


arbeidstidSammendragFraSkjema : JobbprofilSkjema -> String
arbeidstidSammendragFraSkjema info =
    arbeidstidListeFraSkjema info
        |> listeSammendragFraSkjema (hentValg ArbeidstidValg) ", "


omfangsListeFraSkjema : JobbprofilSkjema -> List String
omfangsListeFraSkjema (JobbprofilSkjema info) =
    info.omfangsliste


omfangsSammendragFraSkjema : JobbprofilSkjema -> String
omfangsSammendragFraSkjema info =
    omfangsListeFraSkjema info
        |> listeSammendragFraSkjema (hentValg OmfangValg) " - "


listeSammendragFraSkjema : List ValgElement -> String -> List String -> String
listeSammendragFraSkjema valg separator info =
    List.map
        (\i ->
            List.filterMap
                (\v ->
                    if i == v.value then
                        v.label

                    else
                        Nothing
                )
                valg
        )
        info
        |> List.foldr (++) []
        |> String.join separator


oppstartFraSkjema : JobbprofilSkjema -> Maybe String
oppstartFraSkjema (JobbprofilSkjema info) =
    info.oppstart


oppstartSammendragFraSkjema : JobbprofilSkjema -> String
oppstartSammendragFraSkjema info =
    [ Maybe.withDefault "" (oppstartFraSkjema info) ]
        |> listeSammendragFraSkjema (hentValg OppstartValg) " - "



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
