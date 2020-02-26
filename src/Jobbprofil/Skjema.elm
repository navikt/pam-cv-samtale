module Jobbprofil.Skjema exposing (..)

import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Jobbprofil.Jobbprofil as Jobbprofil exposing (GeografiInfo, Jobbprofil, KompetanseInfo, StillingInfo, StillingKladdInfo)
import Jobbprofil.JobbprofilValg exposing (SeksjonValg(..), ValgElement, ansettelsesFormTilBackendString, arbeidsdagValg, arbeidstidTilBackendString, arbeidstidValg, arbeidstidsordningValg, hentValg, oppstartTilBackendString)
import Jobbprofil.Kompetanse as Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade as Omrade exposing (Omrade)
import Jobbprofil.StegInfo exposing (KompetanseStegInfo)
import Json.Encode
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



--- INIT ---


init : JobbprofilSkjemaInfo -> JobbprofilSkjema
init info =
    JobbprofilSkjema info


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



--- Innhold UvalidertSkjema---


yrkerFraSkjema : UvalidertSkjema -> List Yrke
yrkerFraSkjema (UvalidertSkjema info) =
    info.yrker


omraderFraSkjema : UvalidertSkjema -> List Omrade
omraderFraSkjema (UvalidertSkjema info) =
    info.omrader


omfangerFraSkjema : UvalidertSkjema -> List String
omfangerFraSkjema (UvalidertSkjema info) =
    info.omfanger


arbeidstiderFraSkjema : UvalidertSkjema -> List String
arbeidstiderFraSkjema (UvalidertSkjema info) =
    info.arbeidstider


ansettelsesformerFraSkjema : UvalidertSkjema -> List String
ansettelsesformerFraSkjema (UvalidertSkjema info) =
    info.ansettelsesformer


oppstartFraSkjema : UvalidertSkjema -> String
oppstartFraSkjema (UvalidertSkjema info) =
    info.oppstart


kompetanserFraSkjema : UvalidertSkjema -> List Kompetanse
kompetanserFraSkjema (UvalidertSkjema info) =
    info.kompetanser



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


oppstartFraJobbprofilSkjema : JobbprofilSkjema -> Maybe String
oppstartFraJobbprofilSkjema (JobbprofilSkjema info) =
    info.oppstart


oppstartSammendragFraSkjema : JobbprofilSkjema -> String
oppstartSammendragFraSkjema info =
    [ Maybe.withDefault "" (oppstartFraJobbprofilSkjema info) ]
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


encode : ValidertSkjema -> Json.Encode.Value
encode (ValidertSkjema skjema) =
    [ [ ( "stillingliste", Json.Encode.list Yrke.encode skjema.yrker )
      , ( "stillingKladdListe", Json.Encode.list Json.Encode.string [] )
      , ( "ansettelsesformliste", Json.Encode.list Json.Encode.string (List.map ansettelsesFormTilBackendString skjema.ansettelsesformer) )
      , ( "kompetanseliste", Json.Encode.list Kompetanse.encode skjema.kompetanser )
      , ( "geografiliste", Json.Encode.list Omrade.encode skjema.omrader )
      , ( "oppstart", Json.Encode.string (oppstartTilBackendString skjema.oppstart) )
      , ( "aktiv", Json.Encode.bool False )
      , ( "omfangsliste", Json.Encode.list Json.Encode.string (List.map String.toUpper skjema.omfanger) )
      ]
    , encodeArbeidstider skjema.arbeidstider
    ]
        |> List.concat
        |> Json.Encode.object


encodeArbeidstider : List String -> List ( String, Json.Encode.Value )
encodeArbeidstider arbeidstider =
    let
        _ =
            Debug.log "arbeidstider" arbeidstider

        arbeidstid =
            List.filter (\b -> List.member b arbeidstidValg) arbeidstider

        arbeidsdag =
            List.filter (\b -> List.member b arbeidsdagValg) arbeidstider

        arbeidtidsordning =
            List.filter (\b -> List.member b arbeidstidsordningValg) arbeidstider
    in
    [ ( "arbeidstidliste", Json.Encode.list Json.Encode.string (List.map arbeidstidTilBackendString arbeidstid) )
    , ( "arbeidsdagerliste", Json.Encode.list Json.Encode.string (List.map arbeidstidTilBackendString arbeidsdag) )
    , ( "arbeidstidsordningliste", Json.Encode.list Json.Encode.string (List.map arbeidstidTilBackendString arbeidtidsordning) )
    ]
