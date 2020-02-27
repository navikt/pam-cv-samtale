module Jobbprofil.Skjema exposing (..)

import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Jobbprofil.Jobbprofil as Jobbprofil exposing (Jobbprofil)
import Jobbprofil.JobbprofilValg exposing (SeksjonValg(..), ValgElement, ansettelsesFormTilBackendString, arbeidsdagValg, arbeidstidTilBackendString, arbeidstidValg, arbeidstidsordningValg, hentValg, oppstartTilBackendString)
import Jobbprofil.Kompetanse as Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade as Omrade exposing (Omrade)
import Jobbprofil.StegInfo exposing (KompetanseStegInfo)
import Json.Encode


type ValidertSkjema
    = ValidertSkjema ValidertSkjemaInfo


type alias ValidertSkjemaInfo =
    { id : Maybe Int
    , yrker : List Yrke
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
    { id : Maybe Int
    , yrker : List Yrke
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


initValidert : ValidertSkjemaInfo -> ValidertSkjema
initValidert info =
    ValidertSkjema info


tilValidertSkjema : KompetanseStegInfo -> ValidertSkjema
tilValidertSkjema info =
    ValidertSkjema
        { id = Nothing
        , yrker = info.yrker
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
        { id = skjema.id
        , yrker = skjema.yrker
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


fraJobbprofil : Jobbprofil -> UvalidertSkjema
fraJobbprofil jobbprofil =
    UvalidertSkjema
        { id = Just (Jobbprofil.id jobbprofil)
        , yrker = Jobbprofil.stillingliste jobbprofil
        , omrader = Jobbprofil.geografiliste jobbprofil
        , omfanger = Jobbprofil.omfangsliste jobbprofil
        , arbeidstider =
            List.concat
                [ Jobbprofil.arbeidstidliste jobbprofil
                , Jobbprofil.arbeidsdagerliste jobbprofil
                , Jobbprofil.arbeidstidsordningliste jobbprofil
                ]
        , ansettelsesformer = Jobbprofil.ansettelsesformliste jobbprofil
        , oppstart = (Jobbprofil.oppstart >> Maybe.withDefault "") jobbprofil
        , kompetanser = Jobbprofil.kompetanseliste jobbprofil
        , visYrkerFeilmelding = False
        , visKompetanserFeilmelding = False
        , visOmraderFeilmelding = False
        , visOppstartFeilmelding = False
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


yrkeSammendragFraSkjema : UvalidertSkjema -> String
yrkeSammendragFraSkjema info =
    List.map Yrke.label (yrkerFraSkjema info)
        |> String.join ", "


kompetanseSammendragFraSkjema : UvalidertSkjema -> String
kompetanseSammendragFraSkjema info =
    kompetanserFraSkjema info
        |> List.map Kompetanse.label
        |> String.join ", "


geografiListeFraSkjema : UvalidertSkjema -> List Omrade
geografiListeFraSkjema (UvalidertSkjema info) =
    info.omrader


geografiSammendragFraSkjema : UvalidertSkjema -> String
geografiSammendragFraSkjema info =
    geografiListeFraSkjema info
        |> List.map Omrade.tittel
        |> String.join ", "


ansettelsesformListeFraSkjema : UvalidertSkjema -> List String
ansettelsesformListeFraSkjema (UvalidertSkjema info) =
    info.ansettelsesformer


ansettelsesformSammendragFraSkjema : UvalidertSkjema -> String
ansettelsesformSammendragFraSkjema info =
    ansettelsesformListeFraSkjema info
        |> listeSammendragFraSkjema (hentValg AnsettelsesformValg) " - "


arbeidstidListeFraSkjema : UvalidertSkjema -> List String
arbeidstidListeFraSkjema (UvalidertSkjema info) =
    info.arbeidstider


arbeidstidSammendragFraSkjema : UvalidertSkjema -> String
arbeidstidSammendragFraSkjema info =
    arbeidstidListeFraSkjema info
        |> listeSammendragFraSkjema (hentValg ArbeidstidValg) ", "


omfangsListeFraSkjema : UvalidertSkjema -> List String
omfangsListeFraSkjema (UvalidertSkjema info) =
    info.omfanger


omfangsSammendragFraSkjema : UvalidertSkjema -> String
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


oppstartFraJobbprofilSkjema : UvalidertSkjema -> String
oppstartFraJobbprofilSkjema (UvalidertSkjema info) =
    info.oppstart


oppstartSammendragFraSkjema : UvalidertSkjema -> String
oppstartSammendragFraSkjema info =
    [ oppstartFraSkjema info ]
        |> listeSammendragFraSkjema (hentValg OppstartValg) " - "



--- OPPDATERING ---


leggTilStilling : UvalidertSkjema -> Yrke -> UvalidertSkjema
leggTilStilling (UvalidertSkjema info) stilling =
    UvalidertSkjema { info | yrker = List.append info.yrker [ stilling ] }


fjernStilling : UvalidertSkjema -> Yrke -> UvalidertSkjema
fjernStilling (UvalidertSkjema info) yrke =
    UvalidertSkjema { info | yrker = List.filter (\it -> Yrke.konseptId it /= Yrke.konseptId yrke) info.yrker }


leggTilKompetanse : UvalidertSkjema -> Kompetanse -> UvalidertSkjema
leggTilKompetanse (UvalidertSkjema info) kompetanse =
    UvalidertSkjema { info | kompetanser = List.append info.kompetanser [ kompetanse ] }


fjernKompetanse : UvalidertSkjema -> Kompetanse -> UvalidertSkjema
fjernKompetanse (UvalidertSkjema info) kompetanse =
    UvalidertSkjema { info | kompetanser = List.filter (\it -> Kompetanse.konseptId it /= Kompetanse.konseptId kompetanse) info.kompetanser }


leggTilGeografi : UvalidertSkjema -> Omrade -> UvalidertSkjema
leggTilGeografi (UvalidertSkjema info) geografi =
    UvalidertSkjema { info | omrader = List.append info.omrader [ geografi ] }


fjernGeografi : UvalidertSkjema -> Omrade -> UvalidertSkjema
fjernGeografi (UvalidertSkjema info) omrade =
    UvalidertSkjema { info | omrader = List.filter (\it -> Omrade.konseptId it /= Omrade.konseptId omrade) info.omrader }


leggTilOmfang : UvalidertSkjema -> String -> UvalidertSkjema
leggTilOmfang (UvalidertSkjema info) omfang =
    UvalidertSkjema { info | omfanger = List.append info.omfanger [ omfang ] }


fjernOmfang : UvalidertSkjema -> String -> UvalidertSkjema
fjernOmfang (UvalidertSkjema info) omfang =
    UvalidertSkjema { info | omfanger = List.filter (\it -> it /= omfang) info.omfanger }


leggTilAnsettelsesForm : UvalidertSkjema -> String -> UvalidertSkjema
leggTilAnsettelsesForm (UvalidertSkjema info) ansettelsesForm =
    UvalidertSkjema { info | ansettelsesformer = List.append info.ansettelsesformer [ ansettelsesForm ] }


fjernAnsettelsesForm : UvalidertSkjema -> String -> UvalidertSkjema
fjernAnsettelsesForm (UvalidertSkjema info) ansettelsesForm =
    UvalidertSkjema { info | ansettelsesformer = List.filter (\it -> it /= ansettelsesForm) info.ansettelsesformer }


oppdaterOppstart : UvalidertSkjema -> String -> UvalidertSkjema
oppdaterOppstart (UvalidertSkjema info) oppstart =
    UvalidertSkjema { info | oppstart = oppstart }



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
