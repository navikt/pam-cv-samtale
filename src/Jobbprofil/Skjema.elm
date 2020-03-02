module Jobbprofil.Skjema exposing (..)

--todo: expose bare det du trenger

import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Jobbprofil.Jobbprofil as Jobbprofil exposing (Jobbprofil)
import Jobbprofil.JobbprofilValg exposing (..)
import Jobbprofil.Kompetanse as Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade as Omrade exposing (Omrade)
import Json.Encode


type ValidertSkjema
    = ValidertSkjema ValidertSkjemaInfo


type alias ValidertSkjemaInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List Omfang
    , arbeidstider : List Arbeidstider
    , ansettelsesformer : List AnsettelsesForm
    , oppstart : Oppstart
    , kompetanser : List Kompetanse
    }


type UvalidertSkjema
    = UvalidertSkjema UvalidertSkjemaInfo


type alias UvalidertSkjemaInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List Omfang
    , arbeidstider : List Arbeidstider
    , ansettelsesformer : List AnsettelsesForm
    , oppstart : Maybe Oppstart
    , kompetanser : List Kompetanse
    , visYrkerFeilmelding : Bool
    , visKompetanserFeilmelding : Bool
    , visOmraderFeilmelding : Bool
    , visOppstartFeilmelding : Bool
    }



--- INIT ---


initValidertSkjema : ValidertSkjemaInfo -> ValidertSkjema
initValidertSkjema info =
    ValidertSkjema info


tilUvalidertSkjema : ValidertSkjema -> UvalidertSkjema
tilUvalidertSkjema (ValidertSkjema skjema) =
    UvalidertSkjema
        { yrker = skjema.yrker
        , omrader = skjema.omrader
        , omfanger = skjema.omfanger
        , arbeidstider = skjema.arbeidstider
        , ansettelsesformer = skjema.ansettelsesformer
        , oppstart = Just skjema.oppstart
        , kompetanser = skjema.kompetanser
        , visYrkerFeilmelding = False
        , visKompetanserFeilmelding = False
        , visOmraderFeilmelding = False
        , visOppstartFeilmelding = False
        }


fraJobbprofil : Jobbprofil -> ValidertSkjema
fraJobbprofil jobbprofil =
    ValidertSkjema
        { yrker = Jobbprofil.stillingliste jobbprofil
        , omrader = Jobbprofil.geografiliste jobbprofil
        , omfanger = Jobbprofil.omfangsliste jobbprofil
        , arbeidstider = Jobbprofil.arbeidstider jobbprofil
        , ansettelsesformer = Jobbprofil.ansettelsesformliste jobbprofil
        , oppstart = (Jobbprofil.oppstart >> Maybe.withDefault EtterAvtale) jobbprofil
        , kompetanser = Jobbprofil.kompetanseliste jobbprofil
        }



--- Innhold UvalidertSkjema---


yrkerFraSkjema : UvalidertSkjema -> List Yrke
yrkerFraSkjema (UvalidertSkjema info) =
    info.yrker


omraderFraSkjema : UvalidertSkjema -> List Omrade
omraderFraSkjema (UvalidertSkjema info) =
    info.omrader


omfangerFraSkjema : UvalidertSkjema -> List Omfang
omfangerFraSkjema (UvalidertSkjema info) =
    info.omfanger


arbeidstiderFraSkjema : UvalidertSkjema -> List Arbeidstider
arbeidstiderFraSkjema (UvalidertSkjema info) =
    info.arbeidstider


ansettelsesformerFraSkjema : UvalidertSkjema -> List AnsettelsesForm
ansettelsesformerFraSkjema (UvalidertSkjema info) =
    info.ansettelsesformer


oppstartFraSkjema : UvalidertSkjema -> Maybe Oppstart
oppstartFraSkjema (UvalidertSkjema info) =
    info.oppstart


kompetanserFraSkjema : UvalidertSkjema -> List Kompetanse
kompetanserFraSkjema (UvalidertSkjema info) =
    info.kompetanser


yrke : ValidertSkjema -> String
yrke (ValidertSkjema info) =
    List.map Yrke.label info.yrker
        |> String.join ", "


kompetanse : ValidertSkjema -> String
kompetanse (ValidertSkjema info) =
    info.kompetanser
        |> List.map Kompetanse.label
        |> String.join ", "


geografi : ValidertSkjema -> String
geografi (ValidertSkjema info) =
    info.omrader
        |> List.map Omrade.tittel
        |> String.join ", "


ansettelsesform : ValidertSkjema -> String
ansettelsesform (ValidertSkjema info) =
    info.ansettelsesformer
        |> List.map ansettelsesFormLabel
        |> listeSammendragFraSkjema " - "


arbeidstid : ValidertSkjema -> String
arbeidstid (ValidertSkjema info) =
    info.arbeidstider
        |> List.map arbeidstidLabel
        |> listeSammendragFraSkjema ", "


omfangs : ValidertSkjema -> String
omfangs (ValidertSkjema info) =
    info.omfanger
        |> List.map omfangLabel
        |> listeSammendragFraSkjema " - "


listeSammendragFraSkjema : String -> List String -> String
listeSammendragFraSkjema separator info =
    info
        |> String.join separator


oppstart : ValidertSkjema -> String
oppstart (ValidertSkjema info) =
    info.oppstart
        |> oppstartLabel



--- OPPDATERING ---


leggTilStilling : UvalidertSkjema -> Yrke -> UvalidertSkjema
leggTilStilling (UvalidertSkjema info) stilling =
    UvalidertSkjema { info | yrker = List.append info.yrker [ stilling ] }


fjernStilling : UvalidertSkjema -> Yrke -> UvalidertSkjema
fjernStilling (UvalidertSkjema info) yrke_ =
    UvalidertSkjema { info | yrker = List.filter (\it -> Yrke.konseptId it /= Yrke.konseptId yrke_) info.yrker }


leggTilKompetanse : UvalidertSkjema -> Kompetanse -> UvalidertSkjema
leggTilKompetanse (UvalidertSkjema info) kompetanse_ =
    UvalidertSkjema { info | kompetanser = List.append info.kompetanser [ kompetanse_ ] }


fjernKompetanse : UvalidertSkjema -> Kompetanse -> UvalidertSkjema
fjernKompetanse (UvalidertSkjema info) kompetanse_ =
    UvalidertSkjema { info | kompetanser = List.filter (\it -> Kompetanse.konseptId it /= Kompetanse.konseptId kompetanse_) info.kompetanser }


leggTilGeografi : UvalidertSkjema -> Omrade -> UvalidertSkjema
leggTilGeografi (UvalidertSkjema info) geografi_ =
    UvalidertSkjema { info | omrader = List.append info.omrader [ geografi_ ] }


fjernGeografi : UvalidertSkjema -> Omrade -> UvalidertSkjema
fjernGeografi (UvalidertSkjema info) omrade =
    UvalidertSkjema { info | omrader = List.filter (\it -> Omrade.konseptId it /= Omrade.konseptId omrade) info.omrader }


leggTilOmfang : UvalidertSkjema -> Omfang -> UvalidertSkjema
leggTilOmfang (UvalidertSkjema info) omfang =
    UvalidertSkjema { info | omfanger = List.append info.omfanger [ omfang ] }


fjernOmfang : UvalidertSkjema -> Omfang -> UvalidertSkjema
fjernOmfang (UvalidertSkjema info) omfang =
    UvalidertSkjema { info | omfanger = List.filter (\it -> it /= omfang) info.omfanger }


leggTilAnsettelsesForm : UvalidertSkjema -> AnsettelsesForm -> UvalidertSkjema
leggTilAnsettelsesForm (UvalidertSkjema info) ansettelsesForm =
    UvalidertSkjema { info | ansettelsesformer = List.append info.ansettelsesformer [ ansettelsesForm ] }


fjernAnsettelsesForm : UvalidertSkjema -> AnsettelsesForm -> UvalidertSkjema
fjernAnsettelsesForm (UvalidertSkjema info) ansettelsesForm =
    UvalidertSkjema { info | ansettelsesformer = List.filter (\it -> it /= ansettelsesForm) info.ansettelsesformer }


oppdaterOppstart : UvalidertSkjema -> Oppstart -> UvalidertSkjema
oppdaterOppstart (UvalidertSkjema info) oppstart_ =
    UvalidertSkjema { info | oppstart = Just oppstart_ }



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
      , ( "omfangsliste", Json.Encode.list Json.Encode.string (List.map omfangTilBackendString skjema.omfanger) )
      ]
    , encodeArbeidstider skjema.arbeidstider
    ]
        |> List.concat
        |> Json.Encode.object


encodeArbeidstider : List Arbeidstider -> List ( String, Json.Encode.Value )
encodeArbeidstider arbeidstider =
    let
        arbeidstidspunkt =
            List.filterMap
                (\it ->
                    case it of
                        Arbeidstid value ->
                            Just value

                        _ ->
                            Nothing
                )
                arbeidstider

        arbeidsdager =
            List.filterMap
                (\it ->
                    case it of
                        Arbeidsdager value ->
                            Just value

                        _ ->
                            Nothing
                )
                arbeidstider

        arbeidtidsordning =
            List.filterMap
                (\it ->
                    case it of
                        ArbeidstidOrdning value ->
                            Just value

                        _ ->
                            Nothing
                )
                arbeidstider
    in
    [ ( "arbeidstidliste", Json.Encode.list Json.Encode.string (List.map arbeidstidspunktTilBackendString arbeidstidspunkt) )
    , ( "arbeidsdagerliste", Json.Encode.list Json.Encode.string (List.map arbeidsdagTilBackendString arbeidsdager) )
    , ( "arbeidstidsordningliste", Json.Encode.list Json.Encode.string (List.map arbeidstidOrdningTilBackendString arbeidtidsordning) )
    ]
