module Jobbprofil.Skjema exposing
    ( Felt(..)
    , UvalidertSkjema
    , ValidertSkjema
    , ansettelsesformer
    , arbeidstider
    , encode
    , fjernAnsettelsesForm
    , fjernKompetanse
    , fjernOmfang
    , fjernOmråde
    , fjernStillingYrke
    , fraJobbprofil
    , gjørFeilmeldingOmrådeSynlig
    , gjørFeilmeldingYrkeSynlig
    , initValidertSkjema
    , kompetanser
    , leggTilEllerFjernAnsettelsesForm
    , leggTilEllerFjernArbeidstid
    , leggTilEllerFjernOmfang
    , leggTilKompetanse
    , leggTilOmfang
    , leggTilOmråde
    , leggTilStillingYrke
    , omfanger
    , omrader
    , oppdaterOppstart
    , oppstart
    , oppsummeringInnhold
    , tilUvalidertSkjema
    , tilValidertSkjema
    , tillatÅViseAlleFeilmeldinger
    , valider
    , yrker
    )

import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Jobbprofil.Jobbprofil as Jobbprofil exposing (Jobbprofil)
import Jobbprofil.JobbprofilValg exposing (..)
import Jobbprofil.Kompetanse as Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade as Omrade exposing (Omrade)
import Jobbprofil.Validering exposing (feilmeldingKompetanse, feilmeldingOmråde, feilmeldingOppstart, feilmeldingYrke)
import Json.Encode
import List.Extra as List


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
    }



--- INIT ---


initValidertSkjema : ValidertSkjemaInfo -> ValidertSkjema
initValidertSkjema info =
    ValidertSkjema info


tilValidertSkjema : UvalidertSkjema -> ValidertSkjema
tilValidertSkjema (UvalidertSkjema uvalidertSkjema) =
    ValidertSkjema
        { yrker = uvalidertSkjema.yrker
        , omrader = uvalidertSkjema.omrader
        , omfanger = uvalidertSkjema.omfanger
        , arbeidstider = uvalidertSkjema.arbeidstider
        , ansettelsesformer = uvalidertSkjema.ansettelsesformer
        , oppstart = validertOppstart uvalidertSkjema.oppstart
        , kompetanser = uvalidertSkjema.kompetanser
        }


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


yrker : UvalidertSkjema -> List Yrke
yrker (UvalidertSkjema info) =
    info.yrker


omrader : UvalidertSkjema -> List Omrade
omrader (UvalidertSkjema info) =
    info.omrader


omfanger : UvalidertSkjema -> List Omfang
omfanger (UvalidertSkjema info) =
    info.omfanger


arbeidstider : UvalidertSkjema -> List Arbeidstider
arbeidstider (UvalidertSkjema info) =
    info.arbeidstider


ansettelsesformer : UvalidertSkjema -> List AnsettelsesForm
ansettelsesformer (UvalidertSkjema info) =
    info.ansettelsesformer


oppstart : UvalidertSkjema -> Maybe Oppstart
oppstart (UvalidertSkjema info) =
    info.oppstart


validertOppstart : Maybe Oppstart -> Oppstart
validertOppstart uvalidert =
    case uvalidert of
        Nothing ->
            EtterAvtale

        Just validert ->
            validert


kompetanser : UvalidertSkjema -> List Kompetanse
kompetanser (UvalidertSkjema info) =
    info.kompetanser



--- Oppsummering ---


type Felt
    = StillingYrkeFelt
    | KompetanseFelt
    | GeografiFelt
    | AnsettelsesFormFelt
    | ArbeidstidFelt
    | OmfangFelt
    | OppstartFelt


oppsummeringInnhold : Felt -> ValidertSkjema -> String
oppsummeringInnhold felt (ValidertSkjema info) =
    case felt of
        StillingYrkeFelt ->
            List.map Yrke.label info.yrker
                |> String.join ", "

        KompetanseFelt ->
            info.kompetanser
                |> List.map Kompetanse.label
                |> String.join ", "

        GeografiFelt ->
            info.omrader
                |> List.map Omrade.tittel
                |> String.join ", "

        AnsettelsesFormFelt ->
            info.ansettelsesformer
                |> List.map ansettelsesFormLabel
                |> listeSammendragFraSkjema " - "

        ArbeidstidFelt ->
            info.arbeidstider
                |> List.map arbeidstidLabel
                |> listeSammendragFraSkjema ", "

        OmfangFelt ->
            info.omfanger
                |> List.map omfangLabel
                |> listeSammendragFraSkjema " - "

        OppstartFelt ->
            info.oppstart
                |> oppstartLabel


listeSammendragFraSkjema : String -> List String -> String
listeSammendragFraSkjema separator info =
    info
        |> String.join separator



--- OPPDATERING ---


leggTilStillingYrke : UvalidertSkjema -> Yrke -> UvalidertSkjema
leggTilStillingYrke (UvalidertSkjema info) stilling =
    UvalidertSkjema { info | yrker = List.append info.yrker [ stilling ] }


fjernStillingYrke : UvalidertSkjema -> Yrke -> UvalidertSkjema
fjernStillingYrke (UvalidertSkjema info) yrke_ =
    UvalidertSkjema { info | yrker = List.remove yrke_ info.yrker }


leggTilKompetanse : UvalidertSkjema -> Kompetanse -> UvalidertSkjema
leggTilKompetanse (UvalidertSkjema info) kompetanse_ =
    UvalidertSkjema { info | kompetanser = List.append info.kompetanser [ kompetanse_ ] }


fjernKompetanse : UvalidertSkjema -> Kompetanse -> UvalidertSkjema
fjernKompetanse (UvalidertSkjema info) kompetanse_ =
    UvalidertSkjema { info | kompetanser = List.remove kompetanse_ info.kompetanser }


leggTilOmråde : UvalidertSkjema -> Omrade -> UvalidertSkjema
leggTilOmråde (UvalidertSkjema info) omrade_ =
    UvalidertSkjema { info | omrader = List.append info.omrader [ omrade_ ] }


fjernOmråde : UvalidertSkjema -> Omrade -> UvalidertSkjema
fjernOmråde (UvalidertSkjema info) omrade =
    UvalidertSkjema { info | omrader = List.remove omrade info.omrader }


leggTilOmfang : UvalidertSkjema -> Omfang -> UvalidertSkjema
leggTilOmfang (UvalidertSkjema info) omfang =
    UvalidertSkjema { info | omfanger = List.append info.omfanger [ omfang ] }


fjernOmfang : UvalidertSkjema -> Omfang -> UvalidertSkjema
fjernOmfang (UvalidertSkjema info) omfang =
    UvalidertSkjema { info | omfanger = List.remove omfang info.omfanger }


leggTilEllerFjernOmfang : UvalidertSkjema -> Omfang -> UvalidertSkjema
leggTilEllerFjernOmfang (UvalidertSkjema info) verdi =
    if List.member verdi info.omfanger then
        UvalidertSkjema { info | omfanger = List.remove verdi info.omfanger }

    else
        UvalidertSkjema { info | omfanger = List.append info.omfanger [ verdi ] }


leggTilAnsettelsesForm : UvalidertSkjema -> AnsettelsesForm -> UvalidertSkjema
leggTilAnsettelsesForm (UvalidertSkjema info) ansettelsesForm =
    UvalidertSkjema { info | ansettelsesformer = List.append info.ansettelsesformer [ ansettelsesForm ] }


fjernAnsettelsesForm : UvalidertSkjema -> AnsettelsesForm -> UvalidertSkjema
fjernAnsettelsesForm (UvalidertSkjema info) ansettelsesForm =
    UvalidertSkjema { info | ansettelsesformer = List.remove ansettelsesForm info.ansettelsesformer }


leggTilEllerFjernAnsettelsesForm : UvalidertSkjema -> AnsettelsesForm -> UvalidertSkjema
leggTilEllerFjernAnsettelsesForm (UvalidertSkjema info) verdi =
    if List.member verdi info.ansettelsesformer then
        UvalidertSkjema { info | ansettelsesformer = List.remove verdi info.ansettelsesformer }

    else
        UvalidertSkjema { info | ansettelsesformer = List.append info.ansettelsesformer [ verdi ] }


oppdaterOppstart : UvalidertSkjema -> Oppstart -> UvalidertSkjema
oppdaterOppstart (UvalidertSkjema info) oppstart_ =
    UvalidertSkjema { info | oppstart = Just oppstart_ }


leggTilEllerFjernArbeidstid : UvalidertSkjema -> Arbeidstider -> UvalidertSkjema
leggTilEllerFjernArbeidstid (UvalidertSkjema info) verdi =
    if List.member verdi info.arbeidstider then
        UvalidertSkjema { info | arbeidstider = List.remove verdi info.arbeidstider }

    else
        UvalidertSkjema { info | arbeidstider = List.append info.arbeidstider [ verdi ] }



--- VALIDER ---


valider : UvalidertSkjema -> Maybe ValidertSkjema
valider (UvalidertSkjema info) =
    if feilmeldingOmråde info.omrader /= Nothing then
        Nothing

    else if feilmeldingKompetanse info.kompetanser /= Nothing then
        Nothing

    else if feilmeldingYrke info.yrker /= Nothing then
        Nothing

    else
        Maybe.map
            (\oppstart_ ->
                ValidertSkjema
                    { yrker = info.yrker
                    , omrader = info.omrader
                    , omfanger = info.omfanger
                    , arbeidstider = info.arbeidstider
                    , ansettelsesformer = info.ansettelsesformer
                    , oppstart = oppstart_
                    , kompetanser = info.kompetanser
                    }
            )
            info.oppstart


gjørFeilmeldingYrkeSynlig : Bool -> UvalidertSkjema -> UvalidertSkjema
gjørFeilmeldingYrkeSynlig synlig (UvalidertSkjema skjema) =
    -- Skal alltid vises etter onBlur/onSubmit, så hvis den noen gang har vært True, skal den alltid fortsette å være True
    UvalidertSkjema { skjema | visYrkerFeilmelding = synlig || skjema.visYrkerFeilmelding }


gjørFeilmeldingOmrådeSynlig : Bool -> UvalidertSkjema -> UvalidertSkjema
gjørFeilmeldingOmrådeSynlig synlig (UvalidertSkjema skjema) =
    -- Skal alltid vises etter onBlur/onSubmit, så hvis den noen gang har vært True, skal den alltid fortsette å være True
    UvalidertSkjema { skjema | visOmraderFeilmelding = synlig || skjema.visOmraderFeilmelding }


gjørFeilmeldingKompentanseSynlig : Bool -> UvalidertSkjema -> UvalidertSkjema
gjørFeilmeldingKompentanseSynlig synlig (UvalidertSkjema skjema) =
    -- Skal alltid vises etter onBlur/onSubmit, så hvis den noen gang har vært True, skal den alltid fortsette å være True
    UvalidertSkjema { skjema | visKompetanserFeilmelding = synlig || skjema.visKompetanserFeilmelding }


tillatÅViseAlleFeilmeldinger : UvalidertSkjema -> UvalidertSkjema
tillatÅViseAlleFeilmeldinger skjema =
    skjema
        |> gjørFeilmeldingYrkeSynlig True
        |> gjørFeilmeldingOmrådeSynlig True
        |> gjørFeilmeldingKompentanseSynlig True



--- ENCODE ---


encode : ValidertSkjema -> Json.Encode.Value
encode (ValidertSkjema skjema) =
    [ [ ( "stillingliste", Json.Encode.list Yrke.encode skjema.yrker )
      , ( "stillingKladdListe", Json.Encode.list Json.Encode.string [] )
      , ( "ansettelsesformliste", Json.Encode.list Json.Encode.string (List.map ansettelsesFormTilBackendString skjema.ansettelsesformer) )
      , ( "kompetanseliste", Json.Encode.list Kompetanse.encode skjema.kompetanser )
      , ( "geografiliste", Json.Encode.list Omrade.encode skjema.omrader )
      , ( "oppstart", Json.Encode.string (oppstartTilBackendString skjema.oppstart) )

      --  , ( "omfangsliste", Json.Encode.list Json.Encode.string (List.map omfangTilBackendString skjema.omfanger) )
      ]
    , encodeArbeidspunkt skjema.arbeidstider
    , [ ( "aktiv", Json.Encode.bool False )
      , ( "omfangsliste", Json.Encode.list Json.Encode.string (List.map omfangTilBackendString skjema.omfanger) )
      ]
    , encodeArbeidstider skjema.arbeidstider
    ]
        |> List.concat
        |> Json.Encode.object
