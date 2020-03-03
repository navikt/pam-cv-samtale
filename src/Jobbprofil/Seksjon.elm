module Jobbprofil.Seksjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , subscriptions
    , update
    , viewBrukerInput
    )

import Api
import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering
import Feilmelding
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Merkelapp as Merkelapp exposing (Merkelapp)
import FrontendModuler.Radio as Radio
import FrontendModuler.Typeahead
import Html exposing (Html, br, div, span, text)
import Html.Attributes exposing (class)
import Http
import Jobbprofil.Jobbprofil exposing (Jobbprofil)
import Jobbprofil.JobbprofilValg as JobbprofilValg exposing (AnsettelsesForm, Arbeidstider(..), Omfang, Oppstart(..), ansettelsesformValg, arbeidstidValg, omfangValg, oppstartValg)
import Jobbprofil.Kompetanse as Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade as Omrade exposing (Omrade)
import Jobbprofil.Skjema as Skjema exposing (Felt(..), UvalidertSkjema, ValidertSkjema)
import Jobbprofil.StegInfo exposing (AnsettelsesformStegInfo, ArbeidstidStegInfo, KompetanseStegInfo, OmfangStegInfo, OmradeStegInfo, OppstartStegInfo, YrkeStegInfo)
import Jobbprofil.Validering exposing (feilmeldingKompetanse, feilmeldingOmråde, feilmeldingYrke)
import LagreStatus exposing (LagreStatus)
import List.Extra as List
import Meldinger.Melding as Melding exposing (Melding)
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Person exposing (BrukerInfo(..))
import Process
import Result.Extra as Result
import String exposing (isEmpty)
import Task
import Time exposing (Posix)
import Typeahead.Typeahead as Typeahead exposing (GetSuggestionStatus(..), InputStatus(..))



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , debugStatus : DebugStatus
    , sistLagretFraCV : Posix
    , brukerInfo : BrukerInfo
    }


type alias TypeaheadOppsummeringInfo =
    { yrker : Typeahead.Model Yrke
    , omrader : Typeahead.Model Omrade
    , kompetanser : Typeahead.Model Kompetanse
    }


type Samtale
    = LasterJobbprofil
    | HentingAvJobbprofilFeilet Http.Error
    | HarJobbprofilJobbsøker Jobbprofil
    | HarJobbprofilUnderOppfølging Jobbprofil
    | HarIkkeJobbprofilJobbsøker
    | LeggTilYrker YrkeStegInfo (Typeahead.Model Yrke)
    | LeggTilOmrader OmradeStegInfo (Typeahead.Model Omrade)
    | LeggTilOmfang OmfangStegInfo
    | LeggTilArbeidstid ArbeidstidStegInfo
    | LeggTilAnsettelsesform AnsettelsesformStegInfo
    | VelgOppstart OppstartStegInfo
    | LeggTilKompetanser KompetanseStegInfo (Typeahead.Model Kompetanse)
    | VisOppsummering ValidertSkjema
    | EndreOppsummering UvalidertSkjema TypeaheadOppsummeringInfo
    | LagrerSkjema ValidertSkjema LagreStatus
    | LagringFeilet Http.Error ValidertSkjema


type FullføringStatus
    = LagringLyktesFørsteGang
    | LagringLyktesEtterFlereForsøk
    | BrukerGikkVidere


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Jobbprofil FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model info) =
    info.seksjonsMeldingsLogg


kompetanseInfoTilSkjema : KompetanseStegInfo -> ValidertSkjema
kompetanseInfoTilSkjema info =
    Skjema.initValidertSkjema
        { yrker = info.yrker
        , omrader = info.omrader
        , omfanger = info.omfanger
        , arbeidstider = info.arbeidstider
        , ansettelsesformer = info.ansettelsesformer
        , oppstart = info.oppstart
        , kompetanser = info.kompetanser
        }



--- UPDATE ---


type Msg
    = JobbprofilHentet (Result Http.Error Jobbprofil)
    | VilBegynnePåJobbprofil
    | YrkeTypeaheadMsg (Typeahead.Msg Yrke)
    | HentetYrkeTypeahead Typeahead.Query (Result Http.Error (List Yrke))
    | VilLeggeTilYrke Yrke
    | FjernValgtYrke Yrke
    | VilGåVidereFraYrke
    | OmradeTypeaheadMsg (Typeahead.Msg Omrade)
    | HentetOmradeTypeahead Typeahead.Query (Result Http.Error (List Omrade))
    | VilLeggeTilOmrade Omrade
    | VilGåVidereFraOmrade
    | FjernValgtOmrade Omrade
    | OppdatererOmfang Omfang
    | VilGåVidereFraOmfang
    | OppdatererArbeidstid Arbeidstider
    | VilGåVidereFraArbeidstid
    | OppdatererAnsettelsesform AnsettelsesForm
    | VilGåVidereFraAnsettelsesform
    | OppdatererOppstart Oppstart
    | VilGåVidereFraOppstart
    | KompetanseTypeaheadMsg (Typeahead.Msg Kompetanse)
    | HentetKompetanseTypeahead Typeahead.Query (Result Http.Error (List Kompetanse))
    | VilLeggeTilkompetanse Kompetanse
    | FjernValgtKompetanse Kompetanse
    | VilGåVidereFraKompetanse
    | VilEndreOppsummering
    | VilLagreOppsummering
    | SkjemaEndret CheckboxEndring
    | VilLagreJobbprofil
    | JobbprofilLagret (Result Http.Error Jobbprofil)
    | VilGiOppLagring
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | WindowEndrerVisibility Visibility
    | ErrorLogget
    | FeltMisterFokus
    | TimeoutEtterAtFeltMistetFokus
    | FokusSatt (Result Dom.Error ())


type CheckboxEndring
    = OmfangEndret Omfang
    | ArbeidstidEndret Arbeidstider
    | AnsettelsesformEndret AnsettelsesForm
    | OppstartEndret Oppstart


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        JobbprofilHentet result ->
            let
                underOppfølging =
                    case model.brukerInfo of
                        UnderOppfølging _ ->
                            True

                        JobbSkifter _ ->
                            False
            in
            case result of
                Ok jobbprofil ->
                    let
                        nesteSamtaleSteg =
                            if underOppfølging then
                                HarJobbprofilUnderOppfølging jobbprofil

                            else
                                HarJobbprofilJobbsøker jobbprofil
                    in
                    ( nesteSamtaleSteg
                        |> oppdaterSamtale model UtenSvar
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Err error ->
                    case error of
                        Http.BadStatus 404 ->
                            let
                                nesteSamtaleSteg =
                                    if underOppfølging then
                                        initYrkeTypeahead
                                            |> Tuple.first
                                            |> LeggTilYrker { yrker = [], underOppfølging = True, visFeilmelding = False }

                                    else
                                        HarIkkeJobbprofilJobbsøker
                            in
                            ( nesteSamtaleSteg
                                |> oppdaterSamtale model UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, logFeilmelding "Hente jobbprofil" error )

        VilBegynnePåJobbprofil ->
            ( initYrkeTypeahead
                |> Tuple.first
                |> LeggTilYrker { yrker = [], underOppfølging = False, visFeilmelding = False }
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        KompetanseTypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                LeggTilKompetanser info typeaheadModel ->
                    updateSamtaleKompetanseTypeahead model info typeaheadMsg typeaheadModel

                EndreOppsummering skjema typeaheadInfo ->
                    updateEndreSamtaleKompetanseTypeahead model skjema typeaheadMsg typeaheadInfo

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLeggeTilkompetanse _ ->
            IkkeFerdig ( Model model, Cmd.none )

        VilGåVidereFraKompetanse ->
            case model.aktivSamtale of
                LeggTilKompetanser info _ ->
                    if List.isEmpty info.kompetanser then
                        ( initKompetanseTypeahead
                            |> Tuple.first
                            |> LeggTilKompetanser { info | visFeilmelding = True }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else
                        ( kompetanseInfoTilSkjema info
                            |> VisOppsummering
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FjernValgtKompetanse kompetanse ->
            case model.aktivSamtale of
                LeggTilKompetanser info typeaheadModel ->
                    ( typeaheadModel
                        |> LeggTilKompetanser { info | kompetanser = List.remove kompetanse info.kompetanser }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                EndreOppsummering skjema typeaheadInfo ->
                    ( typeaheadInfo
                        |> EndreOppsummering (Skjema.fjernKompetanse skjema kompetanse)
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetKompetanseTypeahead query result ->
            case model.aktivSamtale of
                LeggTilKompetanser info typeaheadModel ->
                    let
                        resultWithoutSelected =
                            result
                                |> Result.map (List.filter (\kompetanse_ -> List.notMember kompetanse_ info.kompetanser))
                    in
                    ( resultWithoutSelected
                        |> Typeahead.updateSuggestions Kompetanse.label typeaheadModel query
                        |> LeggTilKompetanser info
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente Kompetansetypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                EndreOppsummering skjema typeaheadInfo ->
                    let
                        resultWithoutSelected =
                            result
                                |> Result.map (List.filter (\kompetanse_ -> List.notMember kompetanse_ (Skjema.kompetanser skjema)))
                    in
                    ( { typeaheadInfo | kompetanser = Typeahead.updateSuggestions Kompetanse.label typeaheadInfo.kompetanser query resultWithoutSelected }
                        |> EndreOppsummering skjema
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente Kompetansetypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        OmradeTypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                LeggTilOmrader info typeaheadModel ->
                    updateSamtaleOmradeTypeahead model info typeaheadMsg typeaheadModel

                EndreOppsummering skjema typeaheadInfo ->
                    updateEndreSamtaleOmradeTypeahead model skjema typeaheadMsg typeaheadInfo

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FjernValgtOmrade omrade ->
            case model.aktivSamtale of
                LeggTilOmrader info typeaheadModel ->
                    ( typeaheadModel
                        |> LeggTilOmrader { info | omrader = List.remove omrade info.omrader }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                EndreOppsummering skjema typeaheadInfo ->
                    ( typeaheadInfo
                        |> EndreOppsummering (Skjema.fjernOmråde skjema omrade)
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLeggeTilOmrade _ ->
            IkkeFerdig ( Model model, Cmd.none )

        VilGåVidereFraOmrade ->
            case model.aktivSamtale of
                LeggTilOmrader info _ ->
                    if List.isEmpty info.omrader then
                        ( initOmradeTypeahead
                            |> Tuple.first
                            |> LeggTilOmrader { info | visFeilmelding = True }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else
                        ( LeggTilOmfang { omfanger = [], yrker = info.yrker, omrader = info.omrader }
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetOmradeTypeahead query result ->
            case model.aktivSamtale of
                LeggTilOmrader info typeaheadModel ->
                    let
                        resultWithoutSelected =
                            result
                                |> Result.map (List.filter (\omrade_ -> List.notMember omrade_ info.omrader))
                    in
                    ( resultWithoutSelected
                        |> Typeahead.updateSuggestions Omrade.tittel typeaheadModel query
                        |> LeggTilOmrader info
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente Omradetypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                EndreOppsummering skjema typeaheadInfo ->
                    let
                        resultWithoutSelected =
                            result
                                |> Result.map (List.filter (\omrade_ -> List.notMember omrade_ (Skjema.omrader skjema)))
                    in
                    ( { typeaheadInfo | omrader = Typeahead.updateSuggestions Omrade.tittel typeaheadInfo.omrader query resultWithoutSelected }
                        |> EndreOppsummering skjema
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente Omradetypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        YrkeTypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                LeggTilYrker info typeaheadModel ->
                    updateSamtaleYrkeTypeahead model info typeaheadMsg typeaheadModel

                EndreOppsummering info typeaheadInfo ->
                    updateEndreSamtaleYrkeTypeahead model info typeaheadMsg typeaheadInfo

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetYrkeTypeahead query result ->
            case model.aktivSamtale of
                LeggTilYrker info typeaheadModel ->
                    let
                        resultWithoutSelected =
                            result
                                |> Result.map (List.filter (\yrke_ -> List.notMember yrke_ info.yrker))
                    in
                    ( resultWithoutSelected
                        |> Typeahead.updateSuggestions Yrke.label typeaheadModel query
                        |> LeggTilYrker info
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente Yrketypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                EndreOppsummering skjema typeaheadInfo ->
                    let
                        resultWithoutSelected =
                            result
                                |> Result.map (List.filter (\yrke_ -> List.notMember yrke_ (Skjema.yrker skjema)))
                    in
                    ( { typeaheadInfo | yrker = Typeahead.updateSuggestions Yrke.label typeaheadInfo.yrker query resultWithoutSelected }
                        |> EndreOppsummering skjema
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente Yrketypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilLeggeTilYrke _ ->
            IkkeFerdig ( Model model, Cmd.none )

        FjernValgtYrke yrke ->
            case model.aktivSamtale of
                LeggTilYrker info typeaheadModel ->
                    ( typeaheadModel
                        |> LeggTilYrker { info | yrker = List.remove yrke info.yrker }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                EndreOppsummering skjema typeaheadInfo ->
                    ( typeaheadInfo
                        |> EndreOppsummering (Skjema.fjernStillingYrke skjema yrke)
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilGåVidereFraYrke ->
            case model.aktivSamtale of
                LeggTilYrker info _ ->
                    if List.isEmpty info.yrker then
                        ( initYrkeTypeahead
                            |> Tuple.first
                            |> LeggTilYrker { info | visFeilmelding = True }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else
                        ( initOmradeTypeahead
                            |> Tuple.first
                            |> LeggTilOmrader { yrker = info.yrker, omrader = [], visFeilmelding = False }
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        VilLagreJobbprofil ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    ( LagreStatus.init
                        |> LagrerSkjema skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                      -- Post kalles uansett om man lagrer første gang eller endrer jobbprofil
                    , Api.opprettJobbprofil JobbprofilLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreOppsummering skjema typeaheadInfo ->
                    case skjemaEndring of
                        OmfangEndret verdi ->
                            ( EndreOppsummering (Skjema.leggTilEllerFjernOmfang skjema verdi) typeaheadInfo
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        ArbeidstidEndret verdi ->
                            ( EndreOppsummering (Skjema.leggTilEllerFjernArbeidstid skjema verdi) typeaheadInfo
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        AnsettelsesformEndret verdi ->
                            ( EndreOppsummering (Skjema.leggTilEllerFjernAnsettelsesForm skjema verdi) typeaheadInfo
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        OppstartEndret verdi ->
                            ( EndreOppsummering (Skjema.oppdaterOppstart skjema verdi) typeaheadInfo
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilGåVidereFraOmfang ->
            case model.aktivSamtale of
                LeggTilOmfang info ->
                    ( LeggTilArbeidstid { arbeidstider = [], omfanger = info.omfanger, yrker = info.yrker, omrader = info.omrader }
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        VilGåVidereFraArbeidstid ->
            case model.aktivSamtale of
                LeggTilArbeidstid info ->
                    ( LeggTilAnsettelsesform { ansettelsesformer = [], arbeidstider = info.arbeidstider, omfanger = info.omfanger, yrker = info.yrker, omrader = info.omrader }
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        VilGåVidereFraAnsettelsesform ->
            case model.aktivSamtale of
                LeggTilAnsettelsesform info ->
                    ( VelgOppstart { oppstart = Nothing, ansettelsesformer = info.ansettelsesformer, arbeidstider = info.arbeidstider, omfanger = info.omfanger, yrker = info.yrker, omrader = info.omrader }
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        OppdatererOmfang endring ->
            case model.aktivSamtale of
                LeggTilOmfang omfanginfo ->
                    ( LeggTilOmfang { omfanginfo | omfanger = leggTilEllerFjernFraListe endring omfanginfo.omfanger }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererArbeidstid endring ->
            case model.aktivSamtale of
                LeggTilArbeidstid info ->
                    ( LeggTilArbeidstid { info | arbeidstider = leggTilEllerFjernFraListe endring info.arbeidstider }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererAnsettelsesform endring ->
            case model.aktivSamtale of
                LeggTilAnsettelsesform info ->
                    ( LeggTilAnsettelsesform { info | ansettelsesformer = leggTilEllerFjernFraListe endring info.ansettelsesformer }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererOppstart endring ->
            case model.aktivSamtale of
                VelgOppstart info ->
                    ( VelgOppstart { info | oppstart = Just endring }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilGåVidereFraOppstart ->
            case model.aktivSamtale of
                VelgOppstart info ->
                    case info.oppstart of
                        Nothing ->
                            ( VelgOppstart { info | oppstart = Nothing }
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Just value ->
                            -- TODO - funksjoner for transformering av records i transisjonsfaser, f.eks. oppstartInfoTilKompetanseInfo : OppstartInfo -> KompetanseInfo
                            ( initKompetanseTypeahead
                                |> Tuple.first
                                |> LeggTilKompetanser { kompetanser = [], oppstart = value, ansettelsesformer = info.ansettelsesformer, arbeidstider = info.arbeidstider, omfanger = info.omfanger, yrker = info.yrker, omrader = info.omrader, visFeilmelding = False }
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        VilEndreOppsummering ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    updateEtterVilEndreSkjema model msg skjema

                HarJobbprofilJobbsøker jobbprofil ->
                    updateEtterVilEndreSkjema model msg (Skjema.fraJobbprofil jobbprofil)

                HarJobbprofilUnderOppfølging jobbprofil ->
                    updateEtterVilEndreSkjema model msg (Skjema.fraJobbprofil jobbprofil)

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreOppsummering ->
            IkkeFerdig ( Model model, Cmd.none )

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            SamtaleAnimasjon.update model.debugStatus samtaleAnimasjonMsg model.seksjonsMeldingsLogg
                |> updateEtterFullførtMelding model

        WindowEndrerVisibility _ ->
            IkkeFerdig ( Model model, Cmd.none )

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )

        FeltMisterFokus ->
            IkkeFerdig ( Model model, mistetFokusCmd )

        FokusSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )

        TimeoutEtterAtFeltMistetFokus ->
            case model.aktivSamtale of
                LeggTilYrker info typeaheadModel ->
                    visFeilmeldingForYrke model info typeaheadModel

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        JobbprofilLagret result ->
            IkkeFerdig ( Model model, Cmd.none )

        VilGiOppLagring ->
            IkkeFerdig ( Model model, Cmd.none )


updateEtterVilEndreSkjema : ModelInfo -> Msg -> ValidertSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model msg skjema =
    ( { yrker = Tuple.first initYrkeTypeahead, omrader = Tuple.first initOmradeTypeahead, kompetanser = Tuple.first initKompetanseTypeahead }
        |> EndreOppsummering (Skjema.tilUvalidertSkjema skjema)
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


leggTilEllerFjernFraListe : a -> List a -> List a
leggTilEllerFjernFraListe verdi liste =
    if List.member verdi liste then
        List.remove verdi liste

    else
        List.append [ verdi ] liste


updateSamtaleKompetanseTypeahead : ModelInfo -> KompetanseStegInfo -> Typeahead.Msg Kompetanse -> Typeahead.Model Kompetanse -> SamtaleStatus
updateSamtaleKompetanseTypeahead model info msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Kompetanse.label msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just kompetanse ->
                    ( nyTypeaheadModel
                        |> LeggTilKompetanser { info | kompetanser = List.append info.kompetanser [ kompetanse ], visFeilmelding = False }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForKompetanse model info typeaheadModel

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilKompetanser info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilKompetanser info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getKompetanseJobbprofilTypeahead HentetKompetanseTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilKompetanser info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView Kompetanse.label Nothing
                    |> Cmd.map KompetanseTypeaheadMsg
                )


updateEndreSamtaleKompetanseTypeahead : ModelInfo -> UvalidertSkjema -> Typeahead.Msg Kompetanse -> TypeaheadOppsummeringInfo -> SamtaleStatus
updateEndreSamtaleKompetanseTypeahead model info msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Kompetanse.label msg typeaheadModel.kompetanser
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just kompetanse ->
                    ( EndreOppsummering (Skjema.leggTilKompetanse info kompetanse) { typeaheadModel | kompetanser = nyTypeaheadModel }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForEndreYrke model info { typeaheadModel | kompetanser = nyTypeaheadModel }

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( EndreOppsummering info { typeaheadModel | kompetanser = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( EndreOppsummering info { typeaheadModel | kompetanser = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getKompetanseJobbprofilTypeahead HentetKompetanseTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( EndreOppsummering info { typeaheadModel | kompetanser = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView Kompetanse.label Nothing
                    |> Cmd.map KompetanseTypeaheadMsg
                )


updateSamtaleOmradeTypeahead : ModelInfo -> OmradeStegInfo -> Typeahead.Msg Omrade -> Typeahead.Model Omrade -> SamtaleStatus
updateSamtaleOmradeTypeahead model info msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Omrade.tittel msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just omrade ->
                    ( nyTypeaheadModel
                        |> LeggTilOmrader { info | omrader = List.append info.omrader [ omrade ], visFeilmelding = False }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForOmrade model info typeaheadModel

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilOmrader info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilOmrader info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getOmradeJobbprofilTypeahead HentetOmradeTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilOmrader info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView Omrade.tittel Nothing
                    |> Cmd.map OmradeTypeaheadMsg
                )


updateEndreSamtaleOmradeTypeahead : ModelInfo -> UvalidertSkjema -> Typeahead.Msg Omrade -> TypeaheadOppsummeringInfo -> SamtaleStatus
updateEndreSamtaleOmradeTypeahead model info msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Omrade.tittel msg typeaheadModel.omrader
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just omrade ->
                    ( EndreOppsummering (Skjema.leggTilOmråde info omrade) { typeaheadModel | omrader = nyTypeaheadModel }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForEndreOmrade model info { typeaheadModel | omrader = nyTypeaheadModel }

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( EndreOppsummering info { typeaheadModel | omrader = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( EndreOppsummering info { typeaheadModel | omrader = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getOmradeJobbprofilTypeahead HentetOmradeTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( EndreOppsummering info { typeaheadModel | omrader = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView Omrade.tittel Nothing
                    |> Cmd.map OmradeTypeaheadMsg
                )


updateSamtaleYrkeTypeahead : ModelInfo -> YrkeStegInfo -> Typeahead.Msg Yrke -> Typeahead.Model Yrke -> SamtaleStatus
updateSamtaleYrkeTypeahead model info msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Yrke.label msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just yrke ->
                    ( nyTypeaheadModel
                        |> LeggTilYrker { yrker = List.append info.yrker [ yrke ], underOppfølging = info.underOppfølging, visFeilmelding = False }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForYrke model info nyTypeaheadModel

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilYrker info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilYrker info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getYrkeJobbprofilTypeahead HentetYrkeTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilYrker info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView Yrke.label Nothing
                    |> Cmd.map YrkeTypeaheadMsg
                )


updateEndreSamtaleYrkeTypeahead : ModelInfo -> UvalidertSkjema -> Typeahead.Msg Yrke -> TypeaheadOppsummeringInfo -> SamtaleStatus
updateEndreSamtaleYrkeTypeahead model info msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Yrke.label msg typeaheadModel.yrker
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just yrke ->
                    ( EndreOppsummering (Skjema.leggTilStillingYrke info yrke) { typeaheadModel | yrker = nyTypeaheadModel }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForEndreYrke model info { typeaheadModel | yrker = nyTypeaheadModel }

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( EndreOppsummering info { typeaheadModel | yrker = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( EndreOppsummering info { typeaheadModel | yrker = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getYrkeJobbprofilTypeahead HentetYrkeTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( EndreOppsummering info { typeaheadModel | yrker = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView Yrke.label Nothing
                    |> Cmd.map YrkeTypeaheadMsg
                )


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding model ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                --VenterPåAnimasjonFørFullføring  _ ->
                --  Ferdig (sistLagret (Model model)) ferdigAnimertSamtale
                _ ->
                    ( Model { model | seksjonsMeldingsLogg = nyMeldingsLogg }
                    , Cmd.batch
                        [ Cmd.map SamtaleAnimasjonMsg cmd
                        , settFokus model.aktivSamtale
                        ]
                    )
                        |> IkkeFerdig

        MeldingerGjenstår ->
            ( Model { model | seksjonsMeldingsLogg = nyMeldingsLogg }
            , Cmd.map SamtaleAnimasjonMsg cmd
            )
                |> IkkeFerdig


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg jobbprofilSamtale =
    case jobbprofilSamtale of
        LasterJobbprofil ->
            []

        HarIkkeJobbprofilJobbsøker ->
            [ Melding.spørsmål
                [ "Vi må vite litt mer om jøbbønskene dine for at CV-en skal bli søkbar. Er du klar til å begynne?"
                ]
            ]

        HarJobbprofilJobbsøker jobbprofil ->
            [ Melding.spørsmål
                (List.concat
                    [ [ "Jeg ser du har en jobbprofil fra før av. Du har lagt inn dette:"
                      , Melding.tomLinje
                      ]
                    , jobbprofil
                        |> Skjema.fraJobbprofil
                        |> oppsummering
                    , [ Melding.tomLinje
                      , "Er informasjonen riktig?"
                      ]
                    ]
                )
            ]

        HarJobbprofilUnderOppfølging jobbprofil ->
            [ Melding.spørsmål
                (List.concat
                    [ [ "Nå gjenstår bare jobbprofilen. Jeg ser du har lagt inn dette tidligere:"
                      , Melding.tomLinje
                      ]
                    , jobbprofil
                        |> Skjema.fraJobbprofil
                        |> oppsummering
                    , [ Melding.tomLinje
                      , "Er informasjonen riktig?"
                      ]
                    ]
                )
            ]

        HentingAvJobbprofilFeilet error ->
            --todo: håndter feil
            []

        LeggTilOmfang _ ->
            [ Melding.spørsmål [ "Vil du jobbe heltid eller deltid?" ] ]

        LeggTilArbeidstid _ ->
            [ Melding.spørsmål [ "Når kan du jobbe?" ] ]

        LeggTilAnsettelsesform _ ->
            [ Melding.spørsmål [ "Hva slags ansettelse ønsker du?" ] ]

        VelgOppstart _ ->
            [ Melding.spørsmål [ "Når kan du begynne i ny jobb?" ] ]

        LeggTilYrker info _ ->
            if info.underOppfølging then
                [ Melding.spørsmål [ "Nå gjenstår bare jobbprofilen." ]
                , Melding.spørsmål [ "Hva slags stillinger eller yrker ser du etter? For eksempel møbelsnekker eller butikkmedarbeider." ]
                , Melding.spørsmål [ "Du kan legge til flere stillinger eller yrker" ]
                ]

            else
                [ Melding.spørsmål [ "Flott! Da begynner vi." ]
                , Melding.spørsmål [ "Hva slags stillinger eller yrker ser du etter? For eksempel møbelsnekker eller butikkmedarbeider." ]
                , Melding.spørsmål [ "Du kan legge til flere stillinger eller yrker" ]
                ]

        LeggTilOmrader _ _ ->
            [ Melding.spørsmål [ "Hvor vil du jobbe? For eksempel Oslo eller Kristiansund." ] ]

        LeggTilKompetanser _ _ ->
            [ Melding.spørsmål [ "Tenk på kunnskapene og ferdighetene dine fra jobb eller utdanning." ] ]

        VisOppsummering info ->
            [ Melding.spørsmål
                (List.concat
                    [ [ "Du har lagt inn dette:"
                      , Melding.tomLinje
                      ]
                    , info
                        |> oppsummering
                    , [ Melding.tomLinje
                      , "Er informasjonen riktig?"
                      ]
                    ]
                )
            ]

        EndreOppsummering _ _ ->
            [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

        LagrerSkjema _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre jobbprofil" } ]


oppsummering : ValidertSkjema -> List String
oppsummering skjema =
    [ "Stilling/yrke: " ++ Skjema.oppsummeringInnhold StillingYrkeFelt skjema
    , "Område: " ++ Skjema.oppsummeringInnhold GeografiFelt skjema
    , "Heltid/deltid: " ++ Skjema.oppsummeringInnhold OmfangFelt skjema
    , "Når kan du jobbe? " ++ Skjema.oppsummeringInnhold ArbeidstidFelt skjema
    , "Hva slags ansettelse ønsker du? " ++ Skjema.oppsummeringInnhold AnsettelsesFormFelt skjema
    , "Når kan du begynne? " ++ Skjema.oppsummeringInnhold OppstartFelt skjema
    , "Kompetanser: " ++ Skjema.oppsummeringInnhold KompetanseFelt skjema
    ]


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


logFeilmelding : String -> Http.Error -> Cmd Msg
logFeilmelding operasjon error =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError (always ErrorLogget))
        |> Maybe.withDefault Cmd.none


svarFraBrukerInput : ModelInfo -> Msg -> Melding
svarFraBrukerInput modelInfo msg =
    modelInfo
        |> modelTilBrukerInput
        |> BrukerInput.tilSvarMelding msg


oppdaterSamtale : ModelInfo -> SamtaleOppdatering Msg -> Samtale -> Model
oppdaterSamtale model meldingsoppdatering samtale =
    Model
        { model
            | aktivSamtale = samtale
            , seksjonsMeldingsLogg =
                case meldingsoppdatering of
                    IngenNyeMeldinger ->
                        model.seksjonsMeldingsLogg

                    SvarFraMsg msg ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (svarFraBrukerInput model msg)
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)

                    ManueltSvar melding ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar melding
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)

                    UtenSvar ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)
        }


mistetFokusCmd : Cmd Msg
mistetFokusCmd =
    Process.sleep 100
        |> Task.perform (\_ -> TimeoutEtterAtFeltMistetFokus)



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


type InputId
    = BekreftJobbprofilId
    | BegynnPåJobbprofilId
    | StillingYrkeTypeaheadId
    | OmradeTypeaheadId
    | KompetanseTypeaheadId
    | LagreOppsummeringId
    | OmfangCheckboxId
    | OmfangCheckboxIdMedNummer
    | ArbeidstidCheckboxId
    | ArbeidstidCheckboxIdMedNummer
    | AnsettelsesformCheckboxId
    | AnsettelsesformCheckboxIdMedNummer
    | OppstartCheckboxId
    | OppstartCheckboxIdMedNummer


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        BekreftJobbprofilId ->
            "jobbprofil-bekreft-id"

        BegynnPåJobbprofilId ->
            "jobbprofil-begynn-id"

        StillingYrkeTypeaheadId ->
            "jobbprofil-yrke-typeahaed-id"

        OmradeTypeaheadId ->
            "jobbprofil-omrade-typeahead-id"

        KompetanseTypeaheadId ->
            "jobbprofil-kompetanse-typeahead-id"

        LagreOppsummeringId ->
            "jobbprofil-lagre-oppsummering-id"

        OmfangCheckboxId ->
            "jobbprofil-omfang-checkbox-id"

        OmfangCheckboxIdMedNummer ->
            "jobbprofil-omfang-checkbox-id0"

        ArbeidstidCheckboxId ->
            "jobbprofil-arbeidstid-checkbox-id"

        ArbeidstidCheckboxIdMedNummer ->
            "jobbprofil-arbeidstid-checkbox-id0"

        AnsettelsesformCheckboxId ->
            "jobbprofil-ansettelsesform-checkbox-id"

        AnsettelsesformCheckboxIdMedNummer ->
            "jobbprofil-ansettelsesform-checkbox-id0"

        OppstartCheckboxId ->
            "jobbprofil-oppstart-checkbox-id"

        OppstartCheckboxIdMedNummer ->
            "jobbprofil-oppstart-checkbox-id0"


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        LeggTilYrker _ _ ->
            settFokusCmd StillingYrkeTypeaheadId

        LeggTilOmrader _ _ ->
            settFokusCmd OmradeTypeaheadId

        LeggTilKompetanser _ _ ->
            settFokusCmd KompetanseTypeaheadId

        EndreOppsummering _ _ ->
            settFokusCmd StillingYrkeTypeaheadId

        LeggTilOmfang _ ->
            settFokusCmd OmfangCheckboxIdMedNummer

        LeggTilArbeidstid _ ->
            settFokusCmd ArbeidstidCheckboxIdMedNummer

        LeggTilAnsettelsesform _ ->
            settFokusCmd AnsettelsesformCheckboxIdMedNummer

        VelgOppstart _ ->
            settFokusCmd OppstartCheckboxIdMedNummer

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
        |> Task.attempt FokusSatt


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput model =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            LasterJobbprofil ->
                BrukerInput.utenInnhold

            HarIkkeJobbprofilJobbsøker ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilBegynnePåJobbprofil "Ja!"
                        |> Knapp.withId (inputIdTilString BegynnPåJobbprofilId)
                    ]

            HarJobbprofilJobbsøker _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilLagreJobbprofil "Ja, det er riktig"
                        |> Knapp.withId (inputIdTilString BekreftJobbprofilId)
                    , Knapp.knapp VilEndreOppsummering "Nei, jeg vil endre"
                    ]

            HentingAvJobbprofilFeilet _ ->
                BrukerInput.utenInnhold

            HarJobbprofilUnderOppfølging _ ->
                BrukerInput.utenInnhold

            LeggTilOmrader info typeaheadModel ->
                BrukerInput.typeaheadMedMerkelapperOgGåVidereKnapp VilGåVidereFraOmrade
                    (info.omrader
                        |> feilmeldingOmråde
                        |> maybeHvisTrue info.visFeilmelding
                        |> Typeahead.toViewElement Omrade.tittel typeaheadModel
                        |> FrontendModuler.Typeahead.map OmradeTypeaheadMsg
                    )
                    (List.map (\x -> Merkelapp.merkelapp (FjernValgtOmrade x) (Omrade.tittel x)) info.omrader)

            LeggTilYrker info typeaheadModel ->
                BrukerInput.typeaheadMedMerkelapperOgGåVidereKnapp VilGåVidereFraYrke
                    (info.yrker
                        |> feilmeldingYrke
                        |> maybeHvisTrue info.visFeilmelding
                        |> Typeahead.toViewElement Yrke.label typeaheadModel
                        |> FrontendModuler.Typeahead.map YrkeTypeaheadMsg
                    )
                    (List.map (\x -> Merkelapp.merkelapp (FjernValgtYrke x) (Yrke.label x)) info.yrker)

            LeggTilOmfang info ->
                BrukerInput.checkboxGruppeMedGåVidereKnapp VilGåVidereFraOmfang
                    (List.map
                        (\it ->
                            Checkbox.withId (byggOmfangValgId it) (Checkbox.checkbox (JobbprofilValg.omfangLabel it) (OppdatererOmfang it) (List.member it info.omfanger))
                        )
                        omfangValg
                    )

            LeggTilArbeidstid info ->
                BrukerInput.checkboxGruppeMedGåVidereKnapp VilGåVidereFraArbeidstid
                    (List.map
                        (\it ->
                            Checkbox.withId (byggArbeidstidValgId it) (Checkbox.checkbox (JobbprofilValg.arbeidstidLabel it) (OppdatererArbeidstid it) (List.member it info.arbeidstider))
                        )
                        arbeidstidValg
                    )

            LeggTilAnsettelsesform info ->
                BrukerInput.checkboxGruppeMedGåVidereKnapp VilGåVidereFraAnsettelsesform
                    (List.map
                        (\it ->
                            Checkbox.withId (byggAnsettelsesformValgId it) (Checkbox.checkbox (JobbprofilValg.ansettelsesFormLabel it) (OppdatererAnsettelsesform it) (List.member it info.ansettelsesformer))
                        )
                        ansettelsesformValg
                    )

            VelgOppstart info ->
                BrukerInput.radioGruppeMedGåVidereKnapp VilGåVidereFraOppstart
                    (List.map
                        (\it ->
                            info.oppstart
                                == Just it
                                |> Radio.radio (JobbprofilValg.oppstartLabel it) (JobbprofilValg.oppstartLabel it) (OppdatererOppstart it)
                                |> Radio.withId (byggOppstartValgId it)
                        )
                        oppstartValg
                    )

            LeggTilKompetanser info typeaheadModel ->
                BrukerInput.typeaheadMedMerkelapperOgGåVidereKnapp VilGåVidereFraKompetanse
                    (info.kompetanser
                        |> feilmeldingKompetanse
                        |> maybeHvisTrue info.visFeilmelding
                        |> Typeahead.toViewElement Kompetanse.label typeaheadModel
                        |> FrontendModuler.Typeahead.map KompetanseTypeaheadMsg
                    )
                    (List.map (\x -> Merkelapp.merkelapp (FjernValgtKompetanse x) (Kompetanse.label x)) info.kompetanser)

            VisOppsummering _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilLagreJobbprofil "Ja, det er riktig"
                    , Knapp.knapp VilEndreOppsummering "Nei, jeg vil endre"
                    ]

            EndreOppsummering skjema typeaheadInfo ->
                let
                    stillinger =
                        Skjema.yrker skjema

                    omrader =
                        Skjema.omrader skjema

                    omfanger =
                        Skjema.omfanger skjema

                    arbeidstider =
                        Skjema.arbeidstider skjema

                    ansettelsesformer =
                        Skjema.ansettelsesformer skjema

                    oppstart =
                        Skjema.oppstart skjema

                    kompetanser =
                        Skjema.kompetanser skjema
                in
                BrukerInput.skjema { lagreMsg = VilLagreOppsummering, lagreKnappTekst = "Lagre endringer" }
                    [ stillinger
                        |> feilmeldingYrke
                        |> Typeahead.view Yrke.label typeaheadInfo.yrker
                        |> Html.map YrkeTypeaheadMsg
                    , if List.length stillinger > 0 then
                        div []
                            [ span [ class "skjemaelement__label" ] [ text "Dette har du valgt: " ]
                            , List.map (\x -> Merkelapp.merkelapp (FjernValgtYrke x) (Yrke.label x)) stillinger
                                |> Merkelapp.toHtml
                            ]

                      else
                        text ""
                    , br [] []
                    , omrader
                        |> feilmeldingOmråde
                        |> Typeahead.view Omrade.tittel typeaheadInfo.omrader
                        |> Html.map OmradeTypeaheadMsg
                    , if List.length omrader > 0 then
                        div []
                            [ span [ class "skjemaelement__label" ] [ text "Dette har du valgt: " ]
                            , List.map (\x -> Merkelapp.merkelapp (FjernValgtOmrade x) (Omrade.tittel x)) omrader
                                |> Merkelapp.toHtml
                            ]

                      else
                        text ""
                    , br [] []
                    , div []
                        (span [ class "skjemaelement__label" ] [ text "Vil du jobbe heltid eller deltid? " ]
                            :: List.map
                                (\it ->
                                    Checkbox.checkbox (JobbprofilValg.omfangLabel it) (SkjemaEndret (OmfangEndret it)) (List.member it omfanger)
                                        |> Checkbox.toHtml
                                )
                                omfangValg
                        )
                    , br [] []
                    , div []
                        (span [ class "skjemaelement__label" ] [ text "Når kan du jobbe? " ]
                            :: List.map
                                (\it ->
                                    Checkbox.checkbox (JobbprofilValg.arbeidstidLabel it) (SkjemaEndret (ArbeidstidEndret it)) (List.member it arbeidstider)
                                        |> Checkbox.toHtml
                                )
                                arbeidstidValg
                        )
                    , br [] []
                    , div []
                        (span [ class "skjemaelement__label" ] [ text "Hva slags ansettelse ønsker du? " ]
                            :: List.map
                                (\it ->
                                    Checkbox.checkbox (JobbprofilValg.ansettelsesFormLabel it) (SkjemaEndret (AnsettelsesformEndret it)) (List.member it ansettelsesformer)
                                        |> Checkbox.toHtml
                                )
                                ansettelsesformValg
                        )
                    , br [] []
                    , div []
                        (span [ class "skjemaelement__label" ] [ text "Når kan du begynne i ny jobb? " ]
                            :: List.map
                                (\it ->
                                    oppstart
                                        == Just it
                                        |> Radio.radio (JobbprofilValg.oppstartLabel it) (JobbprofilValg.oppstartLabel it) (SkjemaEndret (OppstartEndret it))
                                        |> Radio.toHtml
                                )
                                oppstartValg
                        )
                    , br [] []
                    , kompetanser
                        |> feilmeldingKompetanse
                        |> Typeahead.view Kompetanse.label typeaheadInfo.kompetanser
                        |> Html.map KompetanseTypeaheadMsg
                    , if List.length kompetanser > 0 then
                        div []
                            [ span [ class "skjemaelement__label" ] [ text "Dette har du valgt: " ]
                            , List.map (\x -> Merkelapp.merkelapp (FjernValgtKompetanse x) (Kompetanse.label x)) kompetanser
                                |> Merkelapp.toHtml
                            ]

                      else
                        text ""
                    ]

            LagrerSkjema _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    BrukerInput.utenInnhold

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    ErrorHåndtering.GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp VilGiOppLagring "Gå videre"

                            --    |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            ]

                    ErrorHåndtering.PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp VilLagreJobbprofil "Prøv igjen"

                            -- |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            , Knapp.knapp VilGiOppLagring "Gå videre"
                            ]

                    ErrorHåndtering.LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

    else
        BrukerInput.utenInnhold


byggOmfangValgId : Omfang -> String
byggOmfangValgId omfang =
    let
        omfangIndex =
            List.elemIndex omfang omfangValg
    in
    inputIdTilString OmfangCheckboxId ++ String.fromInt (Maybe.withDefault 1 omfangIndex)


byggArbeidstidValgId : Arbeidstider -> String
byggArbeidstidValgId arbeidstider =
    let
        arbeidstidIndex =
            List.elemIndex arbeidstider arbeidstidValg
    in
    inputIdTilString ArbeidstidCheckboxId ++ String.fromInt (Maybe.withDefault 1 arbeidstidIndex)


byggAnsettelsesformValgId : AnsettelsesForm -> String
byggAnsettelsesformValgId ansettelsesform =
    let
        ansettelsesformIndex =
            List.elemIndex ansettelsesform ansettelsesformValg
    in
    inputIdTilString AnsettelsesformCheckboxId ++ String.fromInt (Maybe.withDefault 1 ansettelsesformIndex)


byggOppstartValgId : Oppstart -> String
byggOppstartValgId oppstart =
    let
        oppstartIndex =
            List.elemIndex oppstart oppstartValg
    in
    inputIdTilString OppstartCheckboxId ++ String.fromInt (Maybe.withDefault 1 oppstartIndex)


visFeilmeldingForKompetanse : ModelInfo -> KompetanseStegInfo -> Typeahead.Model Kompetanse -> SamtaleStatus
visFeilmeldingForKompetanse model info typeaheadModel =
    ( typeaheadModel
        |> LeggTilKompetanser { info | visFeilmelding = True }
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig


visFeilmeldingForOmrade : ModelInfo -> OmradeStegInfo -> Typeahead.Model Omrade -> SamtaleStatus
visFeilmeldingForOmrade model info typeaheadModel =
    ( typeaheadModel
        |> LeggTilOmrader { info | visFeilmelding = True }
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig


visFeilmeldingForYrke : ModelInfo -> YrkeStegInfo -> Typeahead.Model Yrke -> SamtaleStatus
visFeilmeldingForYrke model info typeaheadModel =
    ( typeaheadModel
        |> LeggTilYrker { info | visFeilmelding = True }
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig


visFeilmeldingForEndreYrke : ModelInfo -> UvalidertSkjema -> TypeaheadOppsummeringInfo -> SamtaleStatus
visFeilmeldingForEndreYrke model info typeaheadInfo =
    ( EndreOppsummering (Skjema.gjørFeilmeldingYrkeSynlig True info) typeaheadInfo
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig


visFeilmeldingForEndreOmrade : ModelInfo -> UvalidertSkjema -> TypeaheadOppsummeringInfo -> SamtaleStatus
visFeilmeldingForEndreOmrade model info typeaheadInfo =
    ( EndreOppsummering (Skjema.gjørFeilmeldingOmrådeSynlig True info) typeaheadInfo
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig



--- INIT ---


init : DebugStatus -> Posix -> BrukerInfo -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
init debugStatus sistLagretFraCV brukerInfo gammelMeldingsLogg =
    let
        aktivSamtale =
            LasterJobbprofil
    in
    ( Model
        { seksjonsMeldingsLogg =
            gammelMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale)
        , aktivSamtale = aktivSamtale
        , brukerInfo = brukerInfo
        , debugStatus = debugStatus
        , sistLagretFraCV = sistLagretFraCV
        }
    , Cmd.batch
        [ lagtTilSpørsmålCmd debugStatus
        , Api.getJobbprofil JobbprofilHentet
        ]
    )


initKompetanseTypeahead : ( Typeahead.Model Kompetanse, Typeahead.Query )
initKompetanseTypeahead =
    Typeahead.init
        { value = ""
        , label = "Kompetanser"
        , id = inputIdTilString KompetanseTypeaheadId
        , toString = Kompetanse.label
        }
        |> Tuple.mapFirst Typeahead.withSubmitOnElementSelected


initOmradeTypeahead : ( Typeahead.Model Omrade, Typeahead.Query )
initOmradeTypeahead =
    Typeahead.init
        { value = ""
        , label = "Skriv inn fylker eller kommuner"
        , id = inputIdTilString OmradeTypeaheadId
        , toString = Omrade.tittel
        }
        |> Tuple.mapFirst Typeahead.withSubmitOnElementSelected


initYrkeTypeahead : ( Typeahead.Model Yrke, Typeahead.Query )
initYrkeTypeahead =
    Typeahead.init
        { value = ""
        , label = "Stillinger/yrker"
        , id = inputIdTilString StillingYrkeTypeaheadId
        , toString = Yrke.label
        }
        |> Tuple.mapFirst Typeahead.withSubmitOnElementSelected


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Browser.Events.onVisibilityChange WindowEndrerVisibility
        , model.seksjonsMeldingsLogg
            |> SamtaleAnimasjon.subscriptions
            |> Sub.map SamtaleAnimasjonMsg
        ]
