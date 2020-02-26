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
import Jobbprofil.JobbprofilValg as JobbprofilValg exposing (SeksjonValg(..), hentValg)
import Jobbprofil.Kompetanse as Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade as Omrade exposing (Omrade)
import Jobbprofil.Skjema as Skjema exposing (JobbprofilSkjema, UvalidertSkjema, UvalidertSkjemaInfo, ValidertSkjema, ansettelsesformSammendragFraSkjema, arbeidstidListeFraSkjema, arbeidstidSammendragFraSkjema, geografiSammendragFraSkjema, kompetanseSammendragFraSkjema, omfangsSammendragFraSkjema, oppstartSammendragFraSkjema, stillingSammendragFraSkjema, tilUvalidertSkjema, tilValidertSkjema)
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



--- UPDATE ---


type Msg
    = JobbprofilHentet (Result Http.Error Jobbprofil)
    | VilEndreJobbprofil
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
    | VilGåVidereFraOmfang
    | VilGåVidereFraArbeidstid
    | VilGåVidereFraAnsettelsesform
    | VilGåVidereFraOppstart
    | KompetanseTypeaheadMsg (Typeahead.Msg Kompetanse)
    | HentetKompetanseTypeahead Typeahead.Query (Result Http.Error (List Kompetanse))
    | VilLeggeTilkompetanse Kompetanse
    | FjernValgtKompetanse Kompetanse
    | VilGåVidereFraKompetanse
    | VilEndreOppsummering UvalidertSkjema
    | VilLagreOppsummering
    | JobbprofilEndret SkjemaEndring
    | VilLagreJobbprofil
    | JobbprofilLagret (Result Http.Error Jobbprofil)
    | VilGiOppLagring
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | WindowEndrerVisibility Visibility
    | ErrorLogget
    | FeltMisterFokus
    | TimeoutEtterAtFeltMistetFokus


type SkjemaEndring
    = Omfang OmfangStegInfo String
    | Arbeidstid ArbeidstidStegInfo String
    | Ansettelsesform AnsettelsesformStegInfo String
    | Oppstart OppstartStegInfo String
    | EndreOmfang UvalidertSkjema TypeaheadOppsummeringInfo String
    | EndreArbeidstid UvalidertSkjema TypeaheadOppsummeringInfo String
    | EndreAnsettelsesform UvalidertSkjema TypeaheadOppsummeringInfo String
    | EndreOppstart UvalidertSkjema TypeaheadOppsummeringInfo String


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
                        ( tilValidertSkjema info
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

                EndreOppsummering (Skjema.UvalidertSkjema skjema) typeaheadInfo ->
                    ( typeaheadInfo
                        |> EndreOppsummering (Skjema.UvalidertSkjema { skjema | kompetanser = List.remove kompetanse skjema.kompetanser })
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
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
                                |> Result.map (List.filter (\kompetanse_ -> List.notMember kompetanse_ (Skjema.kompetanserFraSkjema skjema)))
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

                EndreOppsummering (Skjema.UvalidertSkjema skjema) typeaheadInfo ->
                    ( typeaheadInfo
                        |> EndreOppsummering (Skjema.UvalidertSkjema { skjema | omrader = List.remove omrade skjema.omrader })
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
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
                                |> Result.map (List.filter (\omrade_ -> List.notMember omrade_ (Skjema.omraderFraSkjema skjema)))
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
                                |> Result.map (List.filter (\yrke_ -> List.notMember yrke_ (Skjema.yrkerFraSkjema skjema)))
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

                EndreOppsummering (Skjema.UvalidertSkjema skjema) typeaheadInfo ->
                    ( typeaheadInfo
                        |> EndreOppsummering (Skjema.UvalidertSkjema { skjema | yrker = List.remove yrke skjema.yrker })
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
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

        VilEndreJobbprofil ->
            IkkeFerdig ( Model model, Cmd.none )

        VilLagreJobbprofil ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    ( LagreStatus.init
                        |> LagrerSkjema skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                      -- todo kall endreJobbprofil hvis den finnes fra før
                    , Api.opprettJobbprofil JobbprofilLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        JobbprofilEndret skjemaEndring ->
            case skjemaEndring of
                Oppstart info verdi ->
                    ( VelgOppstart { oppstart = verdi, ansettelsesformer = info.ansettelsesformer, arbeidstider = info.arbeidstider, omfanger = info.omfanger, yrker = info.yrker, omrader = info.omrader }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Ansettelsesform info verdi ->
                    if List.member verdi info.ansettelsesformer then
                        ( LeggTilAnsettelsesform { ansettelsesformer = List.remove verdi info.ansettelsesformer, arbeidstider = info.arbeidstider, omfanger = info.omfanger, yrker = info.yrker, omrader = info.omrader }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else
                        ( LeggTilAnsettelsesform { ansettelsesformer = List.append [ verdi ] info.ansettelsesformer, arbeidstider = info.arbeidstider, omfanger = info.omfanger, yrker = info.yrker, omrader = info.omrader }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                Arbeidstid info verdi ->
                    if List.member verdi info.arbeidstider then
                        ( LeggTilArbeidstid { arbeidstider = List.remove verdi info.arbeidstider, omfanger = info.omfanger, yrker = info.yrker, omrader = info.omrader }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else
                        ( LeggTilArbeidstid { arbeidstider = List.append [ verdi ] info.arbeidstider, omfanger = info.omfanger, yrker = info.yrker, omrader = info.omrader }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                Omfang info verdi ->
                    if List.member verdi info.omfanger then
                        ( LeggTilOmfang { omfanger = List.remove verdi info.omfanger, yrker = info.yrker, omrader = info.omrader }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else
                        ( LeggTilOmfang { omfanger = List.append [ verdi ] info.omfanger, yrker = info.yrker, omrader = info.omrader }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                EndreOmfang (Skjema.UvalidertSkjema skjema) typeaheadInfo verdi ->
                    if List.member verdi skjema.omfanger then
                        ( EndreOppsummering (Skjema.UvalidertSkjema { skjema | omfanger = List.remove verdi skjema.omfanger }) typeaheadInfo
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else
                        ( EndreOppsummering (Skjema.UvalidertSkjema { skjema | omfanger = List.append [ verdi ] skjema.omfanger }) typeaheadInfo
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                EndreArbeidstid (Skjema.UvalidertSkjema skjema) typeaheadInfo verdi ->
                    if List.member verdi skjema.arbeidstider then
                        ( EndreOppsummering (Skjema.UvalidertSkjema { skjema | arbeidstider = List.remove verdi skjema.arbeidstider }) typeaheadInfo
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else
                        ( EndreOppsummering (Skjema.UvalidertSkjema { skjema | arbeidstider = List.append [ verdi ] skjema.arbeidstider }) typeaheadInfo
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                EndreAnsettelsesform (Skjema.UvalidertSkjema skjema) typeaheadInfo verdi ->
                    if List.member verdi skjema.ansettelsesformer then
                        ( EndreOppsummering (Skjema.UvalidertSkjema { skjema | ansettelsesformer = List.remove verdi skjema.ansettelsesformer }) typeaheadInfo
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else
                        ( EndreOppsummering (Skjema.UvalidertSkjema { skjema | ansettelsesformer = List.append [ verdi ] skjema.ansettelsesformer }) typeaheadInfo
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                EndreOppstart (Skjema.UvalidertSkjema skjema) typeaheadInfo verdi ->
                    ( EndreOppsummering (Skjema.UvalidertSkjema { skjema | oppstart = verdi }) typeaheadInfo
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

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
                    ( VelgOppstart { oppstart = "", ansettelsesformer = info.ansettelsesformer, arbeidstider = info.arbeidstider, omfanger = info.omfanger, yrker = info.yrker, omrader = info.omrader }
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        VilGåVidereFraOppstart ->
            case model.aktivSamtale of
                VelgOppstart info ->
                    if isEmpty info.oppstart then
                        ( VelgOppstart { info | oppstart = "" }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else
                        -- TODO - funksjoner for transformering av records i transisjonsfaser, f.eks. oppstartInfoTilKompetanseInfo : OppstartInfo -> KompetanseInfo
                        ( initKompetanseTypeahead
                            |> Tuple.first
                            |> LeggTilKompetanser { kompetanser = [], oppstart = info.oppstart, ansettelsesformer = info.ansettelsesformer, arbeidstider = info.arbeidstider, omfanger = info.omfanger, yrker = info.yrker, omrader = info.omrader, visFeilmelding = False }
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        VilEndreOppsummering info ->
            ( EndreOppsummering info { yrker = Tuple.first initYrkeTypeahead, omrader = Tuple.first initOmradeTypeahead, kompetanser = Tuple.first initKompetanseTypeahead }
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

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
                    , lagtTilSpørsmålCmd model.debugStatus
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
updateEndreSamtaleKompetanseTypeahead model (Skjema.UvalidertSkjema info) msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Kompetanse.label msg typeaheadModel.kompetanser
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just kompetanse ->
                    ( EndreOppsummering (Skjema.UvalidertSkjema { info | kompetanser = List.append info.kompetanser [ kompetanse ] }) { typeaheadModel | kompetanser = nyTypeaheadModel }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForEndreYrke model (Skjema.UvalidertSkjema info) { typeaheadModel | kompetanser = nyTypeaheadModel }

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( EndreOppsummering (Skjema.UvalidertSkjema info) { typeaheadModel | kompetanser = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( EndreOppsummering (Skjema.UvalidertSkjema info) { typeaheadModel | kompetanser = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getKompetanseJobbprofilTypeahead HentetKompetanseTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( EndreOppsummering (Skjema.UvalidertSkjema info) { typeaheadModel | kompetanser = nyTypeaheadModel }
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
updateEndreSamtaleOmradeTypeahead model (Skjema.UvalidertSkjema info) msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Omrade.tittel msg typeaheadModel.omrader
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just omrade ->
                    ( EndreOppsummering (Skjema.UvalidertSkjema { info | omrader = List.append info.omrader [ omrade ] }) { typeaheadModel | omrader = nyTypeaheadModel }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForEndreOmrade model (Skjema.UvalidertSkjema info) { typeaheadModel | omrader = nyTypeaheadModel }

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( EndreOppsummering (Skjema.UvalidertSkjema info) { typeaheadModel | omrader = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( EndreOppsummering (Skjema.UvalidertSkjema info) { typeaheadModel | omrader = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getOmradeJobbprofilTypeahead HentetOmradeTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( EndreOppsummering (Skjema.UvalidertSkjema info) { typeaheadModel | omrader = nyTypeaheadModel }
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
                    -- brukerVelgerYrke model (YrkeTypeaheadMsg msg) yrke
                    ( nyTypeaheadModel
                        |> LeggTilYrker { yrker = List.append info.yrker [ yrke ], underOppfølging = info.underOppfølging, visFeilmelding = False }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
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
updateEndreSamtaleYrkeTypeahead model (Skjema.UvalidertSkjema info) msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Yrke.label msg typeaheadModel.yrker
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just yrke ->
                    ( EndreOppsummering (Skjema.UvalidertSkjema { info | yrker = List.append info.yrker [ yrke ] }) { typeaheadModel | yrker = nyTypeaheadModel }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForEndreYrke model (Skjema.UvalidertSkjema info) { typeaheadModel | yrker = nyTypeaheadModel }

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( EndreOppsummering (Skjema.UvalidertSkjema info) { typeaheadModel | yrker = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( EndreOppsummering (Skjema.UvalidertSkjema info) { typeaheadModel | yrker = nyTypeaheadModel }
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getYrkeJobbprofilTypeahead HentetYrkeTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( EndreOppsummering (Skjema.UvalidertSkjema info) { typeaheadModel | yrker = nyTypeaheadModel }
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

                        -- , settFokus model.aktivSamtale
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
                        |> skjemaOppsummering
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
                        |> skjemaOppsummering
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


oppsummering : Skjema.ValidertSkjema -> List String
oppsummering (Skjema.ValidertSkjema info) =
    [ "Stilling/yrke: " ++ String.join ", " (List.map (\it -> Yrke.label it) info.yrker)
    , "Område: " ++ String.join ", " (List.map (\it -> Omrade.tittel it) info.omrader)
    , "Heltid/deltid: " ++ String.join ", " info.omfanger
    , "Når kan du jobbe? " ++ String.join ", " info.arbeidstider
    , "Hva slags ansettelse ønsker du? " ++ String.join ", " info.ansettelsesformer
    , "Når kan du begynne? " ++ info.oppstart
    , "Kompetanser: " ++ String.join ", " (List.map (\it -> Kompetanse.label it) info.kompetanser)
    ]


skjemaOppsummering : JobbprofilSkjema -> List String
skjemaOppsummering skjema =
    [ "Stilling/yrke: " ++ stillingSammendragFraSkjema skjema
    , "Område: " ++ geografiSammendragFraSkjema skjema
    , "Heltid/deltid: " ++ omfangsSammendragFraSkjema skjema
    , "Når kan du jobbe? " ++ arbeidstidSammendragFraSkjema skjema
    , "Hva slags ansettelse ønsker du? " ++ ansettelsesformSammendragFraSkjema skjema
    , "Når kan du begynne? " ++ oppstartSammendragFraSkjema skjema
    , "Kompetanser: " ++ kompetanseSammendragFraSkjema skjema
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
                    , Knapp.knapp VilEndreJobbprofil "Nei, jeg vil endre"
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
                            Checkbox.checkbox (JobbprofilValg.label it) (JobbprofilEndret (Omfang info (JobbprofilValg.label it))) (List.member (JobbprofilValg.label it) info.omfanger)
                        )
                        (hentValg OmfangValg)
                    )

            LeggTilArbeidstid info ->
                BrukerInput.checkboxGruppeMedGåVidereKnapp VilGåVidereFraArbeidstid
                    (List.map
                        (\it ->
                            Checkbox.checkbox (JobbprofilValg.label it) (JobbprofilEndret (Arbeidstid info (JobbprofilValg.label it))) (List.member (JobbprofilValg.label it) info.arbeidstider)
                        )
                        (hentValg ArbeidstidValg)
                    )

            LeggTilAnsettelsesform info ->
                BrukerInput.checkboxGruppeMedGåVidereKnapp VilGåVidereFraAnsettelsesform
                    (List.map
                        (\it ->
                            Checkbox.checkbox (JobbprofilValg.label it) (JobbprofilEndret (Ansettelsesform info (JobbprofilValg.label it))) (List.member (JobbprofilValg.label it) info.ansettelsesformer)
                        )
                        (hentValg AnsettelsesformValg)
                    )

            VelgOppstart info ->
                BrukerInput.radioGruppeMedGåVidereKnapp VilGåVidereFraOppstart
                    (List.map
                        (\it ->
                            info.oppstart
                                == JobbprofilValg.label it
                                |> Radio.radio (JobbprofilValg.label it) (JobbprofilValg.value it) (JobbprofilEndret (Oppstart info (JobbprofilValg.label it)))
                        )
                        (hentValg OppstartValg)
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

            VisOppsummering info ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilLagreJobbprofil "Ja, det er riktig"
                    , Knapp.knapp (VilEndreOppsummering (tilUvalidertSkjema info)) "Nei, jeg vil endre"
                    ]

            EndreOppsummering skjema typeaheadInfo ->
                let
                    stillinger =
                        Skjema.yrkerFraSkjema skjema

                    omrader =
                        Skjema.omraderFraSkjema skjema

                    omfanger =
                        Skjema.omfangerFraSkjema skjema

                    arbeidstider =
                        Skjema.arbeidstiderFraSkjema skjema

                    ansettelsesformer =
                        Skjema.ansettelsesformerFraSkjema skjema

                    oppstart =
                        Skjema.oppstartFraSkjema skjema

                    kompetanser =
                        Skjema.kompetanserFraSkjema skjema
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
                                    Checkbox.checkbox (JobbprofilValg.label it) (JobbprofilEndret (EndreOmfang skjema typeaheadInfo (JobbprofilValg.label it))) (List.member (JobbprofilValg.label it) omfanger)
                                        |> Checkbox.toHtml
                                )
                                (hentValg OmfangValg)
                        )
                    , br [] []
                    , div []
                        (span [ class "skjemaelement__label" ] [ text "Når kan du jobbe? " ]
                            :: List.map
                                (\it ->
                                    Checkbox.checkbox (JobbprofilValg.label it) (JobbprofilEndret (EndreArbeidstid skjema typeaheadInfo (JobbprofilValg.label it))) (List.member (JobbprofilValg.label it) arbeidstider)
                                        |> Checkbox.toHtml
                                )
                                (hentValg ArbeidstidValg)
                        )
                    , br [] []
                    , div []
                        (span [ class "skjemaelement__label" ] [ text "Hva slags ansettelse ønsker du? " ]
                            :: List.map
                                (\it ->
                                    Checkbox.checkbox (JobbprofilValg.label it) (JobbprofilEndret (EndreAnsettelsesform skjema typeaheadInfo (JobbprofilValg.label it))) (List.member (JobbprofilValg.label it) ansettelsesformer)
                                        |> Checkbox.toHtml
                                )
                                (hentValg AnsettelsesformValg)
                        )
                    , br [] []
                    , div []
                        (span [ class "skjemaelement__label" ] [ text "Når kan du begynne i ny jobb? " ]
                            :: List.map
                                (\it ->
                                    oppstart
                                        == JobbprofilValg.label it
                                        |> Radio.radio (JobbprofilValg.label it) (JobbprofilValg.value it) (JobbprofilEndret (EndreOppstart skjema typeaheadInfo (JobbprofilValg.label it)))
                                        |> Radio.toHtml
                                )
                                (hentValg OppstartValg)
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


omradeMerkelapp : Omrade -> Merkelapp Msg
omradeMerkelapp omrade =
    Merkelapp.merkelapp (FjernValgtOmrade omrade) (Omrade.tittel omrade)


yrkeMerkelapp : Yrke -> Merkelapp Msg
yrkeMerkelapp yrke =
    Merkelapp.merkelapp (FjernValgtYrke yrke) (Yrke.label yrke)


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
visFeilmeldingForEndreYrke model (Skjema.UvalidertSkjema info) typeaheadInfo =
    ( EndreOppsummering (Skjema.UvalidertSkjema { info | visYrkerFeilmelding = True }) typeaheadInfo
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig


visFeilmeldingForEndreOmrade : ModelInfo -> UvalidertSkjema -> TypeaheadOppsummeringInfo -> SamtaleStatus
visFeilmeldingForEndreOmrade model (Skjema.UvalidertSkjema info) typeaheadInfo =
    ( EndreOppsummering (Skjema.UvalidertSkjema { info | visOmraderFeilmelding = True }) typeaheadInfo
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
