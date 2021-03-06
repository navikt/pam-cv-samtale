module Jobbprofil.Seksjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , sistLagret
    , subscriptions
    , update
    , viewBrukerInput
    )

import Api
import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Feilmelding
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.CheckboxGruppe as CheckboxGruppe
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Merkelapp as Merkelapp exposing (Merkelapp)
import FrontendModuler.MerkelappGruppe as MerkelappGruppe
import FrontendModuler.Radio as Radio
import FrontendModuler.RadioGruppe as RadioGruppe
import FrontendModuler.Typeahead
import Html exposing (Html)
import Http
import Jobbprofil.Jobbprofil exposing (Jobbprofil, sistEndretDato)
import Jobbprofil.JobbprofilValg as JobbprofilValg exposing (AnsettelsesForm, Arbeidstider(..), Omfang, Oppstart(..), ansettelsesformValg, arbeidstidValg, omfangValg, oppstartValg)
import Jobbprofil.Kompetanse as Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade as Omrade exposing (Omrade)
import Jobbprofil.Skjema as Skjema exposing (Felt(..), UvalidertSkjema, ValidertSkjema)
import Jobbprofil.StegInfo as StegInfo exposing (AnsettelsesformStegInfo, ArbeidstidStegInfo, KompetanseStegInfo, OmfangStegInfo, OmradeStegInfo, OppstartStegInfo, YrkeStegInfo, arbeidstidInfoTilAnsettelsesformInfo, omfangInfoTilArbeidstidInfo, omraderInfoTilOmfangInfo, oppstartInfoTilKompetanseInfo, yrkeStegInfo, yrkerInfoTilOmradeInfo)
import Jobbprofil.Validering exposing (feilmeldingKompetanse, feilmeldingOmråde, feilmeldingOppstart, feilmeldingYrke)
import LagreStatus exposing (LagreStatus)
import List.Extra as List
import Meldinger.Melding as Melding exposing (Melding)
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilFerdigAnimertMeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Person exposing (BrukerInfo(..), Synlighet(..))
import Process
import Result.Extra as Result
import String
import Task
import Throttle exposing (Throttle)
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
    , sistLagretJobbprofil : Maybe Posix
    , brukerInfo : BrukerInfo
    , throttle : Throttle Msg
    }


type alias TypeaheadOppsummeringInfo =
    { yrker : Typeahead.Model Yrke
    , omrader : Typeahead.Model Omrade
    , kompetanser : Typeahead.Model Kompetanse
    }


type Samtale
    = HenterJobbprofil HenteStatus
    | HentingAvJobbprofilFeilet Http.Error
    | HarJobbprofil InitInfo Jobbprofil
    | HarIkkeJobbprofilJobbsøker
    | LeggTilYrker YrkeStegInfo (Typeahead.Model Yrke)
    | LeggTilOmrader OmradeStegInfo (Typeahead.Model Omrade)
    | LeggTilOmfang OmfangStegInfo
    | LeggTilArbeidstid ArbeidstidStegInfo
    | LeggTilAnsettelsesform AnsettelsesformStegInfo
    | VelgOppstart OppstartStegInfo
    | LeggTilKompetanser KompetanseStegInfo (Typeahead.Model Kompetanse)
    | VisOppsummering Bool ValidertSkjema
    | EndreOppsummering UvalidertSkjema TypeaheadOppsummeringInfo
    | LagrerSkjema ValidertSkjema LagreStatus
    | LagringFeilet Http.Error ValidertSkjema
    | VenterPåAnimasjonFørFullføring FullføringStatus


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Posix BrukerInfo FerdigAnimertMeldingsLogg


type FullføringStatus
    = BekrefterOpprinnelig BrukerInfo
    | LagringLyktes BrukerInfo
    | BrukerGikkVidere


type HenteStatus
    = HentetFørsteGang
    | HenterEtterUtlogging { forsøkPåNytt : Bool }
    | HenterEtterError


type alias InitInfo =
    { underOppfølging : Bool
    , henteStatus : HenteStatus
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model info) =
    info.seksjonsMeldingsLogg


sistLagret : Model -> Posix
sistLagret (Model model) =
    case model.sistLagretJobbprofil of
        Nothing ->
            model.sistLagretFraCV

        Just sistLagretJobbprofil ->
            sistLagretJobbprofil


underOppfølging : ModelInfo -> Bool
underOppfølging model =
    case model.brukerInfo of
        UnderOppfølging _ ->
            True

        JobbSkifter _ ->
            False



--- UPDATE ---


type Msg
    = JobbprofilHentet (Result Http.Error Jobbprofil)
    | HentJobbprofilPåNytt
    | VilBegynnePåJobbprofil
    | YrkeTypeaheadMsg (Typeahead.Msg Yrke)
    | HentetYrkeTypeahead Typeahead.Query (Result Http.Error (List Yrke))
    | VilLeggeTilYrke Yrke
    | FjernValgtYrke Yrke
    | VilGåVidereFraYrke
    | OmradeTypeaheadMsg (Typeahead.Msg Omrade)
    | HentetOmradeTypeahead Typeahead.Query (Result Http.Error (List Omrade))
    | VilLeggeTilOmrade Omrade
    | FjernValgtOmrade Omrade
    | VilGåVidereFraOmrade
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
    | SkjemaEndret CheckboxEndring
    | VilLagreOppsummering
    | VilLagreJobbprofil
    | JobbprofilLagret (Result Http.Error Jobbprofil)
    | FerdigMedJobbprofil
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | WindowEndrerVisibility Visibility
    | ErrorLogget
    | FeltMisterFokus
    | TimeoutEtterAtFeltMistetFokus
    | FokusSatt (Result Dom.Error ())
    | UpdateThrottle Posix


type CheckboxEndring
    = OmfangEndret Omfang
    | ArbeidstidEndret Arbeidstider
    | AnsettelsesformEndret AnsettelsesForm
    | OppstartEndret Oppstart


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        JobbprofilHentet result ->
            case model.aktivSamtale of
                HenterJobbprofil henteStatus ->
                    let
                        initInfo =
                            { underOppfølging = underOppfølging model, henteStatus = henteStatus }
                    in
                    case result of
                        Ok jobbprofil ->
                            let
                                oppdatertMeldingslogg =
                                    case henteStatus of
                                        HenterEtterUtlogging _ ->
                                            model.seksjonsMeldingsLogg
                                                |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])

                                        _ ->
                                            model.seksjonsMeldingsLogg
                            in
                            ( HarJobbprofil initInfo jobbprofil
                                |> oppdaterSamtale { model | sistLagretJobbprofil = sistEndretDato jobbprofil, seksjonsMeldingsLogg = oppdatertMeldingslogg } UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            case error of
                                Http.BadStatus 404 ->
                                    -- "Feilen" er at bruker har ikke jobbprofil, gå til neste del av samtalen
                                    let
                                        nesteSamtaleSteg =
                                            if initInfo.underOppfølging then
                                                initYrkeTypeahead
                                                    |> Tuple.first
                                                    |> LeggTilYrker yrkeStegInfo

                                            else
                                                HarIkkeJobbprofilJobbsøker

                                        oppdatertMeldingslogg =
                                            meldingEtterHentingAvJobbprofil initInfo model
                                    in
                                    ( nesteSamtaleSteg
                                        |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg } UtenSvar
                                    , lagtTilSpørsmålCmd model.debugStatus
                                    )
                                        |> IkkeFerdig

                                _ ->
                                    -- Her er det en feil. Sjekk om det er pga utlogging eller annen grunn
                                    case henteStatus of
                                        HenterEtterUtlogging { forsøkPåNytt } ->
                                            if forsøkPåNytt then
                                                ( HenterEtterUtlogging { forsøkPåNytt = False }
                                                    |> HenterJobbprofil
                                                    |> oppdaterSamtale model (SvarFraMsg msg)
                                                , Api.getJobbprofil JobbprofilHentet
                                                )
                                                    |> IkkeFerdig

                                            else
                                                ( HentingAvJobbprofilFeilet error
                                                    |> oppdaterSamtale model IngenNyeMeldinger
                                                , logFeilmelding "Hente jobbprofil" error
                                                )
                                                    |> IkkeFerdig

                                        _ ->
                                            ( HentingAvJobbprofilFeilet error
                                                |> oppdaterSamtale model UtenSvar
                                            , Cmd.batch
                                                [ lagtTilSpørsmålCmd model.debugStatus
                                                , logFeilmelding "Hente jobbprofil" error
                                                ]
                                            )
                                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentJobbprofilPåNytt ->
            case model.aktivSamtale of
                HentingAvJobbprofilFeilet _ ->
                    ( HenterJobbprofil HenterEtterError
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , Api.getJobbprofil JobbprofilHentet
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilBegynnePåJobbprofil ->
            ( initYrkeTypeahead
                |> Tuple.first
                |> LeggTilYrker yrkeStegInfo
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
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
                            |> LeggTilOmrader (yrkerInfoTilOmradeInfo info)
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        OmradeTypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                LeggTilOmrader info typeaheadModel ->
                    updateSamtaleOmradeTypeahead model info typeaheadMsg typeaheadModel

                EndreOppsummering skjema typeaheadInfo ->
                    updateEndreSamtaleOmradeTypeahead model skjema typeaheadMsg typeaheadInfo

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
                        ( omraderInfoTilOmfangInfo info
                            |> LeggTilOmfang
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

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

        VilGåVidereFraOmfang ->
            case model.aktivSamtale of
                LeggTilOmfang info ->
                    ( omfangInfoTilArbeidstidInfo info
                        |> LeggTilArbeidstid
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

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

        VilGåVidereFraArbeidstid ->
            case model.aktivSamtale of
                LeggTilArbeidstid info ->
                    ( arbeidstidInfoTilAnsettelsesformInfo info
                        |> LeggTilAnsettelsesform
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

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

        VilGåVidereFraAnsettelsesform ->
            case model.aktivSamtale of
                LeggTilAnsettelsesform info ->
                    ( StegInfo.ansettelsesformInfoTilOppstartInfo info
                        |> VelgOppstart
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

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
                        Just value ->
                            ( initKompetanseTypeahead
                                |> Tuple.first
                                |> LeggTilKompetanser (oppstartInfoTilKompetanseInfo value info)
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { info | visFeilmelding = True }
                                |> VelgOppstart
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        KompetanseTypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                LeggTilKompetanser info typeaheadModel ->
                    updateSamtaleKompetanseTypeahead model info typeaheadMsg typeaheadModel

                EndreOppsummering skjema typeaheadInfo ->
                    updateEndreSamtaleKompetanseTypeahead model skjema typeaheadMsg typeaheadInfo

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

        VilLeggeTilkompetanse _ ->
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
                        ( StegInfo.kompetanseInfoTilSkjema info
                            |> VisOppsummering True
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilEndreOppsummering ->
            case model.aktivSamtale of
                VisOppsummering _ skjema ->
                    updateEtterVilEndreSkjema model msg skjema

                HarJobbprofil _ jobbprofil ->
                    updateEtterVilEndreSkjema model msg (Skjema.fraJobbprofil jobbprofil)

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

        VilLagreOppsummering ->
            case model.aktivSamtale of
                EndreOppsummering skjema typeaheadinfo ->
                    case Skjema.valider skjema of
                        Just validertSkjema ->
                            ( Skjema.tilValidertSkjema skjema
                                |> VisOppsummering False
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar (oppsummering validertSkjema)))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( EndreOppsummering (Skjema.tillatÅViseAlleFeilmeldinger skjema) typeaheadinfo
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreJobbprofil ->
            case model.aktivSamtale of
                HarJobbprofil _ _ ->
                    ( VenterPåAnimasjonFørFullføring (BekrefterOpprinnelig model.brukerInfo)
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                VisOppsummering _ skjema ->
                    ( LagreStatus.init
                        |> LagrerSkjema skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                      -- Post kalles uansett om man lagrer første gang eller endrer jobbprofil
                    , Api.opprettJobbprofil JobbprofilLagret skjema
                    )
                        |> IkkeFerdig

                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerSkjema skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , Api.opprettJobbprofil JobbprofilLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        JobbprofilLagret result ->
            case model.aktivSamtale of
                LagrerSkjema skjema lagreStatus ->
                    case result of
                        Ok jobbprofil ->
                            let
                                oppdatertMeldingslogg =
                                    if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])

                                    else
                                        model.seksjonsMeldingsLogg
                            in
                            ( VenterPåAnimasjonFørFullføring (LagringLyktes model.brukerInfo)
                                |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg, sistLagretJobbprofil = sistEndretDato jobbprofil } UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , Api.opprettJobbprofil JobbprofilLagret skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre jobbprofil" error
                                    )
                                        |> IkkeFerdig

                            else
                                ( skjema
                                    |> LagringFeilet error
                                    |> oppdaterSamtale model UtenSvar
                                , Cmd.batch
                                    [ lagtTilSpørsmålCmd model.debugStatus
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre jobbprofil" error
                                    ]
                                )
                                    |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FerdigMedJobbprofil ->
            case model.aktivSamtale of
                LagringFeilet _ _ ->
                    ( VenterPåAnimasjonFørFullføring BrukerGikkVidere
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                HentingAvJobbprofilFeilet _ ->
                    ( VenterPåAnimasjonFørFullføring BrukerGikkVidere
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( VenterPåAnimasjonFørFullføring (LagringLyktes model.brukerInfo)
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> ferdigAnimertMeldingsLogg
                        |> Ferdig (sistLagret (Model model)) model.brukerInfo

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            SamtaleAnimasjon.update model.debugStatus samtaleAnimasjonMsg model.seksjonsMeldingsLogg
                |> updateEtterFullførtMelding model

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case model.aktivSamtale of
                        LagrerSkjema skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerSkjema skjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagringFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                ( error
                                    |> LagreStatus.fraError
                                    |> LagrerSkjema skjema
                                    |> oppdaterSamtale model IngenNyeMeldinger
                                , Api.opprettJobbprofil JobbprofilLagret skjema
                                )
                                    |> IkkeFerdig

                            else
                                IkkeFerdig ( Model model, Cmd.none )

                        HenterJobbprofil henteStatus ->
                            let
                                nyHenteStatus =
                                    case henteStatus of
                                        HentetFørsteGang ->
                                            HentetFørsteGang

                                        HenterEtterUtlogging record ->
                                            HenterEtterUtlogging { forsøkPåNytt = True }

                                        HenterEtterError ->
                                            HenterEtterError
                            in
                            ( HenterJobbprofil nyHenteStatus
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        HentingAvJobbprofilFeilet error ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                ( HenterEtterUtlogging { forsøkPåNytt = False }
                                    |> HenterJobbprofil
                                    |> oppdaterSamtale model IngenNyeMeldinger
                                , Api.getJobbprofil JobbprofilHentet
                                )
                                    |> IkkeFerdig

                            else
                                IkkeFerdig ( Model model, Cmd.none )

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Hidden ->
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

        UpdateThrottle _ ->
            let
                ( newThrottle, cmd ) =
                    Throttle.update model.throttle
            in
            IkkeFerdig ( Model { model | throttle = newThrottle }, cmd )


ferdigAnimertMeldingsLogg : ( Model, Cmd Msg ) -> FerdigAnimertMeldingsLogg
ferdigAnimertMeldingsLogg ( Model model, _ ) =
    tilFerdigAnimertMeldingsLogg model.seksjonsMeldingsLogg


meldingEtterHentingAvJobbprofil : InitInfo -> ModelInfo -> MeldingsLogg
meldingEtterHentingAvJobbprofil info model =
    case info.henteStatus of
        HenterEtterUtlogging _ ->
            if info.underOppfølging then
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])
                    |> MeldingsLogg.leggTilSpørsmål
                        [ Melding.spørsmål [ "Det ser ikke ut som at du har lagt inn noen jobbønsker enda, så da begynner vi med det." ]
                        ]

            else
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])
                    |> MeldingsLogg.leggTilSpørsmål
                        [ Melding.spørsmål
                            [ "Det ser ikke ut som at du har lagt inn noen jobbønsker enda."
                                ++ " Vi må vite litt mer om jobbønskene dine for at CV-en skal bli søkbar. Er du klar til å begynne?"
                            ]
                        ]

        HentetFørsteGang ->
            if info.underOppfølging then
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål
                        [ Melding.spørsmål [ "Nå gjenstår bare jobbprofilen." ]
                        ]

            else
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål
                        [ Melding.spørsmål
                            [ "Vi må vite litt mer om jobbønskene dine for at CV-en skal bli søkbar. Er du klar til å begynne?"
                            ]
                        ]

        HenterEtterError ->
            if info.underOppfølging then
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål
                        [ Melding.spørsmål [ "Nå fikk jeg det til! Det ser ikke ut som at du har lagt inn noen jobbønsker enda, så da begynner vi med det." ]
                        ]

            else
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål
                        [ Melding.spørsmål
                            [ "Nå fikk jeg det til! Det ser ikke ut som at du har lagt inn noen jobbønsker enda."
                                ++ " Vi må vite litt mer om jobbønskene dine for at CV-en skal bli søkbar. Er du klar til å begynne?"
                            ]
                        ]



--  UnderOppfølging Synlig ->


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
            case Typeahead.getSuggestionsStatus status of
                GetSuggestionsForInput query ->
                    let
                        ( newThrottle, throttledCmd ) =
                            Throttle.try
                                (Api.getKompetanseJobbprofilTypeahead HentetKompetanseTypeahead query)
                                model.throttle
                    in
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> LeggTilKompetanser info
                            |> oppdaterSamtale { model | throttle = newThrottle } IngenNyeMeldinger
                        , throttledCmd
                        )

                DoNothing ->
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> LeggTilKompetanser info
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
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
            case Typeahead.getSuggestionsStatus status of
                GetSuggestionsForInput query ->
                    let
                        ( newThrottle, throttledCmd ) =
                            Throttle.try
                                (Api.getKompetanseJobbprofilTypeahead HentetKompetanseTypeahead query)
                                model.throttle
                    in
                    IkkeFerdig
                        ( EndreOppsummering info { typeaheadModel | kompetanser = nyTypeaheadModel }
                            |> oppdaterSamtale { model | throttle = newThrottle } IngenNyeMeldinger
                        , throttledCmd
                        )

                DoNothing ->
                    IkkeFerdig
                        ( EndreOppsummering info { typeaheadModel | kompetanser = nyTypeaheadModel }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
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
            case Typeahead.getSuggestionsStatus status of
                GetSuggestionsForInput query ->
                    let
                        ( newThrottle, throttledCmd ) =
                            Throttle.try
                                (Api.getOmradeJobbprofilTypeahead HentetOmradeTypeahead query)
                                model.throttle
                    in
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> LeggTilOmrader info
                            |> oppdaterSamtale { model | throttle = newThrottle } IngenNyeMeldinger
                        , throttledCmd
                        )

                DoNothing ->
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> LeggTilOmrader info
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
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
            case Typeahead.getSuggestionsStatus status of
                GetSuggestionsForInput query ->
                    let
                        ( newThrottle, throttledCmd ) =
                            Throttle.try
                                (Api.getOmradeJobbprofilTypeahead HentetOmradeTypeahead query)
                                model.throttle
                    in
                    IkkeFerdig
                        ( EndreOppsummering info { typeaheadModel | omrader = nyTypeaheadModel }
                            |> oppdaterSamtale { model | throttle = newThrottle } IngenNyeMeldinger
                        , throttledCmd
                        )

                DoNothing ->
                    IkkeFerdig
                        ( EndreOppsummering info { typeaheadModel | omrader = nyTypeaheadModel }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
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

        erUnderOppfølging =
            underOppfølging model
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just yrke ->
                    ( nyTypeaheadModel
                        |> LeggTilYrker { yrker = List.append info.yrker [ yrke ], visFeilmelding = False }
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
            case Typeahead.getSuggestionsStatus status of
                GetSuggestionsForInput query ->
                    let
                        ( newThrottle, throttledCmd ) =
                            Throttle.try
                                (Api.getYrkeJobbprofilTypeahead HentetYrkeTypeahead query)
                                model.throttle
                    in
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> LeggTilYrker info
                            |> oppdaterSamtale { model | throttle = newThrottle } IngenNyeMeldinger
                        , throttledCmd
                        )

                DoNothing ->
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> LeggTilYrker info
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
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
            case Typeahead.getSuggestionsStatus status of
                GetSuggestionsForInput query ->
                    let
                        ( newThrottle, throttledCmd ) =
                            Throttle.try
                                (Api.getYrkeJobbprofilTypeahead HentetYrkeTypeahead query)
                                model.throttle
                    in
                    IkkeFerdig
                        ( EndreOppsummering info { typeaheadModel | yrker = nyTypeaheadModel }
                            |> oppdaterSamtale { model | throttle = newThrottle } IngenNyeMeldinger
                        , throttledCmd
                        )

                DoNothing ->
                    IkkeFerdig
                        ( EndreOppsummering info { typeaheadModel | yrker = nyTypeaheadModel }
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
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
                VenterPåAnimasjonFørFullføring _ ->
                    Ferdig (sistLagret (Model model)) model.brukerInfo ferdigAnimertSamtale

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
        HenterJobbprofil _ ->
            []

        HentingAvJobbprofilFeilet error ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "se om du allerede har lagt inn noen jobbønsker" }
            ]

        HarJobbprofil info jobbprofil ->
            let
                setning =
                    case info.henteStatus of
                        HenterEtterUtlogging _ ->
                            if info.underOppfølging then
                                "Jeg ser du har lagt inn dette tidligere:"

                            else
                                " Jeg ser du har en jobbprofil fra før av. Du har lagt inn dette:"

                        HentetFørsteGang ->
                            if info.underOppfølging then
                                "Nå gjenstår bare jobbprofilen. Jeg ser du har lagt inn dette tidligere:"

                            else
                                "Jeg ser du har en jobbprofil fra før av. Du har lagt inn dette:"

                        HenterEtterError ->
                            if info.underOppfølging then
                                "Nå fikk jeg det til! Jeg ser du har lagt inn dette tidligere:"

                            else
                                "Nå fikk jeg det til! Jeg ser du har en jobbprofil fra før av. Du har lagt inn dette:"
            in
            [ Melding.spørsmål
                (List.concat
                    [ [ setning
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

        HarIkkeJobbprofilJobbsøker ->
            []

        LeggTilYrker _ _ ->
            [ Melding.spørsmål [ "Hva slags stillinger eller yrker ser du etter? For eksempel møbelsnekker eller butikkmedarbeider." ]
            , Melding.spørsmål [ "Du kan legge til flere stillinger eller yrker" ]
            ]

        LeggTilOmrader _ _ ->
            [ Melding.spørsmål [ "Hvor vil du jobbe? For eksempel Oslo eller Kristiansund." ] ]

        LeggTilOmfang _ ->
            [ Melding.spørsmål [ "Vil du jobbe heltid eller deltid?" ] ]

        LeggTilArbeidstid _ ->
            [ Melding.spørsmål [ "Når kan du jobbe?" ] ]

        LeggTilAnsettelsesform _ ->
            [ Melding.spørsmål [ "Hva slags ansettelse ønsker du?" ] ]

        VelgOppstart _ ->
            [ Melding.spørsmål [ "Når kan du begynne i ny jobb?" ] ]

        LeggTilKompetanser _ _ ->
            [ Melding.spørsmål [ "Tenk på kunnskapene og ferdighetene dine fra jobb eller utdanning." ] ]

        VisOppsummering forsteGang info ->
            let
                ( startSetning, sluttSetning ) =
                    case forsteGang of
                        True ->
                            ( "Du har lagt inn dette:", "Er informasjonen riktig?" )

                        False ->
                            ( "Du har endret til dette:", "Er informasjonen riktig nå?" )
            in
            [ Melding.spørsmål
                (List.concat
                    [ [ startSetning
                      , Melding.tomLinje
                      ]
                    , info
                        |> oppsummering
                    , [ Melding.tomLinje
                      , sluttSetning
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

        VenterPåAnimasjonFørFullføring fullføringStatus ->
            case fullføringStatus of
                BekrefterOpprinnelig brukerInfo ->
                    robotSvarEtterFullførtJobbprofil True brukerInfo

                LagringLyktes brukerInfo ->
                    robotSvarEtterFullførtJobbprofil False brukerInfo

                BrukerGikkVidere ->
                    [ Melding.spørsmål [ "Da går vi videre." ] ]


robotSvarEtterFullførtJobbprofil : Bool -> BrukerInfo -> List Melding
robotSvarEtterFullførtJobbprofil bekreftOpprinnelig brukerInfo =
    let
        førsteSetning =
            if bekreftOpprinnelig then
                "Bra! 👍👍 "

            else
                "Bra innsats! 👍👍 "
    in
    case brukerInfo of
        JobbSkifter IkkeSynlig ->
            [ Melding.spørsmål [ førsteSetning ++ "For å bli synlig må du gjøre CV-en søkbar senere på Min side." ]
            ]

        JobbSkifter Synlig ->
            [ Melding.spørsmål [ førsteSetning ++ "Nå er du søkbar 😊" ]
            ]

        UnderOppfølging IkkeSynlig ->
            [ Melding.spørsmål [ førsteSetning ++ "NAV-veiledere kan nå søke opp CV-en din. Hvis du ønsker at arbeidsgivere skal kunne søke deg opp, må du kontakte NAV-veilederen din." ]
            ]

        UnderOppfølging Synlig ->
            [ Melding.spørsmål [ førsteSetning ++ "Arbeidsgivere og NAV-veiledere kan nå søke opp CV-din. De kan kontakte deg hvis de har en jobb som passer for deg." ]
            ]


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
    = GiOppHentingId
    | BekreftJobbprofilId
    | BekreftOppsummeringId
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
    | LagringFeiletProvIgjenId
    | LagringFeiletGaVidereId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        GiOppHentingId ->
            "jobbprofil-gi-opp-henting-id"

        BekreftJobbprofilId ->
            "jobbprofil-bekreft-id"

        BekreftOppsummeringId ->
            "jobbprofil-bekreft-oppsummering-id"

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

        LagringFeiletProvIgjenId ->
            "jobbprofil-lagring-feilet-prov-igjen-id"

        LagringFeiletGaVidereId ->
            "jobbprofil-lagring-feilet-ga-videre-id"


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        HentingAvJobbprofilFeilet _ ->
            settFokusCmd GiOppHentingId

        HarJobbprofil _ _ ->
            settFokusCmd BekreftJobbprofilId

        HarIkkeJobbprofilJobbsøker ->
            settFokusCmd BegynnPåJobbprofilId

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

        VisOppsummering _ _ ->
            settFokusCmd BekreftOppsummeringId

        LagringFeilet error _ ->
            case ErrorHåndtering.operasjonEtterError error of
                GiOpp ->
                    settFokusCmd LagringFeiletGaVidereId

                _ ->
                    settFokusCmd LagringFeiletProvIgjenId

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
            HenterJobbprofil _ ->
                BrukerInput.utenInnhold

            HentingAvJobbprofilFeilet error ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp FerdigMedJobbprofil "Gå videre"
                                |> Knapp.withId (inputIdTilString GiOppHentingId)
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp HentJobbprofilPåNytt "Prøv igjen"
                                |> Knapp.withId (inputIdTilString GiOppHentingId)
                            , Knapp.knapp VilBegynnePåJobbprofil "Legg inn jobbønsker på nytt"
                            , Knapp.knapp FerdigMedJobbprofil "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            HarJobbprofil _ _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilLagreJobbprofil "Ja, informasjonen er riktig"
                        |> Knapp.withId (inputIdTilString BekreftJobbprofilId)
                    , Knapp.knapp VilEndreOppsummering "Nei, jeg vil endre"
                    ]

            HarIkkeJobbprofilJobbsøker ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilBegynnePåJobbprofil "Ja!"
                        |> Knapp.withId (inputIdTilString BegynnPåJobbprofilId)
                    ]

            LeggTilYrker info typeaheadModel ->
                BrukerInput.typeaheadMedMerkelapperOgGåVidereKnapp VilGåVidereFraYrke
                    (info.yrker
                        |> feilmeldingYrke
                        |> maybeHvisTrue info.visFeilmelding
                        |> Typeahead.toViewElement Yrke.label typeaheadModel
                        |> FrontendModuler.Typeahead.map YrkeTypeaheadMsg
                    )
                    (List.map (\x -> Merkelapp.merkelapp (FjernValgtYrke x) (Yrke.label x)) info.yrker
                        |> MerkelappGruppe.merkelappGruppe
                    )

            LeggTilOmrader info typeaheadModel ->
                let
                    merkelapper =
                        List.map (\x -> Merkelapp.merkelapp (FjernValgtOmrade x) (Omrade.tittel x)) info.omrader
                in
                BrukerInput.typeaheadMedMerkelapperOgGåVidereKnapp VilGåVidereFraOmrade
                    (info.omrader
                        |> feilmeldingOmråde
                        |> maybeHvisTrue info.visFeilmelding
                        |> Typeahead.toViewElement Omrade.tittel typeaheadModel
                        |> FrontendModuler.Typeahead.map OmradeTypeaheadMsg
                    )
                    (merkelapper
                        |> MerkelappGruppe.merkelappGruppe
                    )

            LeggTilOmfang info ->
                BrukerInput.checkboxGruppeMedGåVidereKnapp VilGåVidereFraOmfang
                    (List.map
                        (\it ->
                            Checkbox.fromCheckboxInfo
                                { label = JobbprofilValg.omfangLabel it
                                , onClick = OppdatererOmfang it
                                , checked = List.member it info.omfanger
                                }
                                |> Checkbox.withId (byggOmfangValgId it)
                        )
                        omfangValg
                    )

            LeggTilArbeidstid info ->
                BrukerInput.checkboxGruppeMedGåVidereKnapp VilGåVidereFraArbeidstid
                    (List.map
                        (\it ->
                            { label = JobbprofilValg.arbeidstidLabel it
                            , onClick = OppdatererArbeidstid it
                            , checked = List.member it info.arbeidstider
                            }
                                |> Checkbox.fromCheckboxInfo
                                |> Checkbox.withId (byggArbeidstidValgId it)
                        )
                        arbeidstidValg
                    )

            LeggTilAnsettelsesform info ->
                BrukerInput.checkboxGruppeMedGåVidereKnapp VilGåVidereFraAnsettelsesform
                    (List.map
                        (\it ->
                            { label = JobbprofilValg.ansettelsesFormLabel it
                            , onClick = OppdatererAnsettelsesform it
                            , checked = List.member it info.ansettelsesformer
                            }
                                |> Checkbox.fromCheckboxInfo
                                |> Checkbox.withId (byggAnsettelsesformValgId it)
                        )
                        ansettelsesformValg
                    )

            VelgOppstart info ->
                let
                    feilmelding =
                        (feilmeldingOppstart >> maybeHvisTrue info.visFeilmelding) info.oppstart

                    radioknapper =
                        List.map
                            (\it ->
                                Radio.radio
                                    { gruppeNavn = "Oppstart"
                                    , label = JobbprofilValg.oppstartLabel it
                                    , onSelected = OppdatererOppstart it
                                    , selected =
                                        info.oppstart
                                            == Just it
                                    }
                                    |> Radio.withId (byggOppstartValgId it)
                            )
                            oppstartValg
                in
                BrukerInput.radioGruppeMedGåVidereKnapp VilGåVidereFraOppstart
                    ({ legend = "Velg når du er ledig fra"
                     , radioknapper = radioknapper
                     }
                        |> RadioGruppe.radioGruppe
                        |> RadioGruppe.withFeilmelding feilmelding
                    )

            LeggTilKompetanser info typeaheadModel ->
                BrukerInput.typeaheadMedMerkelapperOgGåVidereKnapp VilGåVidereFraKompetanse
                    (info.kompetanser
                        |> feilmeldingKompetanse
                        |> maybeHvisTrue info.visFeilmelding
                        |> Typeahead.toViewElement Kompetanse.label typeaheadModel
                        |> FrontendModuler.Typeahead.map KompetanseTypeaheadMsg
                    )
                    (List.map (\x -> Merkelapp.merkelapp (FjernValgtKompetanse x) (Kompetanse.label x)) info.kompetanser
                        |> MerkelappGruppe.merkelappGruppe
                    )

            VisOppsummering forsteGang _ ->
                let
                    setning =
                        case forsteGang of
                            True ->
                                "Ja, det er riktig"

                            False ->
                                "Ja, nå er det riktig"
                in
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilLagreJobbprofil
                        setning
                        |> Knapp.withId (inputIdTilString BekreftOppsummeringId)
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
                    , List.map (\x -> Merkelapp.merkelapp (FjernValgtYrke x) (Yrke.label x)) stillinger
                        |> MerkelappGruppe.merkelappGruppe
                        |> MerkelappGruppe.toHtml
                    , omrader
                        |> feilmeldingOmråde
                        |> Typeahead.view Omrade.tittel typeaheadInfo.omrader
                        |> Html.map OmradeTypeaheadMsg
                    , List.map (\x -> Merkelapp.merkelapp (FjernValgtOmrade x) (Omrade.tittel x)) omrader
                        |> MerkelappGruppe.merkelappGruppe
                        |> MerkelappGruppe.toHtml
                    , let
                        checkboxer =
                            List.map
                                (\it ->
                                    Checkbox.fromCheckboxInfo
                                        { label = JobbprofilValg.omfangLabel it
                                        , onClick = SkjemaEndret (OmfangEndret it)
                                        , checked = List.member it omfanger
                                        }
                                )
                                omfangValg
                      in
                      { legend = "Vil du jobbe heltid eller deltid?"
                      , checkboxer = checkboxer
                      }
                        |> CheckboxGruppe.checkboxGruppe
                        |> CheckboxGruppe.toHtml
                    , let
                        checkboxer =
                            List.map
                                (\it ->
                                    Checkbox.fromCheckboxInfo
                                        { label = JobbprofilValg.arbeidstidLabel it
                                        , onClick = SkjemaEndret (ArbeidstidEndret it)
                                        , checked = List.member it arbeidstider
                                        }
                                )
                                arbeidstidValg
                      in
                      { legend = "Når kan du jobbe?"
                      , checkboxer = checkboxer
                      }
                        |> CheckboxGruppe.checkboxGruppe
                        |> CheckboxGruppe.toHtml
                    , let
                        checkboxer =
                            List.map
                                (\it ->
                                    Checkbox.fromCheckboxInfo
                                        { label = JobbprofilValg.ansettelsesFormLabel it
                                        , onClick = SkjemaEndret (AnsettelsesformEndret it)
                                        , checked = List.member it ansettelsesformer
                                        }
                                )
                                ansettelsesformValg
                      in
                      { legend = "Hva slags ansettelse ønsker du?"
                      , checkboxer = checkboxer
                      }
                        |> CheckboxGruppe.checkboxGruppe
                        |> CheckboxGruppe.toHtml
                    , let
                        radioknapper =
                            List.map
                                (\it ->
                                    Radio.radio
                                        { gruppeNavn = "Oppstart"
                                        , label = JobbprofilValg.oppstartLabel it
                                        , onSelected = SkjemaEndret (OppstartEndret it)
                                        , selected =
                                            oppstart
                                                == Just it
                                        }
                                )
                                oppstartValg
                      in
                      { legend = "Når kan du begynne i ny jobb?"
                      , radioknapper = radioknapper
                      }
                        |> RadioGruppe.radioGruppe
                        |> RadioGruppe.withFeilmelding (feilmeldingOppstart oppstart)
                        |> RadioGruppe.toHtml
                    , kompetanser
                        |> feilmeldingKompetanse
                        |> Typeahead.view Kompetanse.label typeaheadInfo.kompetanser
                        |> Html.map KompetanseTypeaheadMsg
                    , List.map (\x -> Merkelapp.merkelapp (FjernValgtKompetanse x) (Kompetanse.label x)) kompetanser
                        |> MerkelappGruppe.merkelappGruppe
                        |> MerkelappGruppe.toHtml
                    ]

            LagrerSkjema _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    BrukerInput.utenInnhold

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp FerdigMedJobbprofil "Gå videre"
                                |> Knapp.withId (inputIdTilString LagringFeiletGaVidereId)
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp VilLagreJobbprofil "Prøv igjen"
                                |> Knapp.withId (inputIdTilString LagringFeiletProvIgjenId)
                            , Knapp.knapp FerdigMedJobbprofil "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            VenterPåAnimasjonFørFullføring _ ->
                BrukerInput.utenInnhold

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
            HenterJobbprofil HentetFørsteGang
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
        , sistLagretJobbprofil = Nothing
        , throttle = Throttle.create 1
        }
    , Cmd.batch
        [ lagtTilSpørsmålCmd debugStatus
        , Api.getJobbprofil JobbprofilHentet
        ]
    )


initYrkeTypeahead : ( Typeahead.Model Yrke, Typeahead.Query )
initYrkeTypeahead =
    Typeahead.init
        { value = ""
        , label = "Stillinger/yrker"
        , id = inputIdTilString StillingYrkeTypeaheadId
        , toString = Yrke.label
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


initKompetanseTypeahead : ( Typeahead.Model Kompetanse, Typeahead.Query )
initKompetanseTypeahead =
    Typeahead.init
        { value = ""
        , label = "Kompetanser"
        , id = inputIdTilString KompetanseTypeaheadId
        , toString = Kompetanse.label
        }
        |> Tuple.mapFirst Typeahead.withSubmitOnElementSelected


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Browser.Events.onVisibilityChange WindowEndrerVisibility
        , model.seksjonsMeldingsLogg
            |> SamtaleAnimasjon.subscriptions
            |> Sub.map SamtaleAnimasjonMsg
        , Throttle.ifNeeded
            (Time.every 200 UpdateThrottle)
            model.throttle
        ]
