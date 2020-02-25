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
import Feilmelding
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Merkelapp as Merkelapp exposing (Merkelapp)
import FrontendModuler.Radio as Radio
import FrontendModuler.Typeahead
import Html exposing (Html)
import Http
import Jobbprofil.Jobbprofil exposing (Jobbprofil)
import Jobbprofil.Kompetanse as Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade as Omrade exposing (Omrade)
import Jobbprofil.Skjema as Skjema exposing (JobbprofilSkjema, SeksjonValg(..), UvalidertSkjema, UvalidertSkjemaInfo, ValidertJobbprofilSkjema, ValidertSkjema, ansettelsesformSammendragFraSkjema, arbeidstidListeFraSkjema, arbeidstidSammendragFraSkjema, geografiSammendragFraSkjema, hentValg, kompetanseSammendragFraSkjema, label, omfangsSammendragFraSkjema, oppstartSammendragFraSkjema, stillingSammendragFraSkjema, tilUvalidertSkjema, tilValidertSkjema)
import Jobbprofil.StegInfo exposing (AnsettelsesformStegInfo, ArbeidstidStegInfo, KompetanseStegInfo, OmfangStegInfo, OmradeStegInfo, OppstartStegInfo, YrkeStegInfo)
import Jobbprofil.Validering exposing (feilmeldingKompetanse, feilmeldingOmråde)
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
    { yrker : ( Typeahead.Model Yrke, Typeahead.Query )
    , omrader : ( Typeahead.Model Omrade, Typeahead.Query )
    , kompetanser : ( Typeahead.Model Kompetanse, Typeahead.Query )
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
    | EndreOppsummering TypeaheadOppsummeringInfo UvalidertSkjema


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
    | VilLagreOppsummering ValidertSkjema
    | JobbprofilEndret SkjemaEndring
    | VilLagreJobbprofil
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



-- Burde bruke uvalidert skjema istedenfor OppsummeringInfo
{- type alias UvalidertSkjema =
   { yrker : List Yrke
   , visYrkerFeilmelding : Bool
   , omrader : List Omrade
   , visOmraderFeilmelding : Bool
   , omfanger : List String
   , arbeidstider : List String
   , ansettelsesformer : List String
   , oppstart : String
   , kompetanser : List Kompetanse
   , visKompetanserFeilmelding : Bool
   }
-}


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

                EndreOppsummering _ _ ->
                    -- Todo: implementer
                    IkkeFerdig ( Model model, Cmd.none )

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
                        |> Maybe.map (logFeilmelding "Hente kompetansetypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                EndreOppsummering _ _ ->
                    -- todo: Gjør det samme som for legg til yrker her
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        OmradeTypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                LeggTilOmrader info typeaheadModel ->
                    updateSamtaleOmradeTypeahead model info typeaheadMsg typeaheadModel

                EndreOppsummering _ skjema ->
                    --Todo: implementer
                    IkkeFerdig ( Model model, Cmd.none )

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
                        |> Maybe.map (logFeilmelding "Hente omradetypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                EndreOppsummering _ _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        YrkeTypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                LeggTilYrker info typeaheadModel ->
                    updateSamtaleYrkeTypeahead model info typeaheadMsg typeaheadModel

                EndreOppsummering _ _ ->
                    --Todo: implementer
                    IkkeFerdig ( Model model, Cmd.none )

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

                EndreOppsummering _ skjema ->
                    -- todo: Gjør det samme som for legg til yrker her
                    ( Model model, Cmd.none )
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
            ( EndreOppsummering { yrker = initYrkeTypeahead, omrader = initOmradeTypeahead, kompetanser = initKompetanseTypeahead } info
                |> oppdaterSamtale model (ManueltSvar (Melding.svar [ "Nei, jeg vil endre" ]))
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        VilLagreOppsummering _ ->
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
                        |> feilmeldingTypeahead
                        |> maybeHvisTrue info.visFeilmelding
                        |> Typeahead.toViewElement Yrke.label typeaheadModel
                        |> FrontendModuler.Typeahead.map YrkeTypeaheadMsg
                    )
                    (List.map (\x -> Merkelapp.merkelapp (FjernValgtYrke x) (Yrke.label x)) info.yrker)

            LeggTilOmfang info ->
                BrukerInput.checkboxGruppeMedGåVidereKnapp VilGåVidereFraOmfang
                    (List.map
                        (\it ->
                            Checkbox.checkbox (Skjema.label it) (JobbprofilEndret (Omfang info (Skjema.label it))) (List.member (Skjema.label it) info.omfanger)
                        )
                        (hentValg OmfangValg)
                    )

            LeggTilArbeidstid info ->
                BrukerInput.checkboxGruppeMedGåVidereKnapp VilGåVidereFraArbeidstid
                    (List.map
                        (\it ->
                            Checkbox.checkbox (Skjema.label it) (JobbprofilEndret (Arbeidstid info (Skjema.label it))) (List.member (Skjema.label it) info.arbeidstider)
                        )
                        (hentValg ArbeidstidValg)
                    )

            LeggTilAnsettelsesform info ->
                BrukerInput.checkboxGruppeMedGåVidereKnapp VilGåVidereFraAnsettelsesform
                    (List.map
                        (\it ->
                            Checkbox.checkbox (Skjema.label it) (JobbprofilEndret (Ansettelsesform info (Skjema.label it))) (List.member (Skjema.label it) info.ansettelsesformer)
                        )
                        (hentValg AnsettelsesformValg)
                    )

            VelgOppstart info ->
                BrukerInput.radioGruppeMedGåVidereKnapp VilGåVidereFraOppstart
                    (List.map
                        (\it ->
                            Radio.radio (Skjema.label it) (Skjema.value it) (JobbprofilEndret (Oppstart info (Skjema.label it))) (info.oppstart == Skjema.label it)
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
                BrukerInput.knapper Kolonne
                    [ Knapp.knapp (VilEndreOppsummering (tilUvalidertSkjema info)) "Nei, jeg vil endre"
                    ]

            EndreOppsummering typeaheadInfo (Skjema.UvalidertSkjema uvalidertSkjema) ->
                BrukerInput.skjema { lagreMsg = VilLagreOppsummering, lagreKnappTekst = "Lagre endringer" }
                    []
        {-
           BrukerInput.skjema { lagreMsg = VilLagreOppsummering, lagreKnappTekst = "Lagre endringer" }
               []
        -}

    else
        BrukerInput.utenInnhold


omradeMerkelapp : Omrade -> Merkelapp Msg
omradeMerkelapp omrade =
    Merkelapp.merkelapp (FjernValgtOmrade omrade) (Omrade.tittel omrade)


yrkeMerkelapp : Yrke -> Merkelapp Msg
yrkeMerkelapp yrke =
    Merkelapp.merkelapp (FjernValgtYrke yrke) (Yrke.label yrke)


feilmeldingTypeahead : List Yrke -> Maybe String
feilmeldingTypeahead yrker =
    if List.length yrker == 0 then
        Just "Skriv inn et yrke eller en stilling. Velg fra listen med forslag som kommer opp."

    else
        Nothing


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
