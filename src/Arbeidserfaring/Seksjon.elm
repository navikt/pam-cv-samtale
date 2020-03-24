module Arbeidserfaring.Seksjon exposing
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
import Arbeidserfaring.Arbeidserfaring as Arbeidserfaring exposing (Arbeidserfaring)
import Arbeidserfaring.Skjema as Skjema exposing (ArbeidserfaringSkjema, Felt(..), ValidertArbeidserfaringSkjema)
import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import Dato.Dato as Dato exposing (TilDato(..), År)
import Dato.Maned as Måned exposing (Måned(..))
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Feilmelding
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.BrukerInputMedGaVidereKnapp as BrukerInputMedGåVidereKnapp
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp exposing (Knapp)
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Textarea as Textarea
import FrontendModuler.Typeahead
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (class)
import Http exposing (Error)
import LagreStatus exposing (LagreStatus)
import Maybe.Extra as Maybe
import Meldinger.Melding as Melding exposing (Melding, Tekstområde(..))
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Process
import Result.Extra as Result
import Task
import Tid exposing (nyesteSistLagretVerdi)
import Time exposing (Posix)
import Typeahead.Typeahead as Typeahead exposing (GetSuggestionStatus(..), InputStatus(..))
import Validering



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , arbeidserfaringListe : List Arbeidserfaring
    , aktivSamtale : Samtale
    , debugStatus : DebugStatus
    , sistLagretFraForrigeSeksjon : Posix
    , harFullførtEnSamtale : Bool
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


sistLagret : Model -> Posix
sistLagret (Model model) =
    model.arbeidserfaringListe
        |> List.map Arbeidserfaring.sistEndretDato
        |> nyesteSistLagretVerdi model.sistLagretFraForrigeSeksjon


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Posix FerdigAnimertMeldingsLogg


type AvsluttetGrunn
    = AvbruttPåbegynt
    | SlettetPåbegynt
    | AnnenAvslutning
    | EndretEksisterende


type OppsummeringsType
    = FørsteGang
    | EtterEndring
    | AvbrøtSletting
    | NyArbeidserfaring


type SkjemaType
    = OppsummeringsSkjema
    | NyArbeidserfaringsSkjema


type Samtale
    = Intro
    | VelgEnArbeidserfaringÅRedigere
    | RegistrerYrke { visFeilmelding : Bool } (Typeahead.Model Yrke)
    | SpørOmBrukerVilEndreJobbtittel JobbtittelInfo
    | EndreJobbtittel JobbtittelInfo
    | RegistrereBedriftsnavn BedriftnavnInfo
    | RegistrereSted StedInfo
    | RegistrereArbeidsoppgaver Bool ArbeidsoppgaverInfo
    | RegistrereFraDato FraDatoInfo
    | RegistrereNåværende NåværendeInfo
    | RegistrereTilDato TilDatoInfo
    | VisOppsummering OppsummeringsType ValidertArbeidserfaringSkjema
    | RedigerSkjema SkjemaType (Typeahead.Model Yrke) ArbeidserfaringSkjema
    | BekreftSlettingAvPåbegynt ValidertArbeidserfaringSkjema
    | LagrerArbeidserfaring ValidertArbeidserfaringSkjema LagreStatus
    | LagringFeilet Http.Error ValidertArbeidserfaringSkjema
    | SpørOmBrukerVilLeggeInnMer (List Arbeidserfaring) AvsluttetGrunn
    | BekreftAvbrytingAvRegistreringen Samtale
    | VenterPåAnimasjonFørFullføring String



--- UPDATE ---


type Msg
    = BrukerVilLeggeTilNyArbeidserfaring
    | BrukerVilRedigereArbeidserfaring
    | BrukerHarValgtArbeidserfaringÅRedigere Arbeidserfaring
    | BrukerHopperOverArbeidserfaring
    | TypeaheadMsg (Typeahead.Msg Yrke)
    | HentetYrkeTypeahead Typeahead.Query (Result Http.Error (List Yrke))
    | FeltMisterFokus
    | TimeoutEtterAtFeltMistetFokus
    | BrukerVilRegistrereYrke
    | BrukerVilEndreJobbtittel
    | BrukerVilIkkeEndreJobbtittel
    | BrukerOppdatererJobbtittelFelt String
    | BrukerVilRegistrereJobbtittel
    | BrukerOppdatererBedriftsnavn String
    | BrukerVilRegistrereBedriftsnavn
    | BrukerOppdatererSted String
    | BrukerVilRegistrereSted
    | VilSeEksempel
    | BrukerOppdatererArbeidsoppgaver String
    | BrukerVilRegistrereArbeidsoppgaver
    | BrukerOppdatererFraMåned String
    | BrukerOppdatererFraÅr String
    | BrukerVilRegistrereFraDato
    | BrukerSvarerJaTilNåværende
    | BrukerSvarerNeiTilNåværende
    | BrukerOppdatererTilMåned String
    | BrukerOppdatererTilÅr String
    | BrukerVilRegistrereTilDato
    | BrukerVilRedigereOppsummering
    | SkjemaEndret SkjemaEndring
    | VilSlettePåbegynt
    | BekrefterSlettPåbegynt
    | AngrerSlettPåbegynt
    | BrukerVilLagreArbeidserfaringIOppsummering
    | BrukerVilLagreArbeidserfaringSkjema
    | ArbeidserfaringLagret (Result Http.Error (List Arbeidserfaring))
    | BrukerVilPrøveÅLagrePåNytt
    | BrukerVilAvbryteLagringen
    | BrukerVilAvbryteRegistreringen
    | BrukerBekrefterAvbrytingAvRegistrering
    | BrukerVilIkkeAvbryteRegistreringen
    | VilLeggeTilFlereArbeidserfaringer
    | FerdigMedArbeidserfaring
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())
    | ErrorLogget


type SkjemaEndring
    = Tekst Felt String
    | NåværendeToggled
    | FraMåned String
    | TilMåned String
    | FraÅrBlurred
    | TilÅrBlurred


type alias JobbtittelInfo =
    { tidligereInfo : Yrke
    , jobbtittel : String
    }


type alias BedriftnavnInfo =
    { tidligereInfo : JobbtittelInfo
    , bedriftNavn : String
    }


type alias StedInfo =
    { tidligereInfo : BedriftnavnInfo
    , lokasjon : String
    }


type alias ArbeidsoppgaverInfo =
    { tidligereInfo : StedInfo
    , arbeidsoppgaver : String
    }


type alias FraDatoInfo =
    { tidligereInfo : ArbeidsoppgaverInfo
    , fraMåned : Måned
    , fraÅr : String
    , visFeilmeldingFraÅr : Bool
    }


type alias NåværendeInfo =
    { yrke : Yrke
    , jobbtittel : String
    , bedriftNavn : String
    , lokasjon : String
    , arbeidsoppgaver : String
    , fraÅr : År
    , fraMåned : Måned
    }


type alias TilDatoInfo =
    { nåværendeInfo : NåværendeInfo
    , tilMåned : Måned
    , tilÅr : String
    , visFeilmeldingTilÅr : Bool
    }


maxLengthArbeidsoppgaver =
    2000


yrkeInfoTilJobbtittelInfo : Yrke -> JobbtittelInfo
yrkeInfoTilJobbtittelInfo yrkeTypeahead =
    { tidligereInfo = yrkeTypeahead, jobbtittel = "" }


jobbtittelInfoTilBedriftnavnsInfo : JobbtittelInfo -> BedriftnavnInfo
jobbtittelInfoTilBedriftnavnsInfo jobbtittelInfo =
    { tidligereInfo = jobbtittelInfo
    , bedriftNavn = ""
    }


bedriftnavnsInfoTilLokasjonInfo : BedriftnavnInfo -> StedInfo
bedriftnavnsInfoTilLokasjonInfo beriftnavnsInfo =
    { tidligereInfo = beriftnavnsInfo
    , lokasjon = ""
    }


stedInfoTilArbeidsoppgaverInfo : StedInfo -> ArbeidsoppgaverInfo
stedInfoTilArbeidsoppgaverInfo lokasjonInfo =
    { tidligereInfo = lokasjonInfo
    , arbeidsoppgaver = ""
    }


arbeidsoppgaverInfoTilFraDatoInfo : ArbeidsoppgaverInfo -> FraDatoInfo
arbeidsoppgaverInfoTilFraDatoInfo arbeidsoppgaverInfo =
    { tidligereInfo = arbeidsoppgaverInfo
    , fraMåned = Januar
    , fraÅr = ""
    , visFeilmeldingFraÅr = False
    }


fraDatoTilNåværende : FraDatoInfo -> År -> NåværendeInfo
fraDatoTilNåværende info år =
    { yrke = info.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo
    , jobbtittel = info.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.jobbtittel
    , bedriftNavn = info.tidligereInfo.tidligereInfo.tidligereInfo.bedriftNavn
    , lokasjon = info.tidligereInfo.tidligereInfo.lokasjon
    , arbeidsoppgaver = info.tidligereInfo.arbeidsoppgaver
    , fraMåned = info.fraMåned
    , fraÅr = år
    }


nåværendeInfoTilTilDatoInfo : NåværendeInfo -> TilDatoInfo
nåværendeInfoTilTilDatoInfo nåværendeInfo =
    { nåværendeInfo = nåværendeInfo
    , tilMåned = Januar
    , tilÅr = ""
    , visFeilmeldingTilÅr = False
    }


nåværendeInfoTilSkjema : NåværendeInfo -> ValidertArbeidserfaringSkjema
nåværendeInfoTilSkjema nåværendeInfo =
    Skjema.initValidertSkjema
        { yrke = nåværendeInfo.yrke
        , jobbTittel = nåværendeInfo.jobbtittel
        , bedriftNavn = nåværendeInfo.bedriftNavn
        , lokasjon = nåværendeInfo.lokasjon
        , arbeidsoppgaver = nåværendeInfo.arbeidsoppgaver
        , fraMåned = nåværendeInfo.fraMåned
        , fraÅr = nåværendeInfo.fraÅr
        , tilDato = Nåværende
        , id = Nothing
        }


tilDatoInfoTilSkjema : TilDatoInfo -> År -> ValidertArbeidserfaringSkjema
tilDatoInfoTilSkjema info år =
    Skjema.initValidertSkjema
        { yrke = info.nåværendeInfo.yrke
        , jobbTittel = info.nåværendeInfo.jobbtittel
        , bedriftNavn = info.nåværendeInfo.bedriftNavn
        , lokasjon = info.nåværendeInfo.lokasjon
        , arbeidsoppgaver = info.nåværendeInfo.arbeidsoppgaver
        , fraMåned = info.nåværendeInfo.fraMåned
        , fraÅr = info.nåværendeInfo.fraÅr
        , tilDato = Avsluttet info.tilMåned år
        , id = Nothing
        }


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        BrukerHopperOverArbeidserfaring ->
            ( VenterPåAnimasjonFørFullføring "Ok, da går vi videre."
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilRedigereArbeidserfaring ->
            ( VelgEnArbeidserfaringÅRedigere
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerHarValgtArbeidserfaringÅRedigere arbeidserfaring ->
            let
                ( typeaheadModel, query ) =
                    initSkjemaTypeaheadFraArbeidserfaring arbeidserfaring
            in
            ( arbeidserfaring
                |> Skjema.fraArbeidserfaring
                |> RedigerSkjema OppsummeringsSkjema typeaheadModel
                |> oppdaterSamtale model (SvarFraMsg msg)
            , Cmd.batch
                [ lagtTilSpørsmålCmd model.debugStatus
                , Api.getYrkeTypeahead HentetYrkeTypeahead query
                ]
            )
                |> IkkeFerdig

        BrukerVilLeggeTilNyArbeidserfaring ->
            ( initSamtaleTypeahead
                |> Tuple.first
                |> RegistrerYrke { visFeilmelding = False }
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        TypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                RegistrerYrke visFeilmelding typeaheadModel ->
                    updateSamtaleTypeahead model visFeilmelding typeaheadMsg typeaheadModel

                RedigerSkjema skjemaType gammelTypeaheadModel skjema ->
                    let
                        ( nyTypeaheadModel, status ) =
                            Typeahead.update Yrke.label typeaheadMsg gammelTypeaheadModel
                    in
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> Typeahead.selected
                            |> Skjema.oppdaterYrke skjema
                            |> Skjema.gjørFeilmeldingYrkeSynlig (Typeahead.inputStatus status == InputBlurred)
                            |> RedigerSkjema skjemaType nyTypeaheadModel
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , case Typeahead.getSuggestionsStatus status of
                            GetSuggestionsForInput query ->
                                Api.getYrkeTypeahead HentetYrkeTypeahead query

                            DoNothing ->
                                Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetYrkeTypeahead query result ->
            case model.aktivSamtale of
                RegistrerYrke visFeilmelding typeaheadModel ->
                    ( result
                        |> Typeahead.updateSuggestions Yrke.label typeaheadModel query
                        |> RegistrerYrke visFeilmelding
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente Yrketypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                RedigerSkjema skjemaType typeaheadModel skjema ->
                    ( RedigerSkjema skjemaType (Typeahead.updateSuggestions Yrke.label typeaheadModel query result) skjema
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

        FeltMisterFokus ->
            IkkeFerdig ( Model model, mistetFokusCmd )

        TimeoutEtterAtFeltMistetFokus ->
            case model.aktivSamtale of
                RegistrerYrke _ typeaheadModel ->
                    visFeilmeldingRegistrerYrke model typeaheadModel

                RegistrereFraDato info ->
                    ( { info | visFeilmeldingFraÅr = True }
                        |> RegistrereFraDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrereTilDato tilÅrInfo ->
                    ( { tilÅrInfo | visFeilmeldingTilÅr = True }
                        |> RegistrereTilDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereYrke ->
            case model.aktivSamtale of
                RegistrerYrke _ typeaheadModel ->
                    case Typeahead.selected typeaheadModel of
                        Just yrke ->
                            brukerVelgerYrke model msg yrke

                        Nothing ->
                            visFeilmeldingRegistrerYrke model typeaheadModel

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilEndreJobbtittel ->
            case model.aktivSamtale of
                SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
                    ( jobbtittelInfo
                        |> EndreJobbtittel
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerOppdatererJobbtittelFelt string ->
            case model.aktivSamtale of
                EndreJobbtittel jobbtittelInfo ->
                    ( { jobbtittelInfo | jobbtittel = string }
                        |> EndreJobbtittel
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilIkkeEndreJobbtittel ->
            case model.aktivSamtale of
                SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
                    ( jobbtittelInfo
                        |> jobbtittelInfoTilBedriftnavnsInfo
                        |> RegistrereBedriftsnavn
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        BrukerVilRegistrereJobbtittel ->
            case model.aktivSamtale of
                EndreJobbtittel jobbtittelInfo ->
                    ( jobbtittelInfo
                        |> jobbtittelInfoTilBedriftnavnsInfo
                        |> RegistrereBedriftsnavn
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        BrukerOppdatererBedriftsnavn string ->
            case model.aktivSamtale of
                RegistrereBedriftsnavn beriftnavnsInfo ->
                    ( { beriftnavnsInfo | bedriftNavn = string }
                        |> RegistrereBedriftsnavn
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereBedriftsnavn ->
            case model.aktivSamtale of
                RegistrereBedriftsnavn bedriftnavnInfo ->
                    ( bedriftnavnInfo
                        |> bedriftnavnsInfoTilLokasjonInfo
                        |> RegistrereSted
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererSted string ->
            case model.aktivSamtale of
                RegistrereSted stedInfo ->
                    ( { stedInfo | lokasjon = string }
                        |> RegistrereSted
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereSted ->
            case model.aktivSamtale of
                RegistrereSted stedInfo ->
                    ( stedInfo
                        |> stedInfoTilArbeidsoppgaverInfo
                        |> RegistrereArbeidsoppgaver True
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilSeEksempel ->
            case model.aktivSamtale of
                RegistrereArbeidsoppgaver _ info ->
                    let
                        oppdatertMeldingslogg =
                            model.seksjonsMeldingsLogg
                                |> MeldingsLogg.leggTilSvar (svarFraBrukerInput model msg)
                                |> MeldingsLogg.leggTilSpørsmål eksemplerPåArbeidserfaring
                    in
                    IkkeFerdig
                        ( info
                            |> RegistrereArbeidsoppgaver False
                            |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg } IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerOppdatererArbeidsoppgaver string ->
            case model.aktivSamtale of
                RegistrereArbeidsoppgaver medEksempelKnapp arbeidsoppgaverInfo ->
                    ( { arbeidsoppgaverInfo | arbeidsoppgaver = string }
                        |> RegistrereArbeidsoppgaver medEksempelKnapp
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereArbeidsoppgaver ->
            case model.aktivSamtale of
                RegistrereArbeidsoppgaver _ arbeidsOppgaveInfo ->
                    case Validering.feilmeldingMaxAntallTegn arbeidsOppgaveInfo.arbeidsoppgaver maxLengthArbeidsoppgaver of
                        Nothing ->
                            ( arbeidsOppgaveInfo
                                |> arbeidsoppgaverInfoTilFraDatoInfo
                                |> RegistrereFraDato
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Just _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererFraMåned string ->
            case model.aktivSamtale of
                RegistrereFraDato info ->
                    ( { info | fraMåned = Måned.stringTilMåned string }
                        |> RegistrereFraDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererFraÅr string ->
            case model.aktivSamtale of
                RegistrereFraDato info ->
                    ( { info | fraÅr = string }
                        |> RegistrereFraDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereFraDato ->
            case model.aktivSamtale of
                RegistrereFraDato fraDatoInfo ->
                    case Dato.stringTilÅr fraDatoInfo.fraÅr of
                        Just fraÅr ->
                            ( fraÅr
                                |> fraDatoTilNåværende fraDatoInfo
                                |> RegistrereNåværende
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { fraDatoInfo | visFeilmeldingFraÅr = True }
                                |> RegistrereFraDato
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerSvarerJaTilNåværende ->
            case model.aktivSamtale of
                RegistrereNåværende nåværendeInfo ->
                    ( nåværendeInfo
                        |> nåværendeInfoTilSkjema
                        |> VisOppsummering FørsteGang
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerSvarerNeiTilNåværende ->
            case model.aktivSamtale of
                RegistrereNåværende info ->
                    ( info
                        |> nåværendeInfoTilTilDatoInfo
                        |> RegistrereTilDato
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererTilMåned string ->
            case model.aktivSamtale of
                RegistrereTilDato tilDatoInfo ->
                    ( { tilDatoInfo | tilMåned = Måned.stringTilMåned string }
                        |> RegistrereTilDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererTilÅr string ->
            case model.aktivSamtale of
                RegistrereTilDato tilDatoInfo ->
                    ( { tilDatoInfo | tilÅr = string }
                        |> RegistrereTilDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereTilDato ->
            case model.aktivSamtale of
                RegistrereTilDato tilDatoInfo ->
                    case Dato.stringTilÅr tilDatoInfo.tilÅr of
                        Just tilÅr ->
                            ( tilÅr
                                |> tilDatoInfoTilSkjema tilDatoInfo
                                |> VisOppsummering FørsteGang
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { tilDatoInfo | visFeilmeldingTilÅr = True }
                                |> RegistrereTilDato
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRedigereOppsummering ->
            case model.aktivSamtale of
                VisOppsummering _ skjema ->
                    updateEtterVilEndreSkjema model msg skjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                RedigerSkjema typeaheadModel skjemaType arbeidserfaringSkjema ->
                    ( arbeidserfaringSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> RedigerSkjema typeaheadModel skjemaType
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilSlettePåbegynt ->
            case model.aktivSamtale of
                VisOppsummering _ skjema ->
                    ( BekreftSlettingAvPåbegynt skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BekrefterSlettPåbegynt ->
            case model.aktivSamtale of
                BekreftSlettingAvPåbegynt _ ->
                    ( SlettetPåbegynt
                        |> SpørOmBrukerVilLeggeInnMer model.arbeidserfaringListe
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        AngrerSlettPåbegynt ->
            case model.aktivSamtale of
                BekreftSlettingAvPåbegynt skjema ->
                    ( VisOppsummering AvbrøtSletting skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilLagreArbeidserfaringIOppsummering ->
            case model.aktivSamtale of
                VisOppsummering _ skjema ->
                    updateEtterLagreKnappTrykket model msg skjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilLagreArbeidserfaringSkjema ->
            case model.aktivSamtale of
                RedigerSkjema skjemaType typeaheadModel skjema ->
                    case Skjema.valider skjema of
                        Just validertSkjema ->
                            let
                                oppsummeringstype =
                                    case skjemaType of
                                        NyArbeidserfaringsSkjema ->
                                            NyArbeidserfaring

                                        _ ->
                                            EtterEndring
                            in
                            ( validertSkjema
                                |> VisOppsummering oppsummeringstype
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar (validertSkjemaTilSetninger validertSkjema)))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> Skjema.gjørAlleFeilmeldingerSynlig
                                |> RedigerSkjema skjemaType typeaheadModel
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        ArbeidserfaringLagret result ->
            case model.aktivSamtale of
                LagrerArbeidserfaring skjema lagreStatus ->
                    case result of
                        Ok arbeidserfaringer ->
                            let
                                avsluttetGrunn =
                                    if List.length model.arbeidserfaringListe == List.length arbeidserfaringer then
                                        EndretEksisterende

                                    else
                                        AnnenAvslutning

                                nyModel =
                                    if List.length model.arbeidserfaringListe == List.length arbeidserfaringer then
                                        { model | arbeidserfaringListe = arbeidserfaringer }

                                    else
                                        { model | arbeidserfaringListe = arbeidserfaringer, harFullførtEnSamtale = True }
                            in
                            ( if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                avsluttetGrunn
                                    |> SpørOmBrukerVilLeggeInnMer arbeidserfaringer
                                    |> oppdaterSamtale nyModel (ManueltSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ]))

                              else
                                avsluttetGrunn
                                    |> SpørOmBrukerVilLeggeInnMer arbeidserfaringer
                                    |> oppdaterSamtale nyModel UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerArbeidserfaring skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , lagreArbeidserfaring ArbeidserfaringLagret skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre arbeidserfaring" error
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
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre arbeidserfaring" error
                                    ]
                                )
                                    |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilPrøveÅLagrePåNytt ->
            case model.aktivSamtale of
                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerArbeidserfaring skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagreArbeidserfaring ArbeidserfaringLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilAvbryteLagringen ->
            IkkeFerdig ( Model model, Cmd.none )

        BrukerVilAvbryteRegistreringen ->
            case model.aktivSamtale of
                RegistrerYrke _ _ ->
                    avbrytRegistrering model msg

                _ ->
                    ( model.aktivSamtale
                        |> BekreftAvbrytingAvRegistreringen
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BrukerBekrefterAvbrytingAvRegistrering ->
            avbrytRegistrering model msg

        BrukerVilIkkeAvbryteRegistreringen ->
            case model.aktivSamtale of
                BekreftAvbrytingAvRegistreringen samtaleStegFørAvbryting ->
                    ( Model
                        { model
                            | aktivSamtale = samtaleStegFørAvbryting
                            , seksjonsMeldingsLogg =
                                model.seksjonsMeldingsLogg
                                    |> MeldingsLogg.leggTilSvar (svarFraBrukerInput model msg)
                                    |> MeldingsLogg.leggTilSpørsmål
                                        (List.concat
                                            [ [ Melding.spørsmål [ "Ok. Da fortsetter vi der vi slapp." ] ]
                                            , samtaleTilMeldingsLogg samtaleStegFørAvbryting
                                            ]
                                        )
                        }
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case model.aktivSamtale of
                        LagrerArbeidserfaring skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerArbeidserfaring skjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagringFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerArbeidserfaring skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , lagreArbeidserfaring ArbeidserfaringLagret skjema
                                    )

                            else
                                IkkeFerdig ( Model model, Cmd.none )

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Hidden ->
                    IkkeFerdig ( Model model, Cmd.none )

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            SamtaleAnimasjon.update model.debugStatus samtaleAnimasjonMsg model.seksjonsMeldingsLogg
                |> updateEtterFullførtMelding model

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )

        VilLeggeTilFlereArbeidserfaringer ->
            let
                ( typeaheadModel, query ) =
                    initSamtaleTypeahead
            in
            ( Skjema.init
                |> RedigerSkjema NyArbeidserfaringsSkjema typeaheadModel
                |> oppdaterSamtale model (SvarFraMsg msg)
            , Cmd.batch
                [ lagtTilSpørsmålCmd model.debugStatus
                , Api.getYrkeTypeahead HentetYrkeTypeahead query
                ]
            )
                |> IkkeFerdig

        FerdigMedArbeidserfaring ->
            let
                sisteMelding =
                    if List.isEmpty model.arbeidserfaringListe then
                        "Ok, da går vi videre."

                    else
                        "Bra innsats! 😊 Nå kan arbeidsgivere finne deg hvis de ser etter en med din erfaring."
            in
            ( sisteMelding
                |> VenterPåAnimasjonFørFullføring
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        FokusSatt _ ->
            case model.aktivSamtale of
                RedigerSkjema skjemaType typeaheadModel skjema ->
                    IkkeFerdig
                        ( RedigerSkjema skjemaType (Typeahead.hideSuggestions typeaheadModel) skjema
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )


avbrytRegistrering : ModelInfo -> Msg -> SamtaleStatus
avbrytRegistrering model msg =
    ( AvbruttPåbegynt
        |> SpørOmBrukerVilLeggeInnMer model.arbeidserfaringListe
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateSamtaleTypeahead : ModelInfo -> { visFeilmelding : Bool } -> Typeahead.Msg Yrke -> Typeahead.Model Yrke -> SamtaleStatus
updateSamtaleTypeahead model visFeilmelding msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Yrke.label msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected typeaheadModel of
                Just yrke ->
                    brukerVelgerYrke model (TypeaheadMsg msg) yrke

                Nothing ->
                    visFeilmeldingRegistrerYrke model nyTypeaheadModel

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerYrke visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerYrke visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getYrkeTypeahead HentetYrkeTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerYrke visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView Yrke.label Nothing
                    |> Cmd.map TypeaheadMsg
                )


initSamtaleTypeahead : ( Typeahead.Model Yrke, Typeahead.Query )
initSamtaleTypeahead =
    Typeahead.init
        { value = ""
        , label = "Hvilken stilling/yrke har du?"
        , id = inputIdTilString YrkeTypeaheadId
        , toString = Yrke.label
        }


initSkjemaTypeaheadFraArbeidserfaring : Arbeidserfaring -> ( Typeahead.Model Yrke, Typeahead.Query )
initSkjemaTypeaheadFraArbeidserfaring arbeidserfaring =
    arbeidserfaring
        |> Arbeidserfaring.yrke
        |> Maybe.map initSkjemaTypeaheadFraYrke
        |> Maybe.withDefault
            (Typeahead.init
                { value = ""
                , label = "Stilling/yrke"
                , id = inputIdTilString YrkeTypeaheadId
                , toString = Yrke.label
                }
            )


initSkjemaTypeaheadFraYrke : Yrke -> ( Typeahead.Model Yrke, Typeahead.Query )
initSkjemaTypeaheadFraYrke yrke =
    Typeahead.initWithSelected
        { selected = yrke
        , label = "Stilling/yrke"
        , id = inputIdTilString YrkeTypeaheadId
        , toString = Yrke.label
        }


oppdaterSkjema : SkjemaEndring -> ArbeidserfaringSkjema -> ArbeidserfaringSkjema
oppdaterSkjema endring skjema =
    case endring of
        Tekst felt string ->
            Skjema.oppdaterStringFelt felt string skjema

        NåværendeToggled ->
            Skjema.toggleNåværende skjema

        FraMåned månedString ->
            månedString
                |> Måned.stringTilMåned
                |> Skjema.oppdaterFraMåned skjema

        TilMåned månedString ->
            månedString
                |> Måned.stringTilMåned
                |> Skjema.oppdaterTilMåned skjema

        FraÅrBlurred ->
            Skjema.gjørFeilmeldingFraÅrSynlig skjema

        TilÅrBlurred ->
            Skjema.gjørFeilmeldingTilÅrSynlig skjema


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding info ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        MeldingsLogg.FerdigAnimert ferdigAnimertSamtale ->
            case info.aktivSamtale of
                VenterPåAnimasjonFørFullføring _ ->
                    Ferdig (sistLagret (Model info)) ferdigAnimertSamtale

                _ ->
                    ( Model { info | seksjonsMeldingsLogg = nyMeldingsLogg }
                    , Cmd.batch
                        [ Cmd.map SamtaleAnimasjonMsg cmd
                        , settFokus info.aktivSamtale
                        ]
                    )
                        |> IkkeFerdig

        MeldingsLogg.MeldingerGjenstår ->
            ( Model { info | seksjonsMeldingsLogg = nyMeldingsLogg }
            , Cmd.map SamtaleAnimasjonMsg cmd
            )
                |> IkkeFerdig


updateEtterVilEndreSkjema : ModelInfo -> Msg -> ValidertArbeidserfaringSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model msg skjema =
    let
        ( typeaheadModel, query ) =
            initSkjemaTypeaheadFraYrke (Skjema.yrke skjema)
    in
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> RedigerSkjema OppsummeringsSkjema typeaheadModel
        |> oppdaterSamtale model (SvarFraMsg msg)
    , Cmd.batch
        [ lagtTilSpørsmålCmd model.debugStatus
        , Api.getYrkeTypeahead HentetYrkeTypeahead query
        ]
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> Msg -> ValidertArbeidserfaringSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model msg skjema =
    ( LagreStatus.init
        |> LagrerArbeidserfaring skjema
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagreArbeidserfaring ArbeidserfaringLagret skjema
    )
        |> IkkeFerdig


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        Intro ->
            settFokusCmd HarArbeidserfaringId

        RegistrerYrke _ _ ->
            settFokusCmd YrkeTypeaheadId

        SpørOmBrukerVilEndreJobbtittel _ ->
            settFokusCmd EndreJobbtittelId

        EndreJobbtittel _ ->
            settFokusCmd JobbtittelInput

        RegistrereBedriftsnavn _ ->
            settFokusCmd BedriftsnavnInput

        RegistrereSted _ ->
            settFokusCmd StedInput

        RegistrereArbeidsoppgaver _ _ ->
            settFokusCmd ArbeidsoppgaverInput

        RegistrereFraDato _ ->
            settFokusCmd FraMånedId

        RegistrereNåværende _ ->
            settFokusCmd NåværendeId

        RegistrereTilDato _ ->
            settFokusCmd TilMånedId

        RedigerSkjema _ _ _ ->
            settFokusCmd YrkeTypeaheadId

        VisOppsummering _ _ ->
            settFokusCmd BekreftOppsummeringId

        SpørOmBrukerVilLeggeInnMer _ _ ->
            settFokusCmd LeggTilArbeidserfaringId

        VelgEnArbeidserfaringÅRedigere ->
            settFokusCmd RedigerArbeidserfaringId

        LagringFeilet _ _ ->
            settFokusCmd LagringFeiletActionId

        BekreftSlettingAvPåbegynt _ ->
            settFokusCmd SlettePåbegyntId

        BekreftAvbrytingAvRegistreringen _ ->
            settFokusCmd AvbrytSlettingId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
        |> Task.attempt FokusSatt


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


mistetFokusCmd : Cmd Msg
mistetFokusCmd =
    Process.sleep 100
        |> Task.perform (\_ -> TimeoutEtterAtFeltMistetFokus)


brukerVelgerYrke : ModelInfo -> Msg -> Yrke -> SamtaleStatus
brukerVelgerYrke info msg yrkesTypeahead =
    ( yrkesTypeahead
        |> yrkeInfoTilJobbtittelInfo
        |> SpørOmBrukerVilEndreJobbtittel
        |> oppdaterSamtale info (SvarFraMsg msg)
    , lagtTilSpørsmålCmd info.debugStatus
    )
        |> IkkeFerdig


feilmeldingTypeahead : Typeahead.Model Yrke -> Maybe String
feilmeldingTypeahead typeaheadModel =
    case Typeahead.selected typeaheadModel of
        Just _ ->
            Nothing

        Nothing ->
            Just "Velg et yrke fra listen med forslag som kommer opp"


visFeilmeldingRegistrerYrke : ModelInfo -> Typeahead.Model Yrke -> SamtaleStatus
visFeilmeldingRegistrerYrke model typeaheadModel =
    ( typeaheadModel
        |> RegistrerYrke { visFeilmelding = True }
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig


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


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg personaliaSeksjon =
    case personaliaSeksjon of
        Intro ->
            [ Melding.spørsmål [ "Nå skal vi registrere arbeidserfaringen din" ] ]

        VelgEnArbeidserfaringÅRedigere ->
            [ Melding.spørsmål [ "Hvilken arbeidserfaring ønsker du å endre?" ] ]

        RegistrerYrke _ _ ->
            [ Melding.spørsmål [ "Nå skal du legge inn arbeidserfaring. La oss begynne med det siste arbeidsforholdet." ]
            , Melding.spørsmål [ "Først må du velge et yrke. Begynn å skriv, velg fra listen med forslag som kommer opp." ]
            , Melding.spørsmål [ "Du må velge et av forslagene, da kan arbeidsgivere finne deg når de søker etter folk. Du kan endre hva som vises på CV-en senere." ]
            ]

        SpørOmBrukerVilEndreJobbtittel info ->
            [ Melding.spørsmål [ "Du valgte «" ++ Yrke.label info.tidligereInfo ++ "». Hvis dette ikke stemmer helt, kan du gi yrket et nytt navn. Det navnet vil vises på CV-en din. Vil du gi det et nytt navn?" ]
            ]

        EndreJobbtittel _ ->
            [ Melding.spørsmål [ "Ok, da kan du skrive inn et nytt navn." ] ]

        RegistrereBedriftsnavn _ ->
            [ Melding.spørsmål [ "Hvilken bedrift jobber eller jobbet du i?" ] ]

        RegistrereSted _ ->
            [ Melding.spørsmål [ "Hvor holder bedriften til?" ] ]

        RegistrereArbeidsoppgaver _ _ ->
            [ Melding.spørsmål [ "Fortell hvilke arbeidsoppgaver du har hatt og hva som var rollen din." ] ]

        RegistrereFraDato _ ->
            [ Melding.spørsmål [ "Når begynte du i jobben?" ] ]

        RegistrereNåværende nåværendeInfo ->
            let
                yrkestittel =
                    case nåværendeInfo.jobbtittel of
                        "" ->
                            Yrke.label nåværendeInfo.yrke

                        jobbtittel ->
                            jobbtittel
            in
            [ Melding.spørsmål [ "Jobber du fremdeles som «" ++ yrkestittel ++ "» i " ++ nåværendeInfo.bedriftNavn ++ "?" ] ]

        RegistrereTilDato _ ->
            [ Melding.spørsmål [ "Når sluttet du i jobben?" ] ]

        VisOppsummering oppsummeringsType validertSkjema ->
            case oppsummeringsType of
                AvbrøtSletting ->
                    [ Melding.spørsmål [ "Ok, da lar jeg arbeidserfaringen stå." ]
                    , Melding.spørsmål
                        (validertSkjemaTilSetninger validertSkjema
                            ++ [ Melding.tomLinje
                               , "Er informasjonen riktig?"
                               ]
                        )
                    ]

                EtterEndring ->
                    [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

                FørsteGang ->
                    [ Melding.spørsmål
                        (validertSkjemaTilSetninger validertSkjema
                            ++ [ Melding.tomLinje
                               , "Er informasjonen riktig?"
                               ]
                        )
                    ]

                NyArbeidserfaring ->
                    [ Melding.spørsmål [ "Du har lagt til en arbeidserfaring. Er informasjonen riktig?" ] ]

        RedigerSkjema skjemaType _ _ ->
            case skjemaType of
                OppsummeringsSkjema ->
                    [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

                _ ->
                    [ Melding.spørsmål [ "Legg inn arbeidserfaringen din under." ] ]

        BekreftSlettingAvPåbegynt _ ->
            [ Melding.spørsmål [ "Er du sikker på at du vil slette denne arbeidserfaringen?" ] ]

        LagrerArbeidserfaring _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre arbeidserfaringen" } ]

        SpørOmBrukerVilLeggeInnMer arbeidserfaringer avsluttetGrunn ->
            [ case avsluttetGrunn of
                AvbruttPåbegynt ->
                    Melding.spørsmål [ "Nå har jeg avbrutt." ]

                SlettetPåbegynt ->
                    Melding.spørsmål [ "Nå har jeg slettet arbeidserfaringen." ]

                EndretEksisterende ->
                    Melding.spørsmål [ "Flott! Nå er arbeidserfaringen endret." ]

                AnnenAvslutning ->
                    Melding.spørsmål [ "Flott! Nå er arbeidserfaringen lagret." ]
            , if List.isEmpty arbeidserfaringer then
                Melding.spørsmål [ "Har du arbeidserfaring du vil legge inn i CV-en?" ]

              else
                Melding.spørsmål [ "Vil du legge inn flere arbeidserfaringer?" ]
            ]

        BekreftAvbrytingAvRegistreringen _ ->
            [ Melding.spørsmål [ "Hvis du avbryter, blir ikke denne arbeidserfaringen lagret på CV-en din. Er du sikker på at du vil avbryte?" ] ]

        VenterPåAnimasjonFørFullføring string ->
            [ Melding.spørsmål [ string ] ]


validertSkjemaTilSetninger : ValidertArbeidserfaringSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    let
        skjema =
            Skjema.tilUvalidertSkjema validertSkjema
    in
    [ "Du har lagt inn dette:"
    , Melding.tomLinje
    , datoRad validertSkjema
    , Melding.tomLinje
    , "Stilling/Yrke: " ++ hentStilling validertSkjema
    , "Bedriftnavn: " ++ Skjema.innholdTekstFelt Bedriftsnavn skjema
    , "Sted: " ++ Skjema.innholdTekstFelt Sted skjema
    , Melding.tomLinje
    , "Arbeidsoppgaver: "
    , Skjema.innholdTekstFelt Arbeidsoppgaver skjema
    ]


hentStilling : ValidertArbeidserfaringSkjema -> String
hentStilling validertSkjema =
    let
        skjema =
            Skjema.tilUvalidertSkjema validertSkjema
    in
    if Skjema.innholdTekstFelt Jobbtittel skjema == "" then
        validertSkjema
            |> Skjema.yrke
            |> Yrke.label

    else
        Skjema.innholdTekstFelt Jobbtittel skjema


datoRad : ValidertArbeidserfaringSkjema -> String
datoRad skjema =
    Dato.periodeTilString
        ((Skjema.tilUvalidertSkjema >> Skjema.fraMåned) skjema)
        (Skjema.fraÅrValidert skjema)
        (Skjema.tilDatoValidert skjema)


eksemplerPåArbeidserfaring : List Melding
eksemplerPåArbeidserfaring =
    [ Melding.eksempelMedTittel "Eksempel 1:" [ "Lærling som elektriker hos Helgeland Elektro, som er spesialist på rehabilitering av elektriske anlegg. Vi er 5 ansatte og tar i hovedsak oppdrag for privatpersoner." ]
    , Melding.eksempelMedTittel "Eksempel 2:" [ "Ekstrahjelp som butikkmedarbeider i sommer- og juleferien. Kassearbeid, påfylling av varer, salg og kundeservice." ]
    , Melding.eksempelMedTittel "Eksempel 3:" [ "Jobbet 3 år som barnehageassistent i en barnehage med 24 barn. Hadde medansvar for pedagogisk arbeid på avdelingen. Bidro ved månedsplanlegging og foreldresamtaler." ]
    , Melding.eksempelMedTittel "Eksempel 4:"
        [ "Advokatsekretær med hovedansvar for resepsjonen i et advokatfirma med 45 ansatte."
        , Melding.tomLinje
        , "- Saksbehandling og kontoradministrative oppgaver"
        , "- Betjening av sentralbord"
        , "- Post- og dokumenthåndtering"
        , "- Bilagsregistrering"
        , "- Klientkontakt"
        , "- Møte- og kursbooking"
        ]
    ]



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput model =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            Intro ->
                if List.isEmpty model.arbeidserfaringListe then
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp BrukerVilLeggeTilNyArbeidserfaring "Ja, jeg har arbeidserfaring"
                            |> Knapp.withId (inputIdTilString HarArbeidserfaringId)
                        , Knapp.knapp FerdigMedArbeidserfaring "Nei, jeg har ikke arbeidserfaring"
                        ]

                else
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp BrukerVilLeggeTilNyArbeidserfaring "Ja, jeg vil legge til mer"
                            |> Knapp.withId (inputIdTilString HarArbeidserfaringId)
                        , Knapp.knapp BrukerHopperOverArbeidserfaring "Nei, jeg er ferdig"
                        , Knapp.knapp BrukerVilRedigereArbeidserfaring "Nei, jeg vil endre det jeg har lagt inn"
                        ]

            VelgEnArbeidserfaringÅRedigere ->
                BrukerInput.knapper Kolonne
                    (case model.arbeidserfaringListe of
                        first :: rest ->
                            (arbeidserfaringKnapp first
                                |> Knapp.withId (inputIdTilString RedigerArbeidserfaringId)
                            )
                                :: List.map arbeidserfaringKnapp rest

                        _ ->
                            []
                    )

            RegistrerYrke visFeilmelding typeaheadModel ->
                viewRegistrerYrke visFeilmelding typeaheadModel

            SpørOmBrukerVilEndreJobbtittel _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerVilEndreJobbtittel "Ja, jeg vil gi det et nytt navn"
                        |> Knapp.withId (inputIdTilString EndreJobbtittelId)
                    , Knapp.knapp BrukerVilIkkeEndreJobbtittel "Nei, gå videre"
                    ]

            EndreJobbtittel jobbtittelInfo ->
                BrukerInput.inputMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilRegistrereJobbtittel }
                    (jobbtittelInfo.jobbtittel
                        |> Input.input { label = "Stilling/yrke som vil vises i CV-en", msg = BrukerOppdatererJobbtittelFelt }
                        |> Input.withOnEnter BrukerVilRegistrereJobbtittel
                        |> Input.withId (inputIdTilString JobbtittelInput)
                    )

            RegistrereBedriftsnavn bedriftnanvsInfo ->
                BrukerInput.inputMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilRegistrereBedriftsnavn }
                    (bedriftnanvsInfo.bedriftNavn
                        |> Input.input { label = "Bedriftens navn", msg = BrukerOppdatererBedriftsnavn }
                        |> Input.withOnEnter BrukerVilRegistrereBedriftsnavn
                        |> Input.withId (inputIdTilString BedriftsnavnInput)
                    )

            RegistrereSted stedInfo ->
                BrukerInput.inputMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilRegistrereSted }
                    (stedInfo.lokasjon
                        |> Input.input { label = "By, sted eller land", msg = BrukerOppdatererSted }
                        |> Input.withOnEnter BrukerVilRegistrereSted
                        |> Input.withId (inputIdTilString StedInput)
                    )

            RegistrereArbeidsoppgaver medEksempelKnapp arbeidsoppgaverInfo ->
                arbeidsoppgaverInfo.arbeidsoppgaver
                    |> Textarea.textarea { label = "Arbeidsoppgaver", msg = BrukerOppdatererArbeidsoppgaver }
                    |> Textarea.withId (inputIdTilString ArbeidsoppgaverInput)
                    |> Textarea.withFeilmelding (Validering.feilmeldingMaxAntallTegn arbeidsoppgaverInfo.arbeidsoppgaver maxLengthArbeidsoppgaver)
                    |> BrukerInputMedGåVidereKnapp.textarea BrukerVilRegistrereArbeidsoppgaver
                    |> BrukerInputMedGåVidereKnapp.withVisEksempelKnapp medEksempelKnapp VilSeEksempel
                    |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp BrukerVilAvbryteRegistreringen
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            RegistrereFraDato info ->
                DatoInput.datoInput
                    { onMånedChange = BrukerOppdatererFraMåned
                    , måned = info.fraMåned
                    , onÅrChange = BrukerOppdatererFraÅr
                    , år = info.fraÅr
                    }
                    |> DatoInput.withFeilmeldingÅr
                        (info.fraÅr
                            |> Dato.feilmeldingÅr
                            |> maybeHvisTrue info.visFeilmeldingFraÅr
                        )
                    |> DatoInput.withFokusId (inputIdTilString FraMånedId)
                    |> DatoInput.withOnBlurÅr FeltMisterFokus
                    |> BrukerInputMedGåVidereKnapp.datoMånedÅr BrukerVilRegistrereFraDato
                    |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp BrukerVilAvbryteRegistreringen
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            RegistrereNåværende _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerSvarerJaTilNåværende "Ja"
                        |> Knapp.withId (inputIdTilString NåværendeId)
                    , Knapp.knapp BrukerSvarerNeiTilNåværende "Nei"
                    ]

            RegistrereTilDato info ->
                DatoInput.datoInput
                    { onMånedChange = BrukerOppdatererTilMåned
                    , måned = info.tilMåned
                    , onÅrChange = BrukerOppdatererTilÅr
                    , år = info.tilÅr
                    }
                    |> DatoInput.withFeilmeldingÅr
                        (info.tilÅr
                            |> Dato.feilmeldingÅr
                            |> maybeHvisTrue info.visFeilmeldingTilÅr
                        )
                    |> DatoInput.withFokusId (inputIdTilString TilMånedId)
                    |> DatoInput.withOnBlurÅr FeltMisterFokus
                    |> BrukerInputMedGåVidereKnapp.datoMånedÅr BrukerVilRegistrereTilDato
                    |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp BrukerVilAvbryteRegistreringen
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            VisOppsummering _ skjema ->
                case Skjema.id skjema of
                    Just _ ->
                        viewBekreftOppsummering False

                    Nothing ->
                        viewBekreftOppsummering True

            RedigerSkjema skjemaType typeaheadModel skjema ->
                case skjemaType of
                    OppsummeringsSkjema ->
                        skjemaInnhold skjema typeaheadModel
                            |> BrukerInput.skjema { lagreMsg = BrukerVilLagreArbeidserfaringSkjema, lagreKnappTekst = "Lagre endringer" }

                    NyArbeidserfaringsSkjema ->
                        skjemaInnhold skjema typeaheadModel
                            |> BrukerInput.skjemaMedAvbryt
                                { lagreMsg = BrukerVilLagreArbeidserfaringSkjema
                                , lagreKnappTekst = "Lagre arbeidserfaring"
                                , onAvbryt = Just BrukerVilAvbryteRegistreringen
                                }

            BekreftSlettingAvPåbegynt _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BekrefterSlettPåbegynt "Ja, jeg vil slette"
                        |> Knapp.withId (inputIdTilString SlettePåbegyntId)
                    , Knapp.knapp AngrerSlettPåbegynt "Nei, jeg vil ikke slette"
                    ]

            LagrerArbeidserfaring _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    BrukerInput.utenInnhold

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilAvbryteLagringen "Gå videre"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilPrøveÅLagrePåNytt "Prøv igjen"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            , Knapp.knapp BrukerVilAvbryteLagringen "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            SpørOmBrukerVilLeggeInnMer arbeidserfaringer _ ->
                let
                    leggTilMsg =
                        if model.harFullførtEnSamtale then
                            VilLeggeTilFlereArbeidserfaringer

                        else
                            BrukerVilLeggeTilNyArbeidserfaring
                in
                BrukerInput.knapper Flytende
                    ([ [ Knapp.knapp leggTilMsg "Ja, legg til en arbeidserfaring"
                            |> Knapp.withId (inputIdTilString LeggTilArbeidserfaringId)
                       , Knapp.knapp FerdigMedArbeidserfaring "Nei, jeg har lagt inn alle"
                       ]
                     , if List.length arbeidserfaringer > 0 then
                        [ Knapp.knapp BrukerVilRedigereArbeidserfaring "Nei, jeg vil endre det jeg har lagt inn" ]

                       else
                        []
                     ]
                        |> List.concat
                    )

            VenterPåAnimasjonFørFullføring _ ->
                BrukerInput.utenInnhold

            BekreftAvbrytingAvRegistreringen _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerBekrefterAvbrytingAvRegistrering "Ja, jeg vil avbryte"
                        |> Knapp.withId (inputIdTilString AvbrytSlettingId)
                    , Knapp.knapp BrukerVilIkkeAvbryteRegistreringen "Nei, jeg vil fortsette"
                    ]

    else
        BrukerInput.utenInnhold


skjemaInnhold : ArbeidserfaringSkjema -> Typeahead.Model Yrke -> List (Html Msg)
skjemaInnhold skjema typeaheadModel =
    [ skjema
        |> Skjema.feilmeldingYrke
        |> Typeahead.view Yrke.label typeaheadModel
        |> Html.map TypeaheadMsg
    , if Skjema.innholdTekstFelt Jobbtittel skjema == "" then
        text ""

      else
        skjema
            |> Skjema.innholdTekstFelt Jobbtittel
            |> Input.input { label = "Jobbtittel", msg = Tekst Jobbtittel >> SkjemaEndret }
            |> Input.toHtml
    , skjema
        |> Skjema.innholdTekstFelt Bedriftsnavn
        |> Input.input { label = "Bedriftens navn", msg = Tekst Bedriftsnavn >> SkjemaEndret }
        |> Input.toHtml
    , skjema
        |> Skjema.innholdTekstFelt Sted
        |> Input.input { label = "By, sted eller land", msg = Tekst Sted >> SkjemaEndret }
        |> Input.toHtml
    , skjema
        |> Skjema.innholdTekstFelt Arbeidsoppgaver
        |> Textarea.textarea { label = "Arbeidsoppgaver", msg = Tekst Arbeidsoppgaver >> SkjemaEndret }
        |> Textarea.withFeilmelding (Validering.feilmeldingMaxAntallTegn (Skjema.innholdTekstFelt Arbeidsoppgaver skjema) maxLengthArbeidsoppgaver)
        |> Textarea.toHtml
    , div [ class "DatoInput-fra-til-rad" ]
        [ DatoInput.datoInput
            { onMånedChange = FraMåned >> SkjemaEndret
            , måned = Skjema.fraMåned skjema
            , onÅrChange = Tekst FraÅr >> SkjemaEndret
            , år = Skjema.innholdTekstFelt FraÅr skjema
            }
            |> DatoInput.withLabel "Når begynte du i jobbben?"
            |> DatoInput.withFeilmeldingÅr (Skjema.feilmeldingFraÅr skjema)
            |> DatoInput.withOnBlurÅr (SkjemaEndret FraÅrBlurred)
            |> DatoInput.toHtml
        , if not (Skjema.nåværende skjema) then
            DatoInput.datoInput
                { onMånedChange = TilMåned >> SkjemaEndret
                , måned = Skjema.tilMåned skjema
                , onÅrChange = Tekst TilÅr >> SkjemaEndret
                , år = Skjema.innholdTekstFelt TilÅr skjema
                }
                |> DatoInput.withLabel "Når sluttet du i jobbben?"
                |> DatoInput.withFeilmeldingÅr (Skjema.feilmeldingTilÅr skjema)
                |> DatoInput.withOnBlurÅr (SkjemaEndret TilÅrBlurred)
                |> DatoInput.toHtml

          else
            text ""
        ]
    , skjema
        |> Skjema.nåværende
        |> Checkbox.checkbox "Jeg jobber fremdeles her" (SkjemaEndret NåværendeToggled)
        |> Checkbox.withClass "blokk-m"
        |> Checkbox.toHtml
    ]


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


type InputId
    = HarArbeidserfaringId
    | RedigerArbeidserfaringId
    | YrkeTypeaheadId
    | EndreJobbtittelId
    | JobbtittelInput
    | BedriftsnavnInput
    | StedInput
    | ArbeidsoppgaverInput
    | FraMånedId
    | TilMånedId
    | NåværendeId
    | BekreftOppsummeringId
    | LeggTilArbeidserfaringId
    | SlettePåbegyntId
    | LagringFeiletActionId
    | AvbrytSlettingId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        HarArbeidserfaringId ->
            "har-arbeidserfaring-id"

        RedigerArbeidserfaringId ->
            "arbeidserfaring-rediger-id"

        YrkeTypeaheadId ->
            "arbeidserfaring-registrer-yrke-typeahead"

        JobbtittelInput ->
            "arbeidserfaring-registrer-jobbtittel"

        EndreJobbtittelId ->
            "arbeidserfaring-endre-jobbtittel"

        BedriftsnavnInput ->
            "arbeidserfaring-registrer-bedriftsnavn"

        StedInput ->
            "arbeidserfaring-registrer-sted"

        ArbeidsoppgaverInput ->
            "arbeidserfaring-registrer-arbeidsoppgaver"

        FraMånedId ->
            "arbeidserfaring-fra-måned-id"

        TilMånedId ->
            "arbeidserfaring-til-måned-id"

        NåværendeId ->
            "arbeidserfaring-nåværende-id"

        BekreftOppsummeringId ->
            "arbeidserfaring-bekreft-oppsummering-id"

        LeggTilArbeidserfaringId ->
            "arbeidserfaring-legg-til-id"

        SlettePåbegyntId ->
            "arbeidserfaring-slett-påbegynt-id"

        LagringFeiletActionId ->
            "arbeidserfaring-lagring-feilet-id"

        AvbrytSlettingId ->
            "arbeidserfaring-avbrytt-slett-id"


viewRegistrerYrke : { visFeilmelding : Bool } -> Typeahead.Model Yrke -> BrukerInput Msg
viewRegistrerYrke { visFeilmelding } typeaheadModel =
    BrukerInput.typeaheadMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilRegistrereYrke }
        (typeaheadModel
            |> feilmeldingTypeahead
            |> maybeHvisTrue visFeilmelding
            |> Typeahead.toViewElement Yrke.label typeaheadModel
            |> FrontendModuler.Typeahead.map TypeaheadMsg
        )


viewBekreftOppsummering : Bool -> BrukerInput Msg
viewBekreftOppsummering skalViseSlett =
    if skalViseSlett then
        BrukerInput.knapper Kolonne
            [ Knapp.knapp BrukerVilLagreArbeidserfaringIOppsummering "Ja, det er riktig"
                |> Knapp.withId (inputIdTilString BekreftOppsummeringId)
            , Knapp.knapp BrukerVilRedigereOppsummering "Nei, jeg vil endre"
            , Knapp.knapp VilSlettePåbegynt "Nei, jeg vil slette"
            ]

    else
        BrukerInput.knapper Flytende
            [ Knapp.knapp BrukerVilLagreArbeidserfaringIOppsummering "Ja, det er riktig"
                |> Knapp.withId (inputIdTilString BekreftOppsummeringId)
            , Knapp.knapp BrukerVilRedigereOppsummering "Nei, jeg vil endre"
            ]


arbeidserfaringKnapp : Arbeidserfaring -> Knapp Msg
arbeidserfaringKnapp arbeidserfaring =
    let
        tekst =
            Maybe.withDefault "" (Arbeidserfaring.yrkeString arbeidserfaring)
                ++ ", "
                ++ Maybe.withDefault "" (Arbeidserfaring.arbeidsgiver arbeidserfaring)
    in
    Knapp.knapp (BrukerHarValgtArbeidserfaringÅRedigere arbeidserfaring) tekst


lagreArbeidserfaring : (Result Error (List Arbeidserfaring) -> msg) -> Skjema.ValidertArbeidserfaringSkjema -> Cmd msg
lagreArbeidserfaring msgConstructor skjema =
    case Skjema.id skjema of
        Just id ->
            Api.endreArbeidserfaring msgConstructor skjema id

        Nothing ->
            Api.opprettArbeidserfaring msgConstructor skjema


arbeidserfaringerTilTekstområder : List Arbeidserfaring -> List Tekstområde
arbeidserfaringerTilTekstområder arbeidserfaringer =
    arbeidserfaringer
        |> List.map arbeidserfaringTilTekstområde
        |> List.intersperse (Avsnitt Melding.tomLinje)


arbeidserfaringTilTekstområde : Arbeidserfaring -> Tekstområde
arbeidserfaringTilTekstområde arbeidserfaring =
    Seksjon (beskrivArbeidserfaring arbeidserfaring)
        [ Dato.periodeTilString (Arbeidserfaring.fraMåned arbeidserfaring) (Arbeidserfaring.fraÅr arbeidserfaring) (Arbeidserfaring.tilDato arbeidserfaring)
        , beskrivArbeidserfaring arbeidserfaring
        ]


beskrivArbeidserfaring : Arbeidserfaring -> String
beskrivArbeidserfaring arbeidserfaring =
    let
        maybeYrkeTekst : Maybe String
        maybeYrkeTekst =
            Maybe.or
                (Arbeidserfaring.yrkeFritekst arbeidserfaring)
                (Arbeidserfaring.yrkeString arbeidserfaring)
    in
    [ maybeYrkeTekst
    , Arbeidserfaring.arbeidsgiver arbeidserfaring
    ]
        |> Maybe.values
        |> List.intersperse "hos"
        |> String.join " "


logFeilmelding : String -> Http.Error -> Cmd Msg
logFeilmelding operasjon error =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError (always ErrorLogget))
        |> Maybe.withDefault Cmd.none


init : DebugStatus -> Posix -> FerdigAnimertMeldingsLogg -> List Arbeidserfaring -> ( Model, Cmd Msg )
init debugStatus sistLagretFraForrigeSeksjon gammelMeldingsLogg arbeidserfaringsListe =
    ( Model
        { seksjonsMeldingsLogg =
            gammelMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> (if List.isEmpty arbeidserfaringsListe then
                        MeldingsLogg.leggTilSpørsmål
                            [ Melding.spørsmål
                                [ "Har du arbeidserfaring du vil legge inn?"
                                ]
                            ]

                    else
                        MeldingsLogg.leggTilSpørsmål
                            [ Melding.spørsmål [ "Nå skal vi legge til arbeidserfaringen din." ]
                            , Melding.spørsmål [ "Jeg ser at du har lagt til noe allerede." ]
                            , Melding.spørsmålMedTekstområder (arbeidserfaringerTilTekstområder arbeidserfaringsListe)
                            , Melding.spørsmål [ "Vil du legge til mer?" ]
                            ]
                   )
        , arbeidserfaringListe = arbeidserfaringsListe
        , aktivSamtale = Intro
        , debugStatus = debugStatus
        , sistLagretFraForrigeSeksjon = sistLagretFraForrigeSeksjon
        , harFullførtEnSamtale = False
        }
    , lagtTilSpørsmålCmd debugStatus
    )


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Browser.Events.onVisibilityChange WindowEndrerVisibility
        , model.seksjonsMeldingsLogg
            |> SamtaleAnimasjon.subscriptions
            |> Sub.map SamtaleAnimasjonMsg
        ]
