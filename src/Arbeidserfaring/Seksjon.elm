module Arbeidserfaring.Seksjon exposing
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
import Arbeidserfaring.Skjema as Skjema exposing (ArbeidserfaringSkjema, Felt(..), ValidertArbeidserfaringSkjema)
import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import Cv.Arbeidserfaring exposing (Arbeidserfaring)
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
import Task
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
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig FerdigAnimertMeldingsLogg


type RegistreringsType
    = RegistrerFørsteGang
    | HarRegistrertFør


type AvsluttetGrunn
    = AvbruttPåbegynt
    | SlettetPåbegynt
    | AnnenAvslutning
    | EndretEksisterende


type OppsummeringsType
    = FørsteGang
    | EtterEndring
    | AvbrøtSletting


type Samtale
    = Intro
    | VelgEnArbeidserfaringÅRedigere
    | RegistrerYrke RegistreringsType { visFeilmelding : Bool } (Typeahead.Model Yrke)
    | HentingFraTypeaheadFeilet (Typeahead.Model Yrke) Http.Error
    | HenterFraTypeaheadPåNyttEtterFeiling (Typeahead.Model Yrke) Http.Error
    | SpørOmBrukerVilEndreJobbtittel JobbtittelInfo
    | EndreJobbtittel JobbtittelInfo
    | RegistrereBedriftsnavn BedriftnavnInfo
    | RegistrereSted StedInfo
    | RegistrereArbeidsoppgaver Bool ArbeidsoppgaverInfo
    | RegistrereFraMåned FraDatoInfo
    | RegistrereFraÅr FraDatoInfo
    | RegistrereNåværende NåværendeInfo
    | RegistrereTilMåned TilDatoInfo
    | RegistrereTilÅr TilDatoInfo
    | VisOppsummering OppsummeringsType ValidertArbeidserfaringSkjema
    | RedigerOppsummering (Typeahead.Model Yrke) ArbeidserfaringSkjema
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
    | HentetYrkeTypeahead (Result Http.Error (List Yrke))
    | BrukerVilRegistrereYrke
    | BrukerVilAvbryteHentingFraTypeahead
    | BrukerVilPrøveÅHenteFraTypeaheadPåNytt
    | BrukerVilEndreJobbtittel JobbtittelInfo
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
    | BrukerTrykketFraMånedKnapp Måned
    | BrukerOppdatererFraÅr String
    | FraÅrMisterFokus
    | BrukerVilRegistrereFraÅr
    | BrukerSvarerJaTilNåværende
    | BrukerSvarerNeiTilNåværende
    | BrukerTrykketTilMånedKnapp Måned
    | BrukerOppdatererTilÅr String
    | TilÅrMisterFokus
    | BrukerVilRegistrereTilÅr
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
    | NyArbeidserfaring
    | FerdigMedArbeidserfaring
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())
    | GåTilNesteSeksjon
    | ErrorLogget


type SkjemaEndring
    = Tekst Felt String
    | NåværendeToggled
    | FraMåned String
    | TilMåned String
    | FraÅrBlurred
    | TilÅrBlurred


type alias RegistrerYrkeInfo =
    { valgtYrke : Maybe Yrke
    , feilmelding : Maybe String
    }


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
    { tidligereInfo : ArbeidsoppgaverInfo
    , fraMåned : Måned
    , fraÅr : År
    }


type alias TilDatoInfo =
    { tidligereInfo : NåværendeInfo
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


arbeidsoppgaverInfoTilfraDatoInfo : ArbeidsoppgaverInfo -> FraDatoInfo
arbeidsoppgaverInfoTilfraDatoInfo arbeidsoppgaverInfo =
    { tidligereInfo = arbeidsoppgaverInfo
    , fraMåned = Januar
    , fraÅr = ""
    , visFeilmeldingFraÅr = False
    }


fraDatoInfoTilTilDatoInfo : NåværendeInfo -> TilDatoInfo
fraDatoInfoTilTilDatoInfo nåværendeInfo =
    { tidligereInfo = nåværendeInfo
    , tilMåned = Januar
    , tilÅr = ""
    , visFeilmeldingTilÅr = False
    }


nåværendeInfoTilSkjema : NåværendeInfo -> ValidertArbeidserfaringSkjema
nåværendeInfoTilSkjema nåværendeInfo =
    Skjema.initValidertSkjema
        { yrke = nåværendeInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo
        , jobbTittel = nåværendeInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.jobbtittel
        , bedriftNavn = nåværendeInfo.tidligereInfo.tidligereInfo.tidligereInfo.bedriftNavn
        , lokasjon = nåværendeInfo.tidligereInfo.tidligereInfo.lokasjon
        , arbeidsoppgaver = nåværendeInfo.tidligereInfo.arbeidsoppgaver
        , fraMåned = nåværendeInfo.fraMåned
        , fraÅr = nåværendeInfo.fraÅr
        , tilDato = Nåværende
        , id = Nothing
        }


tilDatoTilSkjema : TilDatoInfo -> År -> ValidertArbeidserfaringSkjema
tilDatoTilSkjema tilDatoInfo år =
    Skjema.initValidertSkjema
        { yrke = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo
        , jobbTittel = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.jobbtittel
        , bedriftNavn = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.bedriftNavn
        , lokasjon = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.lokasjon
        , arbeidsoppgaver = tilDatoInfo.tidligereInfo.tidligereInfo.arbeidsoppgaver
        , fraMåned = tilDatoInfo.tidligereInfo.fraMåned
        , fraÅr = tilDatoInfo.tidligereInfo.fraÅr
        , tilDato = Avsluttet tilDatoInfo.tilMåned år
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
            ( arbeidserfaring
                |> Skjema.fraArbeidserfaring
                |> RedigerOppsummering (initSkjemaTypeaheadFraArbeidserfaring arbeidserfaring)
                |> oppdaterSamtale model (SvarFraMsg msg)
            , Cmd.batch
                [ lagtTilSpørsmålCmd model.debugStatus
                , arbeidserfaring
                    |> Cv.Arbeidserfaring.yrke
                    |> Maybe.map Yrke.label
                    |> Maybe.map (Api.getYrkeTypeahead HentetYrkeTypeahead)
                    |> Maybe.withDefault Cmd.none
                ]
            )
                |> IkkeFerdig

        BrukerVilLeggeTilNyArbeidserfaring ->
            ( initSamtaleTypeahead
                |> RegistrerYrke HarRegistrertFør { visFeilmelding = False }
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        TypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                RegistrerYrke registreringsType visFeilmelding typeaheadModel ->
                    updateSamtaleTypeahead model registreringsType visFeilmelding typeaheadMsg typeaheadModel

                RedigerOppsummering gammelTypeaheadModel skjema ->
                    let
                        ( nyTypeaheadModel, status ) =
                            Typeahead.update Yrke.label typeaheadMsg gammelTypeaheadModel
                    in
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> Typeahead.selected
                            |> Skjema.oppdaterYrke skjema
                            |> Skjema.gjørFeilmeldingYrkeSynlig (Typeahead.inputStatus status == InputBlurred)
                            |> RedigerOppsummering nyTypeaheadModel
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , case Typeahead.getSuggestionsStatus status of
                            GetSuggestionsForInput string ->
                                Api.getYrkeTypeahead HentetYrkeTypeahead string

                            DoNothing ->
                                Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetYrkeTypeahead result ->
            case model.aktivSamtale of
                RegistrerYrke registreringsType visFeilmelding typeaheadModel ->
                    case result of
                        Ok suggestions ->
                            ( suggestions
                                |> Typeahead.updateSuggestions Yrke.label typeaheadModel
                                |> RegistrerYrke registreringsType visFeilmelding
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( error
                                |> HentingFraTypeaheadFeilet typeaheadModel
                                |> oppdaterSamtale model UtenSvar
                            , Cmd.batch
                                [ lagtTilSpørsmålCmd model.debugStatus
                                , logFeilmelding error "Hente Yrketypeahead"
                                ]
                            )
                                |> IkkeFerdig

                HenterFraTypeaheadPåNyttEtterFeiling typeaheadModel _ ->
                    case result of
                        Ok suggestions ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        suggestions
                                            |> Typeahead.updateSuggestions Yrke.label typeaheadModel
                                            |> RegistrerYrke RegistrerFørsteGang { visFeilmelding = False }
                                    , seksjonsMeldingsLogg =
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Nå gikk det!" ] ]
                                }
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( error
                                |> HentingFraTypeaheadFeilet typeaheadModel
                                |> oppdaterSamtale model UtenSvar
                            , Cmd.batch
                                [ lagtTilSpørsmålCmd model.debugStatus
                                , logFeilmelding error "Hente Yrketypeahead"
                                ]
                            )
                                |> IkkeFerdig

                RedigerOppsummering typeaheadModel skjema ->
                    case result of
                        Ok suggestions ->
                            ( RedigerOppsummering (Typeahead.updateSuggestions Yrke.label typeaheadModel suggestions) skjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente Yrketypeahead" )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereYrke ->
            case model.aktivSamtale of
                RegistrerYrke registreringsType _ typeaheadModel ->
                    case Typeahead.selected typeaheadModel of
                        Just yrke ->
                            brukerVelgerYrke model msg yrke

                        Nothing ->
                            visFeilmeldingRegistrerYrke model registreringsType typeaheadModel

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilPrøveÅHenteFraTypeaheadPåNytt ->
            case model.aktivSamtale of
                HentingFraTypeaheadFeilet typeaheadModel error ->
                    ( error
                        |> HenterFraTypeaheadPåNyttEtterFeiling typeaheadModel
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , typeaheadModel
                        |> Typeahead.inputValue
                        |> Api.getYrkeTypeahead HentetYrkeTypeahead
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilAvbryteHentingFraTypeahead ->
            ( VenterPåAnimasjonFørFullføring "Ok, da går vi videre."
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilEndreJobbtittel jobbtittelInfo ->
            ( jobbtittelInfo
                |> EndreJobbtittel
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

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
                                |> arbeidsoppgaverInfoTilfraDatoInfo
                                |> RegistrereFraMåned
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Just _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykketFraMånedKnapp måned ->
            case model.aktivSamtale of
                RegistrereFraMåned fraDatoInfo ->
                    ( { fraDatoInfo | fraMåned = måned }
                        |> RegistrereFraÅr
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererFraÅr string ->
            case model.aktivSamtale of
                RegistrereFraÅr fraDatoInfo ->
                    ( { fraDatoInfo | fraÅr = string }
                        |> RegistrereFraÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        FraÅrMisterFokus ->
            case model.aktivSamtale of
                RegistrereFraÅr fraDatoInfo ->
                    ( { fraDatoInfo | visFeilmeldingFraÅr = True }
                        |> RegistrereFraÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereFraÅr ->
            case model.aktivSamtale of
                RegistrereFraÅr fraDatoInfo ->
                    case Dato.stringTilÅr fraDatoInfo.fraÅr of
                        Just fraÅr ->
                            ( { tidligereInfo = fraDatoInfo.tidligereInfo
                              , fraMåned = fraDatoInfo.fraMåned
                              , fraÅr = fraÅr
                              }
                                |> RegistrereNåværende
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { fraDatoInfo | visFeilmeldingFraÅr = True }
                                |> RegistrereFraÅr
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
                RegistrereNåværende fraDatoInfo ->
                    ( fraDatoInfo
                        |> fraDatoInfoTilTilDatoInfo
                        |> RegistrereTilMåned
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykketTilMånedKnapp måned ->
            case model.aktivSamtale of
                RegistrereTilMåned tilDatoInfo ->
                    ( { tilDatoInfo | tilMåned = måned }
                        |> RegistrereTilÅr
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererTilÅr string ->
            case model.aktivSamtale of
                RegistrereTilÅr tilDatoInfo ->
                    ( { tilDatoInfo | tilÅr = string }
                        |> RegistrereTilÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        TilÅrMisterFokus ->
            case model.aktivSamtale of
                RegistrereTilÅr tilDatoInfo ->
                    ( { tilDatoInfo | visFeilmeldingTilÅr = True }
                        |> RegistrereTilÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereTilÅr ->
            case model.aktivSamtale of
                RegistrereTilÅr tilDatoInfo ->
                    case Dato.stringTilÅr tilDatoInfo.tilÅr of
                        Just år ->
                            ( tilDatoTilSkjema tilDatoInfo år
                                |> VisOppsummering FørsteGang
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { tilDatoInfo | visFeilmeldingTilÅr = True }
                                |> RegistrereTilÅr
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
                RedigerOppsummering typeaheadModel arbeidserfaringSkjema ->
                    ( arbeidserfaringSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> RedigerOppsummering typeaheadModel
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
                RedigerOppsummering typeaheadModel skjema ->
                    case Skjema.valider skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> VisOppsummering EtterEndring
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar (validertSkjemaTilSetninger validertSkjema)))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> Skjema.gjørAlleFeilmeldingerSynlig
                                |> RedigerOppsummering typeaheadModel
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
                            in
                            ( if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                avsluttetGrunn
                                    |> SpørOmBrukerVilLeggeInnMer arbeidserfaringer
                                    |> oppdaterSamtale { model | arbeidserfaringListe = arbeidserfaringer } (ManueltSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ]))

                              else
                                avsluttetGrunn
                                    |> SpørOmBrukerVilLeggeInnMer arbeidserfaringer
                                    |> oppdaterSamtale { model | arbeidserfaringListe = arbeidserfaringer } UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerArbeidserfaring skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , postEllerPutArbeidserfaring ArbeidserfaringLagret skjema
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
                    , postEllerPutArbeidserfaring ArbeidserfaringLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilAvbryteLagringen ->
            IkkeFerdig ( Model model, Cmd.none )

        BrukerVilAvbryteRegistreringen ->
            case model.aktivSamtale of
                RegistrerYrke _ _ _ ->
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

        NyArbeidserfaring ->
            ( initSamtaleTypeahead
                |> RegistrerYrke HarRegistrertFør { visFeilmelding = False }
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

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
                                    , postEllerPutArbeidserfaring ArbeidserfaringLagret skjema
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

        GåTilNesteSeksjon ->
            case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
                FerdigAnimert ferdigAnimertMeldingsLogg ->
                    Ferdig ferdigAnimertMeldingsLogg

                MeldingerGjenstår ->
                    IkkeFerdig ( Model model, Cmd.none )

        FokusSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )


avbrytRegistrering : ModelInfo -> Msg -> SamtaleStatus
avbrytRegistrering model msg =
    ( AvbruttPåbegynt
        |> SpørOmBrukerVilLeggeInnMer model.arbeidserfaringListe
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateSamtaleTypeahead : ModelInfo -> RegistreringsType -> { visFeilmelding : Bool } -> Typeahead.Msg Yrke -> Typeahead.Model Yrke -> SamtaleStatus
updateSamtaleTypeahead model registreringsType visFeilmelding msg typeaheadModel =
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
                    visFeilmeldingRegistrerYrke model registreringsType nyTypeaheadModel

        Typeahead.InputBlurred ->
            visFeilmeldingRegistrerYrke model registreringsType nyTypeaheadModel

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerYrke registreringsType visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput string ->
                        Api.getYrkeTypeahead HentetYrkeTypeahead string

                    DoNothing ->
                        Cmd.none
                )


initSamtaleTypeahead : Typeahead.Model Yrke
initSamtaleTypeahead =
    Typeahead.init
        { value = ""
        , label = "Hvilken stilling/yrke har du?"
        , id = inputIdTilString YrkeTypeaheadId
        , toString = Yrke.label
        }


initSkjemaTypeaheadFraArbeidserfaring : Arbeidserfaring -> Typeahead.Model Yrke
initSkjemaTypeaheadFraArbeidserfaring arbeidserfaring =
    arbeidserfaring
        |> Cv.Arbeidserfaring.yrke
        |> Maybe.map initSkjemaTypeaheadFraYrke
        |> Maybe.withDefault
            (Typeahead.init
                { value = ""
                , label = "Stilling/yrke"
                , id = inputIdTilString YrkeTypeaheadId
                , toString = Yrke.label
                }
            )


initSkjemaTypeaheadFraYrke : Yrke -> Typeahead.Model Yrke
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
                    Ferdig ferdigAnimertSamtale

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
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> RedigerOppsummering (initSkjemaTypeaheadFraYrke (Skjema.yrke skjema))
        |> oppdaterSamtale model (SvarFraMsg msg)
    , Cmd.batch
        [ lagtTilSpørsmålCmd model.debugStatus
        , skjema
            |> Skjema.yrke
            |> Yrke.label
            |> Api.getYrkeTypeahead HentetYrkeTypeahead
        ]
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> Msg -> ValidertArbeidserfaringSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model msg skjema =
    ( LagreStatus.init
        |> LagrerArbeidserfaring skjema
        |> oppdaterSamtale model (SvarFraMsg msg)
    , postEllerPutArbeidserfaring ArbeidserfaringLagret skjema
    )
        |> IkkeFerdig


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        RegistrerYrke _ _ _ ->
            settFokusCmd YrkeTypeaheadId

        EndreJobbtittel _ ->
            settFokusCmd JobbtittelInput

        RegistrereBedriftsnavn _ ->
            settFokusCmd BedriftsnavnInput

        RegistrereSted _ ->
            settFokusCmd StedInput

        RegistrereArbeidsoppgaver _ _ ->
            settFokusCmd ArbeidsoppgaverInput

        RegistrereFraÅr _ ->
            settFokusCmd FraÅrInput

        RegistrereTilÅr _ ->
            settFokusCmd TilÅrInput

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


visFeilmeldingRegistrerYrke : ModelInfo -> RegistreringsType -> Typeahead.Model Yrke -> SamtaleStatus
visFeilmeldingRegistrerYrke model registreringsType typeaheadModel =
    ( typeaheadModel
        |> RegistrerYrke registreringsType { visFeilmelding = True }
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

        RegistrerYrke registreringstype _ _ ->
            case registreringstype of
                RegistrerFørsteGang ->
                    [ Melding.spørsmål [ "Nå skal du legge inn arbeidserfaring. La oss begynne med det siste arbeidsforholdet." ]
                    , Melding.spørsmål [ "Først må du velge et yrke. Begynn å skriv, velg fra listen med forslag som kommer opp." ]
                    , Melding.spørsmål [ "Du må velge et av forslagene, da kan arbeidsgivere finne deg når de søker etter folk." ]
                    ]

                HarRegistrertFør ->
                    [ Melding.spørsmål [ "Da begynner vi på nytt med å registrere yrke. Husk at du kan endre tittel som kommer på CVen senere" ] ]

        HentingFraTypeaheadFeilet _ error ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "hente forslag i søkefeltet" } ]

        HenterFraTypeaheadPåNyttEtterFeiling _ _ ->
            []

        SpørOmBrukerVilEndreJobbtittel info ->
            [ Melding.spørsmål [ "Du valgte «" ++ Yrke.label info.tidligereInfo ++ "» . Hvis dette ikke stemmer helt, kan du gi yrket et nytt navn. Det navnet vil vises på CV-en din. Ønsker du å kalle det noe annet? " ]
            ]

        EndreJobbtittel _ ->
            [ Melding.spørsmål [ "Ok, da kan du skrive inn et nytt navn." ] ]

        RegistrereBedriftsnavn _ ->
            [ Melding.spørsmål [ "Hvilken bedrift jobber eller jobbet du i?" ] ]

        RegistrereSted _ ->
            [ Melding.spørsmål [ "Hvor holder bedriften til?" ] ]

        RegistrereArbeidsoppgaver _ _ ->
            [ Melding.spørsmål [ "Fortell hvilke arbeidsoppgaver du har hatt og hva som var rollen din." ] ]

        RegistrereFraMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned begynte du i jobben?" ] ]

        RegistrereFraÅr _ ->
            [ Melding.spørsmål [ "Hvilket år begynte du i jobben?" ] ]

        RegistrereNåværende periodeInfo ->
            let
                yrkestittel =
                    case periodeInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.jobbtittel of
                        "" ->
                            Yrke.label periodeInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo

                        jobbtittel ->
                            jobbtittel
            in
            [ Melding.spørsmål [ "Jobber du fremdeles som «" ++ yrkestittel ++ "» i " ++ periodeInfo.tidligereInfo.tidligereInfo.tidligereInfo.bedriftNavn ++ "?" ] ]

        RegistrereTilMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned sluttet du i jobben?" ] ]

        RegistrereTilÅr _ ->
            [ Melding.spørsmål [ "Hvilket år sluttet du i jobben?" ] ]

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

        RedigerOppsummering _ _ ->
            [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

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
                        , Knapp.knapp FerdigMedArbeidserfaring "Nei, jeg har ikke arbeidserfaring"
                        ]

                else
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp BrukerVilLeggeTilNyArbeidserfaring "Ja, jeg vil legge til mer"
                        , Knapp.knapp BrukerHopperOverArbeidserfaring "Nei, jeg er ferdig"
                        , Knapp.knapp BrukerVilRedigereArbeidserfaring "Nei, jeg vil endre det jeg har lagt inn"
                        ]

            VelgEnArbeidserfaringÅRedigere ->
                BrukerInput.knapper Kolonne
                    (List.map lagArbeidserfaringKnapp model.arbeidserfaringListe)

            RegistrerYrke _ visFeilmelding typeaheadModel ->
                viewRegistrerYrke visFeilmelding typeaheadModel

            HentingFraTypeaheadFeilet _ error ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilAvbryteHentingFraTypeahead "Gå videre"
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilPrøveÅHenteFraTypeaheadPåNytt "Prøv igjen"
                            , Knapp.knapp BrukerVilAvbryteHentingFraTypeahead "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            HenterFraTypeaheadPåNyttEtterFeiling _ error ->
                case ErrorHåndtering.operasjonEtterError error of
                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

                    _ ->
                        BrukerInput.utenInnhold

            SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerVilIkkeEndreJobbtittel "Nei, jeg vil ikke kalle det noe annet"
                    , Knapp.knapp (BrukerVilEndreJobbtittel jobbtittelInfo) "Ja, jeg vil kalle det noe annet"
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

            RegistrereFraMåned _ ->
                BrukerInput.månedKnapper { onAvbryt = BrukerVilAvbryteRegistreringen, onMånedValg = BrukerTrykketFraMånedKnapp }

            RegistrereFraÅr fraDatoInfo ->
                BrukerInput.inputMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilRegistrereFraÅr }
                    (fraDatoInfo.fraÅr
                        |> Input.input { label = "År", msg = BrukerOppdatererFraÅr }
                        |> Input.withClass "aar"
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withOnEnter BrukerVilRegistrereFraÅr
                        |> Input.withOnBlur FraÅrMisterFokus
                        |> Input.withId (inputIdTilString FraÅrInput)
                        |> Input.withFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue fraDatoInfo.visFeilmeldingFraÅr) fraDatoInfo.fraÅr)
                        |> Input.withErObligatorisk
                    )

            RegistrereNåværende _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerSvarerJaTilNåværende "Ja"
                    , Knapp.knapp BrukerSvarerNeiTilNåværende "Nei"
                    ]

            RegistrereTilMåned _ ->
                BrukerInput.månedKnapper { onAvbryt = BrukerVilAvbryteRegistreringen, onMånedValg = BrukerTrykketTilMånedKnapp }

            RegistrereTilÅr tilDatoInfo ->
                BrukerInput.inputMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilRegistrereTilÅr }
                    (tilDatoInfo.tilÅr
                        |> Input.input { label = "År", msg = BrukerOppdatererTilÅr }
                        |> Input.withClass "aar"
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withOnEnter BrukerVilRegistrereTilÅr
                        |> Input.withOnBlur TilÅrMisterFokus
                        |> Input.withId (inputIdTilString TilÅrInput)
                        |> Input.withFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue tilDatoInfo.visFeilmeldingTilÅr) tilDatoInfo.tilÅr)
                        |> Input.withErObligatorisk
                    )

            VisOppsummering _ skjema ->
                case Skjema.id skjema of
                    Just _ ->
                        viewBekreftOppsummering False

                    Nothing ->
                        viewBekreftOppsummering True

            RedigerOppsummering typeaheadModel skjema ->
                BrukerInput.skjema { lagreMsg = BrukerVilLagreArbeidserfaringSkjema, lagreKnappTekst = "Lagre endringer" }
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
                            { label = "Når begynte du i jobbben?"
                            , onMånedChange = FraMåned >> SkjemaEndret
                            , måned = Skjema.fraMåned skjema
                            , onÅrChange = Tekst FraÅr >> SkjemaEndret
                            , år = Skjema.innholdTekstFelt FraÅr skjema
                            }
                            |> DatoInput.withFeilmeldingÅr (Skjema.feilmeldingFraÅr skjema)
                            |> DatoInput.withOnBlurÅr (SkjemaEndret FraÅrBlurred)
                            |> DatoInput.toHtml
                        , if not (Skjema.nåværende skjema) then
                            DatoInput.datoInput
                                { label = "Når sluttet du i jobben?"
                                , onMånedChange = TilMåned >> SkjemaEndret
                                , måned = Skjema.tilMåned skjema
                                , onÅrChange = Tekst TilÅr >> SkjemaEndret
                                , år = Skjema.innholdTekstFelt TilÅr skjema
                                }
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

            BekreftSlettingAvPåbegynt _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BekrefterSlettPåbegynt "Ja, jeg vil slette"
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
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilPrøveÅLagrePåNytt "Prøv igjen"
                            , Knapp.knapp BrukerVilAvbryteLagringen "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            SpørOmBrukerVilLeggeInnMer arbeidserfaringer _ ->
                BrukerInput.knapper Flytende
                    ([ [ Knapp.knapp NyArbeidserfaring "Ja, legg til en arbeidserfaring"
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
                    , Knapp.knapp BrukerVilIkkeAvbryteRegistreringen "Nei, jeg vil fortsette"
                    ]

    else
        BrukerInput.utenInnhold


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


type InputId
    = YrkeTypeaheadId
    | JobbtittelInput
    | BedriftsnavnInput
    | StedInput
    | ArbeidsoppgaverInput
    | FraÅrInput
    | TilÅrInput


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        YrkeTypeaheadId ->
            "arbeidserfaring-registrer-yrke-typeahead"

        JobbtittelInput ->
            "arbeidserfaring-registrer-jobbtittel"

        BedriftsnavnInput ->
            "arbeidserfaring-registrer-bedriftsnavn"

        StedInput ->
            "arbeidserfaring-registrer-sted"

        ArbeidsoppgaverInput ->
            "arbeidserfaring-registrer-arbeidsoppgaver"

        FraÅrInput ->
            "arbeidserfaring-registrer-fra-år"

        TilÅrInput ->
            "arbeidserfaring-registrer-til-år"


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
            , Knapp.knapp BrukerVilRedigereOppsummering "Nei, jeg vil endre"
            , Knapp.knapp VilSlettePåbegynt "Nei, jeg vil slette"
            ]

    else
        BrukerInput.knapper Flytende
            [ Knapp.knapp BrukerVilLagreArbeidserfaringIOppsummering "Ja, det er riktig"
            , Knapp.knapp BrukerVilRedigereOppsummering "Nei, jeg vil endre"
            ]


lagArbeidserfaringKnapp : Arbeidserfaring -> Knapp Msg
lagArbeidserfaringKnapp arbeidserfaring =
    let
        tekst =
            Maybe.withDefault "" (Cv.Arbeidserfaring.yrkeString arbeidserfaring)
                ++ ", "
                ++ Maybe.withDefault "" (Cv.Arbeidserfaring.arbeidsgiver arbeidserfaring)
    in
    Knapp.knapp (BrukerHarValgtArbeidserfaringÅRedigere arbeidserfaring) tekst


postEllerPutArbeidserfaring : (Result Error (List Arbeidserfaring) -> msg) -> Skjema.ValidertArbeidserfaringSkjema -> Cmd msg
postEllerPutArbeidserfaring msgConstructor skjema =
    case Skjema.id skjema of
        Just id ->
            Api.putArbeidserfaring msgConstructor skjema id

        Nothing ->
            Api.postArbeidserfaring msgConstructor skjema


arbeidserfaringerTilTekstområder : List Arbeidserfaring -> List Tekstområde
arbeidserfaringerTilTekstområder arbeidserfaringer =
    arbeidserfaringer
        |> List.map arbeidserfaringTilTekstområde
        |> List.intersperse (Avsnitt Melding.tomLinje)


arbeidserfaringTilTekstområde : Arbeidserfaring -> Tekstområde
arbeidserfaringTilTekstområde arbeidserfaring =
    Seksjon (beskrivArbeidserfaring arbeidserfaring)
        [ Dato.periodeTilString (Cv.Arbeidserfaring.fraMåned arbeidserfaring) (Cv.Arbeidserfaring.fraÅr arbeidserfaring) (Cv.Arbeidserfaring.tilDato arbeidserfaring)
        , beskrivArbeidserfaring arbeidserfaring
        ]


beskrivArbeidserfaring : Arbeidserfaring -> String
beskrivArbeidserfaring arbeidserfaring =
    let
        maybeYrkeTekst : Maybe String
        maybeYrkeTekst =
            Maybe.or
                (Cv.Arbeidserfaring.yrkeFritekst arbeidserfaring)
                (Cv.Arbeidserfaring.yrkeString arbeidserfaring)
    in
    [ maybeYrkeTekst
    , Cv.Arbeidserfaring.arbeidsgiver arbeidserfaring
    ]
        |> Maybe.values
        |> List.intersperse "hos"
        |> String.join " "


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError (always ErrorLogget))
        |> Maybe.withDefault Cmd.none


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List Arbeidserfaring -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg arbeidserfaringsListe =
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
