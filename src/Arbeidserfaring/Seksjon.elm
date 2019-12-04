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
import Dato exposing (Måned(..), TilDato(..), År)
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Feilmelding
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.ManedKnapper as MånedKnapper
import FrontendModuler.Textarea as Textarea
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (class)
import Http exposing (Error)
import LagreStatus exposing (LagreStatus)
import Maybe.Extra as Maybe
import Meldinger.Melding as Melding exposing (Melding, Tekstområde(..))
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
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


type Samtale
    = Intro
    | VelgEnArbeidserfaringÅRedigere
    | RegistrerYrke Bool (Typeahead.Model Yrke)
    | SpørOmBrukerVilEndreJobbtittel JobbtittelInfo
    | EndreJobbtittel JobbtittelInfo
    | RegistrereBedriftsnavn BedriftnavnInfo
    | RegistrereSted StedInfo
    | RegistrereArbeidsoppgaver ArbeidsoppgaverInfo
    | RegistrereFraMåned FraDatoInfo
    | RegistrereFraÅr FraDatoInfo
    | RegistrereNåværende NåværendeInfo
    | RegistrereTilMåned TilDatoInfo
    | RegistrereTilÅr TilDatoInfo
    | VisOppsummering ValidertArbeidserfaringSkjema
    | RedigerOppsummering (Typeahead.Model Yrke) ArbeidserfaringSkjema
    | VisOppsummeringEtterEndring ValidertArbeidserfaringSkjema
    | LagrerArbeidserfaring ValidertArbeidserfaringSkjema LagreStatus
    | LagringFeilet Http.Error ValidertArbeidserfaringSkjema
    | SpørOmBrukerVilLeggeInnMer Bool
    | StartNyArbeidserfaring (Typeahead.Model Yrke) -- Denne brukes kun for å få en annen melding fra Cvert i meldingsloggen, men hopper over til RegistrerYrke etter det
    | VenterPåAnimasjonFørFullføring String



--- UPDATE ---


type Msg
    = BrukerVilLeggeTilNyArbeidserfaring String
    | BrukerVilRedigereArbeidserfaring String
    | BrukerHarValgtArbeidserfaringÅRedigere Arbeidserfaring String
    | BrukerHopperOverArbeidserfaring
    | TypeaheadMsg (Typeahead.Msg Yrke)
    | HentetYrkeTypeahead (Result Http.Error (List Yrke))
    | BrukerVilRegistrereYrke
    | BrukerVilEndreJobbtittel JobbtittelInfo
    | BrukerVilIkkeEndreJobbtittel
    | BrukerOppdatererJobbtittelFelt String
    | BrukerVilRegistrereJobbtittel
    | BrukerOppdatererBedriftsnavn String
    | BrukerVilRegistrereBedriftsnavn
    | BrukerOppdatererSted String
    | BrukerVilRegistrereSted
    | BrukerOppdatererArbeidsoppgaver String
    | BrukerVilRegistrereArbeidsoppgaver
    | BrukerTrykketFraMånedKnapp Dato.Måned
    | BrukerOppdatererFraÅr String
    | FraÅrMisterFokus
    | BrukerVilRegistrereFraÅr
    | BrukerSvarerJaTilNåværende
    | BrukerSvarerNeiTilNåværende
    | BrukerTrykketTilMånedKnapp Dato.Måned
    | BrukerOppdatererTilÅr String
    | TilÅrMisterFokus
    | BrukerVilRegistrereTilÅr
    | BrukerVilRedigereOppsummering
    | SkjemaEndret SkjemaEndring
    | BrukerVilLagreArbeidserfaringIOppsummering
    | BrukerVilLagreArbeidserfaringSkjema
    | ArbeidserfaringLagret (Result Http.Error (List Arbeidserfaring))
    | BrukerVilPrøveÅLagrePåNytt
    | BrukerVilAvbryteLagringen
    | NyArbeidserfaring
    | FerdigMedArbeidserfaring String
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
                |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg er ferdig" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilRedigereArbeidserfaring knappeTekst ->
            ( VelgEnArbeidserfaringÅRedigere
                |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerHarValgtArbeidserfaringÅRedigere arbeidserfaring knappeTekst ->
            ( arbeidserfaring
                |> Skjema.fraArbeidserfaring
                |> RedigerOppsummering (initSkjemaTypeaheadFraArbeidserfaring arbeidserfaring)
                |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
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

        BrukerVilLeggeTilNyArbeidserfaring knappeTekst ->
            ( initSamtaleTypeahead
                |> RegistrerYrke False
                |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        TypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                RegistrerYrke visFeilmelding typeaheadModel ->
                    updateSamtaleTypeahead model visFeilmelding typeaheadMsg typeaheadModel

                StartNyArbeidserfaring typeaheadModel ->
                    updateSamtaleTypeahead model False typeaheadMsg typeaheadModel

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
                            |> oppdaterSamtaleSteg model
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
                RegistrerYrke visFeilmelding typeaheadModel ->
                    case result of
                        Ok suggestions ->
                            ( let
                                nyTypeaheadModel =
                                    Typeahead.updateSuggestions Yrke.label typeaheadModel suggestions
                              in
                              nyTypeaheadModel
                                |> RegistrerYrke visFeilmelding
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente Yrketypeahead" )
                                |> IkkeFerdig

                RedigerOppsummering typeaheadModel skjema ->
                    case result of
                        Ok suggestions ->
                            ( RedigerOppsummering (Typeahead.updateSuggestions Yrke.label typeaheadModel suggestions) skjema
                                |> oppdaterSamtaleSteg model
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
                RegistrerYrke _ typeaheadModel ->
                    case Typeahead.selected typeaheadModel of
                        Just yrke ->
                            brukerVelgerYrke model yrke

                        Nothing ->
                            visFeilmeldingRegistrerYrke model typeaheadModel

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilEndreJobbtittel jobbtittelInfo ->
            ( EndreJobbtittel jobbtittelInfo
                |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil ikke kalle det noe annet" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerOppdatererJobbtittelFelt string ->
            case model.aktivSamtale of
                EndreJobbtittel jobbtittelInfo ->
                    ( Model
                        { model
                            | aktivSamtale = EndreJobbtittel { jobbtittelInfo | jobbtittel = string }
                        }
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
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil ikke kalle det noe annet" ])
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
                        |> nesteSamtaleSteg model (Melding.svar [ jobbtittelInfo.jobbtittel ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        BrukerOppdatererBedriftsnavn string ->
            case model.aktivSamtale of
                RegistrereBedriftsnavn beriftnavnsInfo ->
                    ( Model
                        { model
                            | aktivSamtale = RegistrereBedriftsnavn { beriftnavnsInfo | bedriftNavn = string }
                        }
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
                        |> nesteSamtaleSteg model (Melding.svar [ bedriftnavnInfo.bedriftNavn ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererSted string ->
            case model.aktivSamtale of
                RegistrereSted stedInfo ->
                    ( Model
                        { model
                            | aktivSamtale = RegistrereSted { stedInfo | lokasjon = string }
                        }
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
                        |> RegistrereArbeidsoppgaver
                        |> nesteSamtaleSteg model (Melding.svar [ stedInfo.lokasjon ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererArbeidsoppgaver string ->
            case model.aktivSamtale of
                RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
                    ( Model
                        { model
                            | aktivSamtale =
                                RegistrereArbeidsoppgaver { arbeidsoppgaverInfo | arbeidsoppgaver = string }
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereArbeidsoppgaver ->
            case model.aktivSamtale of
                RegistrereArbeidsoppgaver arbeidsOppgaveInfo ->
                    case Validering.feilmeldingMaxAntallTegn arbeidsOppgaveInfo.arbeidsoppgaver maxLengthArbeidsoppgaver of
                        Nothing ->
                            ( arbeidsOppgaveInfo
                                |> arbeidsoppgaverInfoTilfraDatoInfo
                                |> RegistrereFraMåned
                                |> nesteSamtaleSteg model (Melding.svar [ arbeidsOppgaveInfo.arbeidsoppgaver ])
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
                    ( måned
                        |> setFraMåned fraDatoInfo
                        |> RegistrereFraÅr
                        |> nesteSamtaleSteg model
                            (Melding.svar
                                [ måned
                                    |> Dato.månedTilString
                                ]
                            )
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
                        |> oppdaterSamtaleSteg model
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
                        |> oppdaterSamtaleSteg model
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
                                |> nesteSamtaleSteg model (Melding.svar [ fraDatoInfo.fraÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { fraDatoInfo | visFeilmeldingFraÅr = True }
                                |> RegistrereFraÅr
                                |> oppdaterSamtaleSteg model
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
                        |> VisOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja" ])
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
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykketTilMånedKnapp måned ->
            case model.aktivSamtale of
                RegistrereTilMåned tilDatoInfo ->
                    ( måned
                        |> setTilMåned tilDatoInfo
                        |> RegistrereTilÅr
                        |> nesteSamtaleSteg model
                            (Melding.svar
                                [ måned
                                    |> Dato.månedTilString
                                ]
                            )
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
                        |> oppdaterSamtaleSteg model
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
                        |> oppdaterSamtaleSteg model
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
                                |> VisOppsummering
                                |> nesteSamtaleSteg model (Melding.svar [ tilDatoInfo.tilÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { tilDatoInfo | visFeilmeldingTilÅr = True }
                                |> RegistrereTilÅr
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRedigereOppsummering ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    updateEtterVilEndreSkjema model skjema

                VisOppsummeringEtterEndring skjema ->
                    updateEtterVilEndreSkjema model skjema

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                RedigerOppsummering typeaheadModel arbeidserfaringSkjema ->
                    ( arbeidserfaringSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> RedigerOppsummering typeaheadModel
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilLagreArbeidserfaringIOppsummering ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    updateEtterLagreKnappTrykket model skjema

                VisOppsummeringEtterEndring skjema ->
                    updateEtterLagreKnappTrykket model skjema

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilLagreArbeidserfaringSkjema ->
            case model.aktivSamtale of
                RedigerOppsummering typeaheadModel skjema ->
                    case Skjema.valider skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> VisOppsummeringEtterEndring
                                |> nesteSamtaleSteg model (Melding.svar (validertSkjemaTilSetninger validertSkjema))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> Skjema.gjørAlleFeilmeldingerSynlig
                                |> RedigerOppsummering typeaheadModel
                                |> oppdaterSamtaleSteg model
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
                                harEndretEksisterende =
                                    List.length model.arbeidserfaringListe == List.length arbeidserfaringer
                            in
                            ( if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                SpørOmBrukerVilLeggeInnMer harEndretEksisterende
                                    |> nesteSamtaleSteg { model | arbeidserfaringListe = arbeidserfaringer } (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])

                              else
                                SpørOmBrukerVilLeggeInnMer harEndretEksisterende
                                    |> nesteSamtaleStegUtenSvar { model | arbeidserfaringListe = arbeidserfaringer }
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerArbeidserfaring skjema
                                        |> oppdaterSamtaleSteg model
                                    , postEllerPutArbeidserfaring ArbeidserfaringLagret skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtaleSteg model
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre arbeidserfaring" error
                                    )
                                        |> IkkeFerdig

                            else
                                ( skjema
                                    |> LagringFeilet error
                                    |> nesteSamtaleStegUtenSvar model
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
                        |> nesteSamtaleSteg model (Melding.svar [ "Prøv igjen" ])
                    , postEllerPutArbeidserfaring ArbeidserfaringLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilAvbryteLagringen ->
            IkkeFerdig ( Model model, Cmd.none )

        NyArbeidserfaring ->
            ( initSamtaleTypeahead
                |> StartNyArbeidserfaring
                |> nesteSamtaleSteg model (Melding.svar [ "Ja, legg til en arbeidserfaring" ])
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
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagringFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerArbeidserfaring skjema
                                        |> oppdaterSamtaleSteg model
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

        FerdigMedArbeidserfaring knappeTekst ->
            if List.isEmpty model.arbeidserfaringListe then
                ( VenterPåAnimasjonFørFullføring "Ok, da går vi videre."
                    |> nesteSamtaleSteg model
                        (Melding.svar [ knappeTekst ])
                , lagtTilSpørsmålCmd model.debugStatus
                )
                    |> IkkeFerdig

            else
                ( VenterPåAnimasjonFørFullføring "Bra innsats! 😊 Nå kan arbeidsgivere finne deg hvis de ser etter en med din erfaring."
                    |> nesteSamtaleSteg model
                        (Melding.svar [ knappeTekst ])
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


updateSamtaleTypeahead : ModelInfo -> Bool -> Typeahead.Msg Yrke -> Typeahead.Model Yrke -> SamtaleStatus
updateSamtaleTypeahead model visFeilmelding msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Yrke.label msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected typeaheadModel of
                Just yrke ->
                    brukerVelgerYrke model yrke

                Nothing ->
                    visFeilmeldingRegistrerYrke model nyTypeaheadModel

        Typeahead.InputBlurred ->
            visFeilmeldingRegistrerYrke model nyTypeaheadModel

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerYrke visFeilmelding
                    |> oppdaterSamtaleSteg model
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
                |> Dato.stringTilMåned
                |> Skjema.oppdaterFraMåned skjema

        TilMåned månedString ->
            månedString
                |> Dato.stringTilMåned
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


updateEtterVilEndreSkjema : ModelInfo -> ValidertArbeidserfaringSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model skjema =
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> RedigerOppsummering (initSkjemaTypeaheadFraYrke (Skjema.yrke skjema))
        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
    , Cmd.batch
        [ lagtTilSpørsmålCmd model.debugStatus
        , skjema
            |> Skjema.yrke
            |> Yrke.label
            |> Api.getYrkeTypeahead HentetYrkeTypeahead
        ]
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> ValidertArbeidserfaringSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model skjema =
    ( LagreStatus.init
        |> LagrerArbeidserfaring skjema
        |> nesteSamtaleSteg model (Melding.svar [ "Ja, informasjonen er riktig" ])
    , postEllerPutArbeidserfaring ArbeidserfaringLagret skjema
    )
        |> IkkeFerdig


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        RegistrerYrke _ _ ->
            settFokusCmd YrkeTypeaheadId

        EndreJobbtittel _ ->
            settFokusCmd JobbtittelInput

        RegistrereBedriftsnavn _ ->
            settFokusCmd BedriftsnavnInput

        RegistrereSted _ ->
            settFokusCmd StedInput

        RegistrereArbeidsoppgaver _ ->
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


brukerVelgerYrke : ModelInfo -> Yrke -> SamtaleStatus
brukerVelgerYrke info yrkesTypeahead =
    ( yrkesTypeahead
        |> yrkeInfoTilJobbtittelInfo
        |> SpørOmBrukerVilEndreJobbtittel
        |> nesteSamtaleSteg info (Melding.svar [ Yrke.label yrkesTypeahead ])
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
    ( RegistrerYrke True typeaheadModel
        |> oppdaterSamtaleSteg model
    , Cmd.none
    )
        |> IkkeFerdig


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg modelInfo samtaleSeksjon =
    case samtaleSeksjon of
        SpørOmBrukerVilLeggeInnMer _ ->
            Model
                { modelInfo
                    | aktivSamtale = samtaleSeksjon
                    , seksjonsMeldingsLogg =
                        modelInfo.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
                }

        _ ->
            Model
                { modelInfo
                    | aktivSamtale = samtaleSeksjon
                }


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg modelInfo melding samtaleSeksjon =
    Model
        { modelInfo
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                modelInfo.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


nesteSamtaleStegUtenSvar : ModelInfo -> Samtale -> Model
nesteSamtaleStegUtenSvar modelInfo samtaleSeksjon =
    Model
        { modelInfo
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                modelInfo.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


setFraMåned : FraDatoInfo -> Dato.Måned -> FraDatoInfo
setFraMåned fraDatoInfo måned =
    { fraDatoInfo | fraMåned = måned }


setTilMåned : TilDatoInfo -> Dato.Måned -> TilDatoInfo
setTilMåned tilDatoInfo måned =
    { tilDatoInfo | tilMåned = måned }


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
            , Melding.spørsmål [ "Du må velge et av forslagene, da kan arbeidsgivere finne deg når de søker etter folk." ]
            ]

        SpørOmBrukerVilEndreJobbtittel info ->
            [ Melding.spørsmål [ "Du valgte «" ++ Yrke.label info.tidligereInfo ++ "» . Hvis dette ikke stemmer helt, kan du gi yrket et nytt navn. Det navnet vil vises på CV-en din. Ønsker du å kalle det noe annet? " ]
            ]

        EndreJobbtittel _ ->
            []

        RegistrereBedriftsnavn _ ->
            [ Melding.spørsmål [ "Hvilken bedrift jobber eller jobbet du i?" ] ]

        RegistrereSted _ ->
            [ Melding.spørsmål [ "Hvor holder bedriften til?" ] ]

        RegistrereArbeidsoppgaver _ ->
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

        VisOppsummering validertSkjema ->
            [ Melding.spørsmål
                (validertSkjemaTilSetninger validertSkjema
                    ++ [ Melding.tomLinje
                       , "Er informasjonen riktig?"
                       ]
                )
            ]

        RedigerOppsummering _ _ ->
            [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

        VisOppsummeringEtterEndring _ ->
            [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

        LagrerArbeidserfaring _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre arbeidserfaringen" } ]

        SpørOmBrukerVilLeggeInnMer harEndretEksisterende ->
            [ if harEndretEksisterende then
                Melding.spørsmål [ "Flott! Nå er arbeidserfaringen endret." ]

              else
                Melding.spørsmål [ "Flott! Nå er arbeidserfaringen lagret." ]
            , Melding.spørsmål [ "Vil du legge inn flere arbeidserfaringer?" ]
            ]

        StartNyArbeidserfaring _ ->
            [ Melding.spørsmål [ "Da begynner vi på nytt med å registrere yrke. Husk at du kan endre tittel som kommer på CVen senere" ] ]

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



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            Intro ->
                if List.isEmpty model.arbeidserfaringListe then
                    Containers.knapper Flytende
                        [ "Ja, jeg har arbeidserfaring"
                            |> Knapp.knapp (BrukerVilLeggeTilNyArbeidserfaring "Ja, jeg har arbeidserfaring")
                            |> Knapp.toHtml
                        , Knapp.knapp (FerdigMedArbeidserfaring "Nei, jeg har ikke arbeidserfaring") "Nei, jeg har ikke arbeidserfaring"
                            |> Knapp.toHtml
                        ]

                else
                    Containers.knapper Flytende
                        [ "Ja, jeg vil legge til mer"
                            |> Knapp.knapp (BrukerVilLeggeTilNyArbeidserfaring "Ja, jeg vil legge til mer")
                            |> Knapp.toHtml
                        , Knapp.knapp BrukerHopperOverArbeidserfaring "Nei, jeg er ferdig"
                            |> Knapp.toHtml
                        , Knapp.knapp (BrukerVilRedigereArbeidserfaring "Nei, jeg vil endre det jeg har lagt inn") "Nei, jeg vil endre det jeg har lagt inn"
                            |> Knapp.toHtml
                        ]

            VelgEnArbeidserfaringÅRedigere ->
                Containers.knapper Kolonne
                    (List.map lagArbeidserfaringKnapp model.arbeidserfaringListe)

            RegistrerYrke visFeilmelding typeaheadModel ->
                Containers.typeaheadMedGåVidereKnapp BrukerVilRegistrereYrke
                    [ typeaheadModel
                        |> feilmeldingTypeahead
                        |> maybeHvisTrue visFeilmelding
                        |> Typeahead.view Yrke.label typeaheadModel
                        |> Html.map TypeaheadMsg
                    ]

            SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
                Containers.knapper Flytende
                    [ "Nei, jeg vil ikke kalle det noe annet"
                        |> Knapp.knapp BrukerVilIkkeEndreJobbtittel
                        |> Knapp.toHtml
                    , "Ja, jeg vil kalle det noe annet"
                        |> Knapp.knapp (BrukerVilEndreJobbtittel jobbtittelInfo)
                        |> Knapp.toHtml
                    ]

            EndreJobbtittel jobbtittelInfo ->
                Containers.inputMedGåVidereKnapp BrukerVilRegistrereJobbtittel
                    [ jobbtittelInfo.jobbtittel
                        |> Input.input { label = "Stilling/yrke som vil vises i CV-en", msg = BrukerOppdatererJobbtittelFelt }
                        |> Input.withOnEnter BrukerVilRegistrereJobbtittel
                        |> Input.withId (inputIdTilString JobbtittelInput)
                        |> Input.toHtml
                    ]

            RegistrereBedriftsnavn bedriftnanvsInfo ->
                Containers.inputMedGåVidereKnapp BrukerVilRegistrereBedriftsnavn
                    [ bedriftnanvsInfo.bedriftNavn
                        |> Input.input { label = "Bedriftens navn", msg = BrukerOppdatererBedriftsnavn }
                        |> Input.withOnEnter BrukerVilRegistrereBedriftsnavn
                        |> Input.withId (inputIdTilString BedriftsnavnInput)
                        |> Input.toHtml
                    ]

            RegistrereSted stedInfo ->
                Containers.inputMedGåVidereKnapp BrukerVilRegistrereSted
                    [ stedInfo.lokasjon
                        |> Input.input { label = "By, sted eller land", msg = BrukerOppdatererSted }
                        |> Input.withOnEnter BrukerVilRegistrereSted
                        |> Input.withId (inputIdTilString StedInput)
                        |> Input.toHtml
                    ]

            RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
                Containers.inputMedGåVidereKnapp BrukerVilRegistrereArbeidsoppgaver
                    [ arbeidsoppgaverInfo.arbeidsoppgaver
                        |> Textarea.textarea { label = "Arbeidsoppgaver", msg = BrukerOppdatererArbeidsoppgaver }
                        |> Textarea.withId (inputIdTilString ArbeidsoppgaverInput)
                        |> Textarea.withMaybeFeilmelding (Validering.feilmeldingMaxAntallTegn arbeidsoppgaverInfo.arbeidsoppgaver maxLengthArbeidsoppgaver)
                        |> Textarea.toHtml
                    ]

            RegistrereFraMåned _ ->
                MånedKnapper.månedKnapper BrukerTrykketFraMånedKnapp

            RegistrereFraÅr fraDatoInfo ->
                Containers.inputMedGåVidereKnapp BrukerVilRegistrereFraÅr
                    [ div [ class "år-wrapper" ]
                        [ fraDatoInfo.fraÅr
                            |> Input.input { label = "År", msg = BrukerOppdatererFraÅr }
                            |> Input.withClass "aar"
                            |> Input.withOnEnter BrukerVilRegistrereFraÅr
                            |> Input.withOnBlur FraÅrMisterFokus
                            |> Input.withId (inputIdTilString FraÅrInput)
                            |> Input.withMaybeFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue fraDatoInfo.visFeilmeldingFraÅr) fraDatoInfo.fraÅr)
                            |> Input.withErObligatorisk
                            |> Input.toHtml
                        ]
                    ]

            RegistrereNåværende _ ->
                Containers.knapper Flytende
                    [ Knapp.knapp BrukerSvarerJaTilNåværende "Ja"
                        |> Knapp.toHtml
                    , Knapp.knapp BrukerSvarerNeiTilNåværende "Nei"
                        |> Knapp.toHtml
                    ]

            RegistrereTilMåned _ ->
                MånedKnapper.månedKnapper BrukerTrykketTilMånedKnapp

            RegistrereTilÅr tilDatoInfo ->
                Containers.inputMedGåVidereKnapp BrukerVilRegistrereTilÅr
                    [ div [ class "år-wrapper" ]
                        [ tilDatoInfo.tilÅr
                            |> Input.input { label = "År", msg = BrukerOppdatererTilÅr }
                            |> Input.withClass "aar"
                            |> Input.withOnEnter BrukerVilRegistrereTilÅr
                            |> Input.withOnBlur TilÅrMisterFokus
                            |> Input.withId (inputIdTilString TilÅrInput)
                            |> Input.withMaybeFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue tilDatoInfo.visFeilmeldingTilÅr) tilDatoInfo.tilÅr)
                            |> Input.withErObligatorisk
                            |> Input.toHtml
                        ]
                    ]

            VisOppsummering _ ->
                Containers.knapper Flytende
                    [ Knapp.knapp BrukerVilLagreArbeidserfaringIOppsummering "Ja, informasjonen er riktig"
                        |> Knapp.toHtml
                    , Knapp.knapp BrukerVilRedigereOppsummering "Nei, jeg vil endre"
                        |> Knapp.toHtml
                    ]

            VisOppsummeringEtterEndring _ ->
                viewBekreftOppsummering

            RedigerOppsummering typeaheadModel skjema ->
                Containers.skjema { lagreMsg = BrukerVilLagreArbeidserfaringSkjema, lagreKnappTekst = "Lagre endringer" }
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
                        |> Textarea.withMaybeFeilmelding (Validering.feilmeldingMaxAntallTegn (Skjema.innholdTekstFelt Arbeidsoppgaver skjema) maxLengthArbeidsoppgaver)
                        |> Textarea.toHtml
                    , div [ class "DatoInput-fra-til-rad" ]
                        [ DatoInput.datoInput
                            { label = "Når begynte du i jobbben?"
                            , onMånedChange = FraMåned >> SkjemaEndret
                            , måned = Skjema.fraMåned skjema
                            , onÅrChange = Tekst FraÅr >> SkjemaEndret
                            , år = Skjema.innholdTekstFelt FraÅr skjema
                            }
                            |> DatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingFraÅr skjema)
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
                                |> DatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingTilÅr skjema)
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

            LagrerArbeidserfaring _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    text ""

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        Containers.knapper Flytende
                            [ Knapp.knapp BrukerVilAvbryteLagringen "Gå videre"
                                |> Knapp.toHtml
                            ]

                    PrøvPåNytt ->
                        Containers.knapper Flytende
                            [ Knapp.knapp BrukerVilPrøveÅLagrePåNytt "Prøv igjen"
                                |> Knapp.toHtml
                            , Knapp.knapp BrukerVilAvbryteLagringen "Gå videre"
                                |> Knapp.toHtml
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            SpørOmBrukerVilLeggeInnMer _ ->
                Containers.knapper Flytende
                    [ Knapp.knapp NyArbeidserfaring "Ja, legg til en arbeidserfaring"
                        |> Knapp.toHtml
                    , Knapp.knapp (FerdigMedArbeidserfaring "Nei, jeg har lagt inn alle") "Nei, jeg har lagt inn alle"
                        |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRedigereArbeidserfaring "Nei, jeg vil endre det jeg har lagt inn") "Nei, jeg vil endre det jeg har lagt inn"
                        |> Knapp.toHtml
                    ]

            StartNyArbeidserfaring typeaheadModel ->
                Containers.typeaheadMedGåVidereKnapp BrukerVilRegistrereYrke
                    [ Typeahead.view Yrke.label typeaheadModel Nothing
                        |> Html.map TypeaheadMsg
                    ]

            VenterPåAnimasjonFørFullføring _ ->
                text ""

    else
        text ""


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


viewBekreftOppsummering : Html Msg
viewBekreftOppsummering =
    Containers.knapper Flytende
        [ Knapp.knapp BrukerVilLagreArbeidserfaringIOppsummering "Ja, informasjonen er riktig"
            |> Knapp.toHtml
        , Knapp.knapp BrukerVilRedigereOppsummering "Nei, jeg vil endre"
            |> Knapp.toHtml
        ]


lagArbeidserfaringKnapp : Arbeidserfaring -> Html Msg
lagArbeidserfaringKnapp arbeidserfaring =
    let
        text =
            Maybe.withDefault "" (Cv.Arbeidserfaring.yrkeString arbeidserfaring)
                ++ ", "
                ++ Maybe.withDefault "" (Cv.Arbeidserfaring.arbeidsgiver arbeidserfaring)
    in
    Knapp.knapp (BrukerHarValgtArbeidserfaringÅRedigere arbeidserfaring text) text
        |> Knapp.toHtml


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
