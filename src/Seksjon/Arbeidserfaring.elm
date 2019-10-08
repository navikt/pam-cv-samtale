module Seksjon.Arbeidserfaring exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , update
    , viewBrukerInput
    )

import Api
import Browser.Dom as Dom
import Cv.Arbeidserfaring exposing (Arbeidserfaring)
import Dato exposing (Måned(..), TilDato(..), År)
import DebugStatus exposing (DebugStatus)
import Feilmelding
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.ManedKnapper as MånedKnapper
import FrontendModuler.Textarea as Textarea
import FrontendModuler.Typeahead as Typeahead
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (class)
import Http exposing (Error)
import Melding exposing (Melding)
import MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Process
import SamtaleAnimasjon
import Skjema.Arbeidserfaring as ArbeidserfaringSkjema exposing (ArbeidserfaringSkjema, Felt(..), TypeaheadFelt(..), ValidertArbeidserfaringSkjema)
import Task
import TypeaheadState exposing (TypeaheadState)
import Yrke exposing (Yrke)



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



--- UPDATE ---


type Msg
    = BrukerVilLeggeTilNyArbeidserfaring String
    | BrukerVilRedigereArbeidserfaring String
    | BrukerHarValgtArbeidserfaringÅRedigere Arbeidserfaring String
    | BrukerHopperOverArbeidserfaring
    | HentAAregArbeidserfaring
    | HentetAAregArbeidserfaring (Result Http.Error (List Arbeidserfaring))
    | BrukerOppdatererYrke String
    | BrukerTrykkerTypeaheadTast Typeahead.Operation
    | BrukerHovrerOverTypeaheadSuggestion Yrke
    | BrukerVelgerYrke Yrke
    | RegistrerYrkeTypeaheadFikkFokus
    | RegistrerYrkeTypeaheadMisterFokus
    | BrukerVilRegistrereYrke
    | HentetYrkeTypeahead (Result Http.Error (List Yrke))
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
    | YrkeRedigeringsfeltEndret String
    | BrukerTrykkerTypeaheadTastIOppsummering Typeahead.Operation
    | BrukerHovrerOverTypeaheadSuggestionIOppsummering Yrke
    | BrukerVelgerYrkeIOppsummering Yrke
    | SkjemaEndret SkjemaEndring
    | BrukerVilLagreArbeidserfaringIOppsummering
    | BrukerVilLagreArbeidserfaringSkjema
    | ArbeidserfaringLagret (Result Http.Error (List Arbeidserfaring))
    | NyArbeidserfaring
    | FerdigMedArbeidserfaring String
    | StartÅSkrive
    | FullFørMelding
    | ViewportSatt (Result Dom.Error ())
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


type Samtale
    = Intro
    | VelgEnArbeidserfaringÅRedigere
    | HenterFraAareg
    | HentetFraAAreg
    | IkkeHentetFraAAreg
    | IngenArbeidserfaringFraAareg (List Arbeidserfaring)
    | VisArbeidserfaringFraAareg (List Arbeidserfaring)
    | RegistrerYrke RegistrerYrkeInfo (TypeaheadState Yrke)
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
    | RedigerOppsummering ArbeidserfaringSkjema
    | LagrerArbeidserfaring ValidertArbeidserfaringSkjema
    | LagringFeilet Http.Error ValidertArbeidserfaringSkjema
    | SpørOmBrukerVilLeggeInnMer
    | StartNyArbeidserfaring (TypeaheadState Yrke) -- Denne brukes kun for å få en annen melding fra Cvert i meldingsloggen, men hopper over til RegistrerYrke etter det
    | VenterPåAnimasjonFørFullføring String


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
    ArbeidserfaringSkjema.initValidertSkjema
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
    ArbeidserfaringSkjema.initValidertSkjema
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
            ( VenterPåAnimasjonFørFullføring "Ok, da går vi videre. Du kan alltid komme tilbake og legge til om du kommer på noe!"
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
                |> ArbeidserfaringSkjema.fraArbeidserfaring
                |> RedigerOppsummering
                |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        HentAAregArbeidserfaring ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (Melding.svar [ "Ja, jeg har arbeidserfaring" ])
                    , aktivSamtale = HenterFraAareg
                }
            , Api.getAAreg HentetAAregArbeidserfaring
            )
                |> IkkeFerdig

        HentetAAregArbeidserfaring result ->
            case result of
                Ok arbeidserfaringFraAAreg ->
                    ( model
                        |> visAaregResultat arbeidserfaringFraAAreg
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Err error ->
                    ( IkkeHentetFraAAreg
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, jeg har arbeidserfaring" ])
                    , logFeilmelding error "Hente fra Aareg"
                    )
                        |> IkkeFerdig

        BrukerVilLeggeTilNyArbeidserfaring knappeTekst ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerYrke { valgtYrke = Nothing, feilmelding = Nothing }
                |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerOppdatererYrke string ->
            case model.aktivSamtale of
                RegistrerYrke info typeaheadState ->
                    updateEtterOppdatertYrke model string typeaheadState info

                StartNyArbeidserfaring typeaheadState ->
                    updateEtterOppdatertYrke model string typeaheadState { valgtYrke = Nothing, feilmelding = Nothing }

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        RegistrerYrkeTypeaheadFikkFokus ->
            case model.aktivSamtale of
                RegistrerYrke info typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.showSuggestions
                                    |> RegistrerYrke info
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        RegistrerYrkeTypeaheadMisterFokus ->
            case model.aktivSamtale of
                RegistrerYrke info typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.hideSuggestions
                                    |> RegistrerYrke info
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereYrke ->
            case model.aktivSamtale of
                RegistrerYrke info typeaheadState ->
                    case info.valgtYrke of
                        Just yrke ->
                            brukerVelgerYrke model yrke

                        Nothing ->
                            ( RegistrerYrke { info | feilmelding = Just "Velg et yrke fra listen med forslag som kommer opp" } typeaheadState
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetYrkeTypeahead result ->
            case model.aktivSamtale of
                RegistrerYrke info typeaheadState ->
                    case result of
                        Ok suggestions ->
                            ( let
                                nyTypeaheadState =
                                    TypeaheadState.updateSuggestions "" suggestions typeaheadState
                              in
                              nyTypeaheadState
                                |> RegistrerYrke (registrerYrkeInfoEtterUpdate nyTypeaheadState info)
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente Yrketypeahed" )
                                |> IkkeFerdig

                RedigerOppsummering skjema ->
                    case result of
                        Ok suggestions ->
                            ( TypeaheadState.updateSuggestions "" suggestions
                                |> ArbeidserfaringSkjema.mapTypeaheadState skjema
                                |> RedigerOppsummering
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente Yrketypeahed" )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerHovrerOverTypeaheadSuggestion yrkeTypeahead ->
            case model.aktivSamtale of
                RegistrerYrke info typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateActive yrkeTypeahead
                                    |> RegistrerYrke info
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykkerTypeaheadTast operation ->
            case model.aktivSamtale of
                RegistrerYrke info typeaheadState ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.arrowUp
                                            |> RegistrerYrke info
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.ArrowDown ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.arrowDown
                                            |> RegistrerYrke info
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            case TypeaheadState.getActive typeaheadState of
                                Just active ->
                                    brukerVelgerYrke model active

                                Nothing ->
                                    ( Model
                                        { model
                                            | aktivSamtale = RegistrerYrke info typeaheadState
                                        }
                                    , Cmd.none
                                    )
                                        |> IkkeFerdig

                        Typeahead.MouseLeaveSuggestions ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.removeActive
                                            |> RegistrerYrke info
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVelgerYrke yrke ->
            case model.aktivSamtale of
                RegistrerYrke info typeaheadState ->
                    ( typeaheadState
                        |> TypeaheadState.hideSuggestions
                        |> TypeaheadState.updateValue (Yrke.label yrke)
                        |> RegistrerYrke { valgtYrke = Just yrke, feilmelding = Nothing }
                        |> oppdaterSamtalesteg model
                    , Api.getYrkeTypeahead HentetYrkeTypeahead (Yrke.label yrke)
                    )
                        |> IkkeFerdig

                RedigerOppsummering _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilEndreJobbtittel jobbtittelInfo ->
            ( EndreJobbtittel jobbtittelInfo
                |> nesteSamtaleSteg model (Melding.svar [ "Nei, legg til et nytt navn" ])
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
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, det stemmer" ])
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
                    ( arbeidsOppgaveInfo
                        |> arbeidsoppgaverInfoTilfraDatoInfo
                        |> RegistrereFraMåned
                        |> nesteSamtaleSteg model (Melding.svar [ arbeidsOppgaveInfo.arbeidsoppgaver ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

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
                        |> oppdaterSamtalesteg model
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
                        |> oppdaterSamtalesteg model
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
                                |> oppdaterSamtalesteg model
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
                        |> oppdaterSamtalesteg model
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
                        |> oppdaterSamtalesteg model
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
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRedigereOppsummering ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    ( skjema
                        |> ArbeidserfaringSkjema.tilUvalidertSkjema
                        |> RedigerOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        YrkeRedigeringsfeltEndret string ->
            case model.aktivSamtale of
                RedigerOppsummering skjema ->
                    ( Model
                        { model
                            | aktivSamtale =
                                ArbeidserfaringSkjema.oppdaterYrkeFelt skjema string
                                    |> RedigerOppsummering
                        }
                    , Api.getYrkeTypeahead HentetYrkeTypeahead string
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykkerTypeaheadTastIOppsummering operation ->
            case model.aktivSamtale of
                RedigerOppsummering skjema ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        TypeaheadState.arrowUp
                                            |> ArbeidserfaringSkjema.mapTypeaheadState skjema
                                            |> RedigerOppsummering
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.ArrowDown ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        TypeaheadState.arrowDown
                                            |> ArbeidserfaringSkjema.mapTypeaheadState skjema
                                            |> RedigerOppsummering
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        skjema
                                            |> ArbeidserfaringSkjema.velgAktivYrkeITypeahead
                                            |> RedigerOppsummering
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.MouseLeaveSuggestions ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        TypeaheadState.removeActive
                                            |> ArbeidserfaringSkjema.mapTypeaheadState skjema
                                            |> RedigerOppsummering
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerHovrerOverTypeaheadSuggestionIOppsummering _ ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        BrukerVelgerYrkeIOppsummering yrke ->
            case model.aktivSamtale of
                RedigerOppsummering skjema ->
                    ( Model
                        { model
                            | aktivSamtale =
                                skjema
                                    |> ArbeidserfaringSkjema.setYrkeFeltTilYrke yrke
                                    |> RedigerOppsummering
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                RedigerOppsummering arbeidserfaringSkjema ->
                    ( arbeidserfaringSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> RedigerOppsummering
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilLagreArbeidserfaringIOppsummering ->
            case model.aktivSamtale of
                VisOppsummering validertSkjema ->
                    ( validertSkjema
                        |> LagrerArbeidserfaring
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, informasjonen er riktig" ])
                    , Cmd.batch
                        [ postEllerPutArbeidserfaring ArbeidserfaringLagret validertSkjema
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilLagreArbeidserfaringSkjema ->
            case model.aktivSamtale of
                RedigerOppsummering skjema ->
                    case ArbeidserfaringSkjema.valider skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> LagrerArbeidserfaring
                                |> nesteSamtaleSteg model (Melding.svar (validertSkjemaTilSetninger validertSkjema))
                            , Cmd.batch
                                [ postEllerPutArbeidserfaring ArbeidserfaringLagret validertSkjema
                                , lagtTilSpørsmålCmd model.debugStatus
                                ]
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> ArbeidserfaringSkjema.gjørAlleFeilmeldingerSynlig
                                |> RedigerOppsummering
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        ArbeidserfaringLagret result ->
            case model.aktivSamtale of
                LagrerArbeidserfaring arbeidserfaringSkjema ->
                    case result of
                        Ok arbeidserfaringer ->
                            ( oppdaterSamtalesteg { model | arbeidserfaringListe = arbeidserfaringer } SpørOmBrukerVilLeggeInnMer
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( LagringFeilet error arbeidserfaringSkjema
                                |> nesteSamtaleSteg model (Melding.spørsmål [ "Noe gikk galt med lagringen" ])
                            , arbeidserfaringSkjema
                                |> ArbeidserfaringSkjema.encode
                                |> Api.logErrorWithRequestBody ErrorLogget "Lagre arbeidserfaring" error
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        NyArbeidserfaring ->
            ( ""
                |> TypeaheadState.init
                |> StartNyArbeidserfaring
                |> nesteSamtaleSteg model (Melding.svar [ "Ja, legg til en arbeidserfaring" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        StartÅSkrive ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        MeldingsLogg.startÅSkrive model.seksjonsMeldingsLogg
                }
            , Cmd.batch
                [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                , (MeldingsLogg.nesteMeldingToString model.seksjonsMeldingsLogg * 1000.0)
                    |> DebugStatus.meldingsTimeout model.debugStatus
                    |> Process.sleep
                    |> Task.perform (always FullFørMelding)
                ]
            )
                |> IkkeFerdig

        FullFørMelding ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.fullførMelding
                |> updateEtterFullførtMelding model

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )

        ViewportSatt _ ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        FerdigMedArbeidserfaring knappeTekst ->
            if List.isEmpty model.arbeidserfaringListe then
                ( VenterPåAnimasjonFørFullføring "Ok, da går vi videre. Du kan alltid komme tilbake og legge til om du kommer på noe!"
                    |> nesteSamtaleSteg model
                        (Melding.svar [ knappeTekst ])
                , lagtTilSpørsmålCmd model.debugStatus
                )
                    |> IkkeFerdig

            else
                ( VenterPåAnimasjonFørFullføring "Bra innsats! 😊 Nå kan arbeidsgivere finne deg hvis du har den erfaringen de ser etter."
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


updateEtterOppdatertYrke : ModelInfo -> String -> TypeaheadState Yrke -> RegistrerYrkeInfo -> SamtaleStatus
updateEtterOppdatertYrke model string typeaheadState info =
    ( let
        nyTypeaheadState =
            TypeaheadState.updateValue string typeaheadState
      in
      nyTypeaheadState
        |> RegistrerYrke (registrerYrkeInfoEtterUpdate nyTypeaheadState info)
        |> oppdaterSamtalesteg model
    , Api.getYrkeTypeahead HentetYrkeTypeahead string
    )
        |> IkkeFerdig


registrerYrkeInfoEtterUpdate : TypeaheadState Yrke -> RegistrerYrkeInfo -> RegistrerYrkeInfo
registrerYrkeInfoEtterUpdate typeaheadState info =
    case TypeaheadState.findSuggestionMatchingInputValue Yrke.label typeaheadState of
        Just yrke ->
            { feilmelding = Nothing
            , valgtYrke =
                case info.valgtYrke of
                    Just alleredeValgt ->
                        Just alleredeValgt

                    Nothing ->
                        Just yrke
            }

        Nothing ->
            { info | valgtYrke = Nothing }


oppdaterSkjema : SkjemaEndring -> ArbeidserfaringSkjema -> ArbeidserfaringSkjema
oppdaterSkjema endring skjema =
    case endring of
        Tekst felt string ->
            ArbeidserfaringSkjema.oppdaterStringFelt felt string skjema

        NåværendeToggled ->
            ArbeidserfaringSkjema.toggleNåværende skjema

        FraMåned månedString ->
            månedString
                |> Dato.stringTilMåned
                |> ArbeidserfaringSkjema.oppdaterFraMåned skjema

        TilMåned månedString ->
            månedString
                |> Dato.stringTilMåned
                |> ArbeidserfaringSkjema.oppdaterTilMåned skjema

        FraÅrBlurred ->
            ArbeidserfaringSkjema.gjørFeilmeldingFraÅrSynlig skjema

        TilÅrBlurred ->
            ArbeidserfaringSkjema.gjørFeilmeldingTilÅrSynlig skjema


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding info nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        MeldingsLogg.FerdigAnimert ferdigAnimertSamtale ->
            case info.aktivSamtale of
                VenterPåAnimasjonFørFullføring _ ->
                    Ferdig ferdigAnimertSamtale

                _ ->
                    ( Model { info | seksjonsMeldingsLogg = nyMeldingsLogg }
                    , Cmd.batch
                        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        , settFokus info.aktivSamtale
                        ]
                    )
                        |> IkkeFerdig

        MeldingsLogg.MeldingerGjenstår ->
            ( Model
                { info
                    | seksjonsMeldingsLogg = nyMeldingsLogg
                }
            , lagtTilSpørsmålCmd info.debugStatus
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
    inputId
        |> inputIdTilString
        |> Dom.focus
        |> Task.attempt FokusSatt


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    Cmd.batch
        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
        , 200
            |> DebugStatus.meldingsTimeout debugStatus
            |> Process.sleep
            |> Task.perform (always StartÅSkrive)
        ]


brukerVelgerYrke : ModelInfo -> Yrke -> SamtaleStatus
brukerVelgerYrke info yrkesTypeahead =
    ( yrkesTypeahead
        |> yrkeInfoTilJobbtittelInfo
        |> SpørOmBrukerVilEndreJobbtittel
        |> nesteSamtaleSteg info (Melding.svar [ Yrke.label yrkesTypeahead ])
    , lagtTilSpørsmålCmd info.debugStatus
    )
        |> IkkeFerdig


visAaregResultat : List Arbeidserfaring -> ModelInfo -> Model
visAaregResultat list info =
    if List.isEmpty list then
        RegistrerYrke { valgtYrke = Nothing, feilmelding = Nothing } (TypeaheadState.init "")
            |> oppdaterSamtalesteg info

    else
        list
            ++ info.arbeidserfaringListe
            |> VisArbeidserfaringFraAareg
            |> oppdaterSamtalesteg info


oppdaterSamtalesteg : ModelInfo -> Samtale -> Model
oppdaterSamtalesteg modelInfo samtaleSeksjon =
    case samtaleSeksjon of
        IngenArbeidserfaringFraAareg list ->
            Model
                { modelInfo
                    | aktivSamtale = samtaleSeksjon
                    , arbeidserfaringListe = list
                    , seksjonsMeldingsLogg =
                        modelInfo.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
                }

        VisArbeidserfaringFraAareg list ->
            Model
                { modelInfo
                    | aktivSamtale = samtaleSeksjon
                    , arbeidserfaringListe = list
                    , seksjonsMeldingsLogg =
                        modelInfo.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
                }

        SpørOmBrukerVilLeggeInnMer ->
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
            [ Melding.spørsmål [ "Hvilken registrerte arbeidserfaring ønsker du å redigere?" ] ]

        HenterFraAareg ->
            -- TODO: fiks dette etter at vi har skjekket om det funker
            []

        HentetFraAAreg ->
            [ Melding.spørsmål
                [ "Da har vi hentet arbeidserfaringen fra Aa-registeret."
                , "Informasjonen er registrert av arbeidsgiverene dine. "
                ]
            , Melding.spørsmål
                [ "Dessverre finnes det bare informasjon om arbeidsforhold etter 2015."
                , "Jeg håper det kan hjelpe deg litt "
                ]
            ]

        IkkeHentetFraAAreg ->
            [ Melding.spørsmål
                [ "Normalt ville jeg hentet informasjon om arbeidserfaringen dine fra Aa-reg."
                , "Dessverre fikk jeg ikke kontakt med dem nå, så vi må fylle ut arbeidserfaringen selv."
                ]
            , Melding.spørsmål [ "Da må vi registrere arbeidserfaringen selv, men det går heldigvis ganske kjapt!" ]
            , Melding.spørsmål
                [ "Vi begynner med å registrere yrke,"
                , "det er viktig at du velger et yrke fra listen."
                , "Hvis ikke navnet passer helt, så kan du endre det senere."
                ]
            ]

        IngenArbeidserfaringFraAareg _ ->
            [ Melding.spørsmål
                [ "Normalt ville jeg hentet informasjon om arbeidserfaringen dine fra Aa-reg."
                , "Dessverre ser det ut til at du ikke har noen registrerte jobber der."
                ]
            , Melding.spørsmål [ "Da må vi registrere arbeidserfaringen selv, men det går heldigvis ganske kjapt!" ]
            , Melding.spørsmål
                [ "Vi begynner med å registrere yrke,"
                , "det er viktig at du velger et yrke fra listen."
                , "Hvis ikke navnet passer helt, så kan du endre det senere."
                ]
            ]

        VisArbeidserfaringFraAareg _ ->
            [ Melding.spørsmål [ "" ] ]

        RegistrerYrke _ _ ->
            [ Melding.spørsmål [ "Nå skal du legge inn arbeidserfaring. La oss begynne med det siste arbeidsforholdet." ]
            , Melding.spørsmål [ "Først må du velge et yrke. Begynn å skriv, velg fra listen med forslag som kommer opp." ]
            , Melding.spørsmål [ "Du må velge et av forslagene, da kan arbeidsgivere finne deg når de søker etter folk." ]
            ]

        SpørOmBrukerVilEndreJobbtittel _ ->
            [ Melding.spørsmål
                [ "Stemte yrket du la inn, eller ønsker du å gi det et nytt navn?"
                , "Navnet du skriver vil vises på CV-en din"
                ]
            ]

        EndreJobbtittel _ ->
            []

        RegistrereBedriftsnavn _ ->
            [ Melding.spørsmål [ "Hvilken bedrift jobber eller jobbet du i?" ] ]

        RegistrereSted _ ->
            [ Melding.spørsmål [ "Hvor holder bedriften til?" ] ]

        RegistrereArbeidsoppgaver _ ->
            [ Melding.spørsmål [ "Fortell om hvilke arbeidsoppgaver du har hatt, hva du har lært og hva som var rollen din." ] ]

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

        RedigerOppsummering _ ->
            [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

        LagrerArbeidserfaring _ ->
            []

        LagringFeilet _ _ ->
            []

        SpørOmBrukerVilLeggeInnMer ->
            [ Melding.spørsmål [ "Flott! Da har du lagt inn en arbeidserfaring." ]
            , Melding.spørsmål [ "Har du flere arbeidserfaringer du ønsker å legge inn?" ]
            ]

        StartNyArbeidserfaring _ ->
            [ Melding.spørsmål [ "Da begynner vi på nytt med å registrere yrke. Husk at du kan endre tittel som kommer på CVen senere" ] ]

        VenterPåAnimasjonFørFullføring string ->
            [ Melding.spørsmål [ string ] ]


validertSkjemaTilSetninger : ValidertArbeidserfaringSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    let
        skjema =
            ArbeidserfaringSkjema.tilUvalidertSkjema validertSkjema
    in
    [ datoRad validertSkjema
    , Melding.tomLinje
    , "Stilling/Yrke: " ++ hentStilling validertSkjema
    , "Bedriftnavn: " ++ ArbeidserfaringSkjema.innholdTekstFelt Bedriftsnavn skjema
    , "Sted: " ++ ArbeidserfaringSkjema.innholdTekstFelt Sted skjema
    , Melding.tomLinje
    , "Arbeidsoppgaver: "
    , ArbeidserfaringSkjema.innholdTekstFelt Arbeidsoppgaver skjema
    ]


hentStilling : ValidertArbeidserfaringSkjema -> String
hentStilling validertSkjema =
    let
        skjema =
            ArbeidserfaringSkjema.tilUvalidertSkjema validertSkjema
    in
    if ArbeidserfaringSkjema.innholdTekstFelt Jobbtittel skjema == "" then
        validertSkjema
            |> ArbeidserfaringSkjema.yrke
            |> Yrke.label

    else
        ArbeidserfaringSkjema.innholdTekstFelt Jobbtittel skjema


datoRad : ValidertArbeidserfaringSkjema -> String
datoRad skjema =
    Dato.periodeTilString
        ((ArbeidserfaringSkjema.tilUvalidertSkjema >> ArbeidserfaringSkjema.fraMåned) skjema)
        (ArbeidserfaringSkjema.fraÅrValidert skjema)
        (ArbeidserfaringSkjema.tilDatoValidert skjema)



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        MeldingsLogg.FerdigAnimert _ ->
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
                            , Knapp.knapp (BrukerVilRedigereArbeidserfaring "Jeg vil redigere det jeg har lagt inn") "Jeg vil redigere det jeg har lagt inn"
                                |> Knapp.toHtml
                            ]

                VelgEnArbeidserfaringÅRedigere ->
                    Containers.knapper Kolonne
                        (List.map lagArbeidserfaringKnapp model.arbeidserfaringListe)

                HenterFraAareg ->
                    div [] []

                HentetFraAAreg ->
                    div [] []

                IkkeHentetFraAAreg ->
                    div []
                        []

                IngenArbeidserfaringFraAareg _ ->
                    div [] []

                VisArbeidserfaringFraAareg _ ->
                    div [] []

                RegistrerYrke info typeaheadState ->
                    Containers.typeaheadMedGåVidereKnapp BrukerVilRegistrereYrke
                        [ viewTypeaheadRegistrerYrke info typeaheadState
                        ]

                SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
                    Containers.knapper Flytende
                        [ "Nei, legg til nytt navn"
                            |> Knapp.knapp (BrukerVilEndreJobbtittel jobbtittelInfo)
                            |> Knapp.toHtml
                        , "Ja, det stemmer"
                            |> Knapp.knapp BrukerVilIkkeEndreJobbtittel
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

                RedigerOppsummering skjema ->
                    Containers.skjema { lagreMsg = BrukerVilLagreArbeidserfaringSkjema, lagreKnappTekst = "Lagre endringer" }
                        [ case ArbeidserfaringSkjema.yrkeTypeahead skjema of
                            Yrke yrke ->
                                yrke
                                    |> Yrke.label
                                    |> Input.input { label = "Stilling/yrke", msg = YrkeRedigeringsfeltEndret }
                                    |> Input.toHtml

                            Typeahead typeaheadState ->
                                viewTypeaheadOppsummering typeaheadState
                        , if ArbeidserfaringSkjema.innholdTekstFelt Jobbtittel skjema == "" then
                            text ""

                          else
                            skjema
                                |> ArbeidserfaringSkjema.innholdTekstFelt Jobbtittel
                                |> Input.input { label = "Jobbtittel", msg = Tekst Jobbtittel >> SkjemaEndret }
                                |> Input.toHtml
                        , skjema
                            |> ArbeidserfaringSkjema.innholdTekstFelt Bedriftsnavn
                            |> Input.input { label = "Bedriftens navn", msg = Tekst Bedriftsnavn >> SkjemaEndret }
                            |> Input.toHtml
                        , skjema
                            |> ArbeidserfaringSkjema.innholdTekstFelt Sted
                            |> Input.input { label = "By, sted eller land", msg = Tekst Sted >> SkjemaEndret }
                            |> Input.toHtml
                        , skjema
                            |> ArbeidserfaringSkjema.innholdTekstFelt Arbeidsoppgaver
                            |> Textarea.textarea { label = "Arbeidsoppgaver", msg = Tekst Arbeidsoppgaver >> SkjemaEndret }
                            |> Textarea.toHtml
                        , div [ class "DatoInput-fra-til-rad" ]
                            [ DatoInput.datoInput
                                { label = "Fra"
                                , onMånedChange = FraMåned >> SkjemaEndret
                                , måned = ArbeidserfaringSkjema.fraMåned skjema
                                , onÅrChange = Tekst FraÅr >> SkjemaEndret
                                , år = ArbeidserfaringSkjema.innholdTekstFelt FraÅr skjema
                                }
                                |> DatoInput.withMaybeFeilmeldingÅr (ArbeidserfaringSkjema.feilmeldingFraÅr skjema)
                                |> DatoInput.withOnBlurÅr (SkjemaEndret FraÅrBlurred)
                                |> DatoInput.toHtml
                            , if not (ArbeidserfaringSkjema.nåværende skjema) then
                                DatoInput.datoInput
                                    { label = "Til"
                                    , onMånedChange = TilMåned >> SkjemaEndret
                                    , måned = ArbeidserfaringSkjema.tilMåned skjema
                                    , onÅrChange = Tekst TilÅr >> SkjemaEndret
                                    , år = ArbeidserfaringSkjema.innholdTekstFelt TilÅr skjema
                                    }
                                    |> DatoInput.withMaybeFeilmeldingÅr (ArbeidserfaringSkjema.feilmeldingTilÅr skjema)
                                    |> DatoInput.withOnBlurÅr (SkjemaEndret TilÅrBlurred)
                                    |> DatoInput.toHtml

                              else
                                text ""
                            ]
                        , skjema
                            |> ArbeidserfaringSkjema.nåværende
                            |> Checkbox.checkbox "Nåværende" (SkjemaEndret NåværendeToggled)
                            |> Checkbox.toHtml
                        ]

                LagrerArbeidserfaring _ ->
                    div [] []

                LagringFeilet _ _ ->
                    div [] []

                SpørOmBrukerVilLeggeInnMer ->
                    Containers.knapper Flytende
                        [ Knapp.knapp NyArbeidserfaring "Ja, legg til en arbeidserfaring"
                            |> Knapp.toHtml
                        , Knapp.knapp (FerdigMedArbeidserfaring "Nei, jeg har lagt inn alle") "Nei, jeg har lagt inn alle"
                            |> Knapp.toHtml
                        , Knapp.knapp (BrukerVilRedigereArbeidserfaring "Rediger arbeidserfaring") "Rediger arbeidserfaring"
                            |> Knapp.toHtml
                        ]

                StartNyArbeidserfaring typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ viewTypeaheadRegistrerYrke { valgtYrke = Nothing, feilmelding = Nothing } typeaheadState
                            ]
                        ]

                VenterPåAnimasjonFørFullføring _ ->
                    div [] []

        MeldingsLogg.MeldingerGjenstår ->
            div [] []


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


viewTypeaheadRegistrerYrke : RegistrerYrkeInfo -> TypeaheadState Yrke -> Html Msg
viewTypeaheadRegistrerYrke { feilmelding } typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Hvilken stilling/yrke har du?", onInput = BrukerOppdatererYrke, onTypeaheadChange = BrukerTrykkerTypeaheadTast, inputId = inputIdTilString YrkeTypeaheadId }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestionRegistrerYrke typeaheadState)
        |> Typeahead.withFeilmelding feilmelding
        |> Typeahead.withOnFocus RegistrerYrkeTypeaheadFikkFokus
        |> Typeahead.withOnBlur RegistrerYrkeTypeaheadMisterFokus
        |> Typeahead.toHtml


typeaheadStateSuggestionsTilViewSuggestionRegistrerYrke : TypeaheadState Yrke -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSuggestionRegistrerYrke typeaheadState =
    typeaheadState
        |> TypeaheadState.mapSuggestions
            (\activeState suggestion ->
                { innhold = Yrke.label suggestion
                , onClick = BrukerVelgerYrke suggestion
                , onActive = BrukerHovrerOverTypeaheadSuggestion suggestion
                , active =
                    case activeState of
                        TypeaheadState.Active ->
                            True

                        TypeaheadState.NotActive ->
                            False
                }
            )


viewTypeaheadOppsummering : TypeaheadState Yrke -> Html Msg
viewTypeaheadOppsummering typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Stilling/yrke", onInput = YrkeRedigeringsfeltEndret, onTypeaheadChange = BrukerTrykkerTypeaheadTastIOppsummering, inputId = inputIdTilString YrkeTypeaheadId }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestionOppsummering typeaheadState)
        |> Typeahead.toHtml


typeaheadStateSuggestionsTilViewSuggestionOppsummering : TypeaheadState Yrke -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSuggestionOppsummering typeaheadState =
    typeaheadState
        |> TypeaheadState.mapSuggestions
            (\activeState suggestion ->
                { innhold = Yrke.label suggestion
                , onClick = BrukerVelgerYrkeIOppsummering suggestion
                , onActive = BrukerHovrerOverTypeaheadSuggestionIOppsummering suggestion
                , active =
                    case activeState of
                        TypeaheadState.Active ->
                            True

                        TypeaheadState.NotActive ->
                            False
                }
            )


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


postEllerPutArbeidserfaring : (Result Error (List Arbeidserfaring) -> msg) -> ArbeidserfaringSkjema.ValidertArbeidserfaringSkjema -> Cmd msg
postEllerPutArbeidserfaring msgConstructor skjema =
    case ArbeidserfaringSkjema.id skjema of
        Just id ->
            Api.putArbeidserfaring msgConstructor skjema id

        Nothing ->
            Api.postArbeidserfaring msgConstructor skjema


arbeidserfaringerTilString : List Arbeidserfaring -> List String
arbeidserfaringerTilString arbeidserfaringer =
    arbeidserfaringer
        |> List.map arbeidserfaringTilString
        |> List.intersperse [ Melding.tomLinje ]
        |> List.concat


arbeidserfaringTilString : Arbeidserfaring -> List String
arbeidserfaringTilString arbeidserfaring =
    -- TODO : FIks dette? og Sjekk
    [ Dato.periodeTilString (Cv.Arbeidserfaring.fraMåned arbeidserfaring) (Cv.Arbeidserfaring.fraÅr arbeidserfaring) (Cv.Arbeidserfaring.tilDato arbeidserfaring)
    , ((if Cv.Arbeidserfaring.yrkeFritekst arbeidserfaring == Nothing then
            Cv.Arbeidserfaring.yrkeString arbeidserfaring

        else
            Cv.Arbeidserfaring.yrkeFritekst arbeidserfaring
       )
        |> Maybe.withDefault ""
      )
        ++ " hos "
        ++ (Cv.Arbeidserfaring.arbeidsgiver arbeidserfaring |> Maybe.withDefault "")
    ]


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
                            , Melding.spørsmål (arbeidserfaringerTilString arbeidserfaringsListe)
                            , Melding.spørsmål [ "Vil du legge til mer?" ]
                            ]
                   )
        , arbeidserfaringListe = arbeidserfaringsListe
        , aktivSamtale = Intro
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )
