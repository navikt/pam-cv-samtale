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
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
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
    = BrukerOppretterNyArbeidserfaring String
    | BrukerVilRedigereArbeidserfaring String
    | BrukerHarValgtArbeidserfaringÅRedigere Arbeidserfaring String
    | BrukerHopperOverArbeidserfaring String
    | HentAAregArbeidserfaring
    | HentetAAregArbeidserfaring (Result Http.Error (List Arbeidserfaring))
    | BrukerOppdatererYrke String
    | BrukerTrykkerTypeaheadTast Typeahead.Operation
    | BrukerHovrerOverTypeaheadSuggestion Yrke
    | BrukerVelgerYrke Yrke
    | HentetYrkeTypeahead (Result Http.Error (List Yrke))
    | BrukerVilEndreJobbtittel JobbtittelInfo
    | BrukerOppdatererJobbtittelFelt String
    | BrukerVilRegistrereBedriftnavn String
    | BrukerOppdatererBedriftnavn String
    | BrukerVilRegistrereSted
    | BrukerOppdatererSted String
    | BrukerVilRegistrereArbeidsoppgaver
    | BrukerOppdatererArbeidsoppgaver String
    | BrukerVilRegistrereFraMåned
    | BrukerTrykketFraMånedKnapp Dato.Måned
    | BrukerOppdatererFraÅr String
    | BrukerVilRegistrereNaavarende
    | BrukerSvarerJaTilNaavarende String
    | BrukerSvarerNeiTilNaavarende String
    | BrukerTrykketTilMånedKnapp Dato.Måned
    | BrukerOppdatererTilÅr String
    | BrukerVilGåTilOppsummering
    | BrukerVilRedigereOppsummering String
    | YrkeRedigeringsfeltEndret String
    | BrukerTrykkerTypeaheadTastIOppsummering Typeahead.Operation
    | BrukerHovrerOverTypeaheadSuggestionIOppsummering Yrke
    | BrukerVelgerYrkeIOppsummering Yrke
    | SkjemaEndret SkjemaEndring
    | ArbeidserfaringBoolSkjemaEndret ArbeidserfaringSkjema.Felt
    | BrukerTrykkerPåLagreArbeidserfaringKnapp String ValidertArbeidserfaringSkjema
    | BrukerTrykkerPåLagreArbeidserfaringKnappMenSkjemaValidererIkke
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


type Samtale
    = Intro
    | VelgEnArbeidserfaringÅRedigere
    | HenterFraAareg
    | HentetFraAAreg
    | IkkeHentetFraAAreg
    | IngenArbeidserfaringFraAareg (List Arbeidserfaring)
    | VisArbeidserfaringFraAareg (List Arbeidserfaring)
    | RegistrerYrke (TypeaheadState Yrke)
    | SpørOmBrukerVilEndreJobbtittel JobbtittelInfo
    | EndreJobbtittel JobbtittelInfo
    | RegistrereBedriftNavn BedriftnavnInfo
    | RegistrereSted StedInfo
    | RegistrereArbeidsoppgaver ArbeidsoppgaverInfo
    | RegistrereFraMåned FraDatoInfo
    | RegistrereFraÅr FraDatoInfo
    | RegistrereNåværende NåværendeInfo
    | RegistrereTilMåned TilDatoInfo
    | RegistrereTilÅr TilDatoInfo
    | VisOppsummering ValidertArbeidserfaringSkjema
    | RedigerOppsummering ArbeidserfaringSkjema
    | LagreArbeidserfaring ValidertArbeidserfaringSkjema
    | LagringFeilet Http.Error ValidertArbeidserfaringSkjema
    | SpørOmBrukerVilLeggeInnMer
    | StartNyArbeidserfaring (TypeaheadState Yrke)
    | VenterPåAnimasjonFørFullføring String
    | HeltFerdig
    | HeltFerdigUtenArbeidsErfaring


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
    }


fraDatoInfoTilTilDatoInfo : NåværendeInfo -> TilDatoInfo
fraDatoInfoTilTilDatoInfo nåværendeInfo =
    { tidligereInfo = nåværendeInfo
    , tilMåned = Januar
    , tilÅr = ""
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
        BrukerHopperOverArbeidserfaring knappeTekst ->
            ( VenterPåAnimasjonFørFullføring "Ok, da går vi videre. Du kan alltid komme tilbake og legge til om du kommer på noe!"
                |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
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

        BrukerOppretterNyArbeidserfaring knappeTekst ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerYrke
                |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerOppdatererYrke string ->
            case model.aktivSamtale of
                RegistrerYrke typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue string
                                    |> RegistrerYrke
                        }
                    , Cmd.batch
                        [ Api.getYrkeTypeahead HentetYrkeTypeahead string
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                StartNyArbeidserfaring typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue string
                                    |> RegistrerYrke
                        }
                    , Cmd.batch
                        [ Api.getYrkeTypeahead HentetYrkeTypeahead string
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        HentetYrkeTypeahead result ->
            case model.aktivSamtale of
                RegistrerYrke typeaheadState ->
                    case result of
                        Ok suggestions ->
                            ( typeaheadState
                                |> TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> RegistrerYrke
                                |> oppdaterSamtalesteg model
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente Yrketypeahed" )
                                |> IkkeFerdig

                RedigerOppsummering skjema ->
                    case result of
                        Ok suggestions ->
                            ( TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
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
                RegistrerYrke typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateActive yrkeTypeahead
                                    |> RegistrerYrke
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykkerTypeaheadTast operation ->
            case model.aktivSamtale of
                RegistrerYrke typeaheadState ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.arrowUp
                                            |> RegistrerYrke
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
                                            |> RegistrerYrke
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
                                            | aktivSamtale = RegistrerYrke typeaheadState
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
                                            |> RegistrerYrke
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVelgerYrke yrkesTypeahead ->
            case model.aktivSamtale of
                RegistrerYrke _ ->
                    brukerVelgerYrke model yrkesTypeahead

                RedigerOppsummering _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
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

        BrukerVilRegistrereBedriftnavn knappeTekst ->
            case model.aktivSamtale of
                EndreJobbtittel jobbtittelInfo ->
                    ( jobbtittelInfo
                        |> jobbtittelInfoTilBedriftnavnsInfo
                        |> RegistrereBedriftNavn
                        |> nesteSamtaleSteg model (Melding.svar [ jobbtittelInfo.jobbtittel ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
                    ( jobbtittelInfo
                        |> jobbtittelInfoTilBedriftnavnsInfo
                        |> RegistrereBedriftNavn
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        BrukerOppdatererBedriftnavn string ->
            case model.aktivSamtale of
                RegistrereBedriftNavn beriftnavnsInfo ->
                    ( Model
                        { model
                            | aktivSamtale = RegistrereBedriftNavn { beriftnavnsInfo | bedriftNavn = string }
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereSted ->
            case model.aktivSamtale of
                RegistrereBedriftNavn bedriftnavnInfo ->
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

        BrukerVilRegistrereArbeidsoppgaver ->
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

        BrukerVilRegistrereFraMåned ->
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
                    ( Model
                        { model
                            | aktivSamtale =
                                RegistrereFraÅr { fraDatoInfo | fraÅr = string }
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereNaavarende ->
            case model.aktivSamtale of
                RegistrereFraÅr datoInfo ->
                    case Dato.stringTilÅr datoInfo.fraÅr of
                        Just fraÅr ->
                            ( { tidligereInfo = datoInfo.tidligereInfo
                              , fraMåned = datoInfo.fraMåned
                              , fraÅr = fraÅr
                              }
                                |> RegistrereNåværende
                                |> nesteSamtaleSteg model (Melding.svar [ datoInfo.fraÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerSvarerJaTilNaavarende knappeTekst ->
            case model.aktivSamtale of
                RegistrereNåværende datoInfo ->
                    ( datoInfo
                        |> nåværendeInfoTilSkjema
                        |> VisOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerSvarerNeiTilNaavarende knappeTekst ->
            case model.aktivSamtale of
                RegistrereNåværende fraDatoInfo ->
                    ( fraDatoInfo
                        |> fraDatoInfoTilTilDatoInfo
                        |> RegistrereTilMåned
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
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
                    ( Model
                        { model
                            | aktivSamtale =
                                RegistrereTilÅr { tilDatoInfo | tilÅr = string }
                        }
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilGåTilOppsummering ->
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
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRedigereOppsummering knappeTekst ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    ( skjema
                        |> ArbeidserfaringSkjema.tilUvalidertSkjema
                        |> RedigerOppsummering
                        |> nesteSamtaleSteg model
                            (Melding.svar [ knappeTekst ])
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

        ArbeidserfaringBoolSkjemaEndret felt ->
            case model.aktivSamtale of
                RedigerOppsummering arbeidserfaringSkjema ->
                    ( Model
                        { model
                            | aktivSamtale =
                                arbeidserfaringSkjema
                                    |> ArbeidserfaringSkjema.toggleNåværende
                                    |> RedigerOppsummering
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykkerPåLagreArbeidserfaringKnapp brukerSvar validertSkjema ->
            case model.aktivSamtale of
                RedigerOppsummering _ ->
                    ( validertSkjema
                        |> LagreArbeidserfaring
                        |> nesteSamtaleSteg model (Melding.svar (validertSkjemaTilSetninger validertSkjema))
                    , Cmd.batch
                        [ validertSkjema
                            |> postEllerPutArbeidserfaring ArbeidserfaringLagret
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                VisOppsummering _ ->
                    ( validertSkjema
                        |> LagreArbeidserfaring
                        |> nesteSamtaleSteg model
                            (Melding.svar [ brukerSvar ])
                    , Cmd.batch
                        [ validertSkjema
                            |> postEllerPutArbeidserfaring ArbeidserfaringLagret
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykkerPåLagreArbeidserfaringKnappMenSkjemaValidererIkke ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        ArbeidserfaringLagret result ->
            case model.aktivSamtale of
                LagreArbeidserfaring arbeidserfaringSkjema ->
                    case result of
                        Ok arbeidserfaringer ->
                            ( oppdaterSamtalesteg { model | arbeidserfaringListe = arbeidserfaringer } SpørOmBrukerVilLeggeInnMer
                            , Cmd.none
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
        RegistrerYrke _ ->
            settFokusCmd YrkeTypeaheadId

        EndreJobbtittel _ ->
            settFokusCmd JobbtittelInput

        RegistrereBedriftNavn _ ->
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
        RegistrerYrke (TypeaheadState.init "")
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

        RegistrerYrke _ ->
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

        RegistrereBedriftNavn _ ->
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

        LagreArbeidserfaring _ ->
            [ Melding.spørsmål [ "Flott! Da har du lagt inn en arbeidserfaring." ]
            , Melding.spørsmål [ "Har du flere arbeidserfaringer du ønsker å legge inn?" ]
            ]

        LagringFeilet _ _ ->
            []

        SpørOmBrukerVilLeggeInnMer ->
            [ Melding.spørsmål [ "Har du flere arbeidserfaringer du ønsker å legge inn?" ] ]

        StartNyArbeidserfaring _ ->
            [ Melding.spørsmål [ "Da begynner vi på nytt med å registrere yrke. Husk at du kan endre tittel som kommer på CVen senere" ] ]

        VenterPåAnimasjonFørFullføring string ->
            [ Melding.spørsmål [ string ] ]

        HeltFerdig ->
            [ Melding.spørsmål [ "Bra innsats! 😊 Nå kan arbeidsgivere finne deg hvis du har den erfaringen de ser etter." ] ]

        HeltFerdigUtenArbeidsErfaring ->
            [ Melding.spørsmål [ "Det var synd! Du kan alltid komme tilbake og legge til om du kommer på noe!" ] ]


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
viewBrukerInput (Model info) =
    case MeldingsLogg.ferdigAnimert info.seksjonsMeldingsLogg of
        MeldingsLogg.FerdigAnimert _ ->
            case info.aktivSamtale of
                Intro ->
                    if List.isEmpty info.arbeidserfaringListe then
                        div [ class "skjema-wrapper" ]
                            [ div [ class "knapperad-wrapper" ]
                                [ div [ class "inputkolonne" ]
                                    [ "Ja, jeg har arbeidserfaring"
                                        |> Knapp.knapp (BrukerOppretterNyArbeidserfaring "Ja, jeg har arbeidserfaring")
                                        |> Knapp.withClass Knapp.LeggeTilUtdannelseKnapp
                                        |> Knapp.toHtml
                                    ]
                                , div [ class "inputkolonne" ]
                                    [ Knapp.knapp (FerdigMedArbeidserfaring "Nei, jeg har ikke arbeidserfaring") "Nei, jeg har ikke arbeidserfaring"
                                        |> Knapp.withClass Knapp.LeggeTilUtdannelseKnapp
                                        |> Knapp.toHtml
                                    ]
                                ]
                            ]

                    else
                        div [ class "skjema-wrapper" ]
                            [ div [ class "skjema" ]
                                [ div [ class "inputkolonne" ]
                                    [ "Ja, jeg vil legge til mer"
                                        |> Knapp.knapp (BrukerOppretterNyArbeidserfaring "Ja, jeg vil legge til mer")
                                        |> Knapp.toHtml
                                    , Knapp.knapp (BrukerHopperOverArbeidserfaring "Nei, jeg er ferdig") "Nei, jeg er ferdig"
                                        |> Knapp.toHtml
                                    , Knapp.knapp (BrukerVilRedigereArbeidserfaring "Jeg vil redigere det jeg har lagt inn") "Jeg vil redigere det jeg har lagt inn"
                                        |> Knapp.toHtml
                                    ]
                                ]
                            ]

                VelgEnArbeidserfaringÅRedigere ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            (Model info
                                |> lagArbeidserfaringKnapper
                                |> List.map
                                    (\msg ->
                                        div [ class "inputkolonne" ]
                                            [ msg
                                            ]
                                    )
                            )
                        ]

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

                RegistrerYrke typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ viewTypeaheadRegistrerYrke typeaheadState
                            ]
                        ]

                SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ div [ class "inputkolonne-innhold" ]
                                    [ (jobbtittelInfo
                                        |> BrukerVilEndreJobbtittel
                                        |> Knapp.knapp
                                      )
                                        "Nei, legg til nytt navn"
                                        |> Knapp.withClass Knapp.SpråknivåKnapp
                                        |> Knapp.toHtml
                                    , (BrukerVilRegistrereBedriftnavn "Ja, det stemmer"
                                        |> Knapp.knapp
                                      )
                                        "Ja, det stemmer"
                                        |> Knapp.withClass Knapp.SpråknivåKnapp
                                        |> Knapp.toHtml
                                    ]
                                ]
                            ]
                        ]

                EndreJobbtittel jobbtittelInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ jobbtittelInfo.jobbtittel
                                |> Input.input { label = "Stilling/yrke som vil vises i CV-en", msg = BrukerOppdatererJobbtittelFelt }
                                |> Input.withId (inputIdTilString JobbtittelInput)
                                |> Input.toHtml
                            , BrukerVilRegistrereBedriftnavn "Gå videre"
                                |> lagTekstInputKnapp "Gå videre" jobbtittelInfo.jobbtittel
                            ]
                        ]

                RegistrereBedriftNavn bedriftnanvsInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ bedriftnanvsInfo.bedriftNavn
                                |> Input.input { label = "Bedriftens navn", msg = BrukerOppdatererBedriftnavn }
                                |> Input.withId (inputIdTilString BedriftsnavnInput)
                                |> Input.toHtml
                            , BrukerVilRegistrereSted
                                |> lagTekstInputKnapp "Gå videre" bedriftnanvsInfo.bedriftNavn
                            ]
                        ]

                RegistrereSted stedInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ stedInfo.lokasjon
                                |> Input.input { label = "Sted/land", msg = BrukerOppdatererSted }
                                |> Input.withId (inputIdTilString StedInput)
                                |> Input.toHtml
                            , BrukerVilRegistrereArbeidsoppgaver
                                |> lagTekstInputKnapp "Gå videre" stedInfo.lokasjon
                            ]
                        ]

                RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ arbeidsoppgaverInfo.arbeidsoppgaver
                                |> Textarea.textarea { label = "Arbeidsoppgaver", msg = BrukerOppdatererArbeidsoppgaver }
                                |> Textarea.withId (inputIdTilString ArbeidsoppgaverInput)
                                |> Textarea.toHtml
                            , BrukerVilRegistrereFraMåned
                                |> lagTekstInputKnapp "Gå videre" arbeidsoppgaverInfo.arbeidsoppgaver
                            ]
                        ]

                RegistrereFraMåned _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ div [ class "knapperad-wrapper" ]
                                    [ lagFraMånedKnapp Dato.Januar
                                    , lagFraMånedKnapp Dato.Februar
                                    , lagFraMånedKnapp Dato.Mars
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagFraMånedKnapp Dato.April
                                    , lagFraMånedKnapp Dato.Mai
                                    , lagFraMånedKnapp Dato.Juni
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagFraMånedKnapp Dato.Juli
                                    , lagFraMånedKnapp Dato.August
                                    , lagFraMånedKnapp Dato.September
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagFraMånedKnapp Dato.Oktober
                                    , lagFraMånedKnapp Dato.November
                                    , lagFraMånedKnapp Dato.Desember
                                    ]
                                ]
                            ]
                        ]

                RegistrereFraÅr fraDatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema-int" ]
                            [ div [ class "inputkolonne" ]
                                [ fraDatoInfo.fraÅr
                                    |> Input.input { label = "År", msg = BrukerOppdatererFraÅr }
                                    |> Input.withClass Input.År
                                    |> Input.withId (inputIdTilString FraÅrInput)
                                    |> Input.toHtml
                                , BrukerVilRegistrereNaavarende
                                    |> lagÅrInputKnapp "Gå videre" fraDatoInfo.fraÅr
                                ]
                            ]
                        ]

                RegistrereNåværende _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            [ div [ class "inputrad" ]
                                [ "Ja"
                                    |> BrukerSvarerJaTilNaavarende
                                    |> lagMessageKnapp "Ja"
                                , "Nei"
                                    |> BrukerSvarerNeiTilNaavarende
                                    |> lagMessageKnapp "Nei"
                                ]
                            ]
                        ]

                RegistrereTilMåned _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ div [ class "knapperad-wrapper" ]
                                    [ lagTilMånedKnapp Dato.Januar
                                    , lagTilMånedKnapp Dato.Februar
                                    , lagTilMånedKnapp Dato.Mars
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagTilMånedKnapp Dato.April
                                    , lagTilMånedKnapp Dato.Mai
                                    , lagTilMånedKnapp Dato.Juni
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagTilMånedKnapp Dato.Juli
                                    , lagTilMånedKnapp Dato.August
                                    , lagTilMånedKnapp Dato.September
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagTilMånedKnapp Dato.Oktober
                                    , lagTilMånedKnapp Dato.November
                                    , lagTilMånedKnapp Dato.Desember
                                    ]
                                ]
                            ]
                        ]

                RegistrereTilÅr tilDatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema-int" ]
                            [ tilDatoInfo.tilÅr
                                |> Input.input { label = "År", msg = BrukerOppdatererTilÅr }
                                |> Input.withClass Input.År
                                |> Input.withId (inputIdTilString TilÅrInput)
                                |> Input.toHtml
                            , div [ class "inputkolonne" ]
                                [ BrukerVilGåTilOppsummering
                                    |> lagÅrInputKnapp "Gå videre" tilDatoInfo.tilÅr
                                ]
                            ]
                        ]

                VisOppsummering validertSkjema ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ BrukerTrykkerPåLagreArbeidserfaringKnapp "Ja, informasjonen er riktig" validertSkjema
                                    |> lagMessageKnapp "Ja, informasjonen er riktig"
                                , "Nei, jeg vil endre"
                                    |> BrukerVilRedigereOppsummering
                                    |> lagMessageKnapp "Nei, jeg vil endre"
                                ]
                            ]
                        ]

                RedigerOppsummering skjema ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
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
                                |> Input.input { label = "Sted/land", msg = Tekst Sted >> SkjemaEndret }
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
                                    --                                    |> DatoInput.withMaybeFeilmeldingÅr (ArbeidserfaringSkjema.feilmeldingFraÅr skjema)
                                    --                                    |> DatoInput.withOnBlurÅr (SkjemaEndret FraÅrBlurred)
                                    |> DatoInput.toHtml
                                , if not (ArbeidserfaringSkjema.nåværende skjema) then
                                    DatoInput.datoInput
                                        { label = "Til"
                                        , onMånedChange = TilMåned >> SkjemaEndret
                                        , måned = ArbeidserfaringSkjema.tilMåned skjema
                                        , onÅrChange = Tekst TilÅr >> SkjemaEndret
                                        , år = ArbeidserfaringSkjema.innholdTekstFelt TilÅr skjema
                                        }
                                        --                                        |> DatoInput.withMaybeFeilmeldingÅr (ArbeidserfaringSkjema.feilmeldingTilÅr skjema)
                                        --                                        |> DatoInput.withOnBlurÅr (SkjemaEndret TilÅrBlurred)
                                        |> DatoInput.toHtml

                                  else
                                    text ""
                                ]
                            , skjema
                                |> ArbeidserfaringSkjema.nåværende
                                |> Checkbox.checkbox "Nåværende" (SkjemaEndret NåværendeToggled)
                                |> Checkbox.toHtml
                            , div [ class "inputrad" ]
                                [ case ArbeidserfaringSkjema.valider skjema of
                                    Just validertSkjema ->
                                        "Lagre endringer"
                                            |> Knapp.knapp (BrukerTrykkerPåLagreArbeidserfaringKnapp "Lagre endringer" validertSkjema)
                                            |> Knapp.toHtml

                                    Nothing ->
                                        "Lagre endringer"
                                            |> Knapp.knapp BrukerTrykkerPåLagreArbeidserfaringKnappMenSkjemaValidererIkke
                                            |> Knapp.withEnabled Knapp.Disabled
                                            |> Knapp.toHtml
                                ]
                            ]
                        ]

                LagreArbeidserfaring _ ->
                    div [] []

                LagringFeilet _ _ ->
                    div [] []

                SpørOmBrukerVilLeggeInnMer ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp
                                    NyArbeidserfaring
                                    "Ja, legg til en arbeidserfaring"
                                    |> Knapp.toHtml
                                , Knapp.knapp (FerdigMedArbeidserfaring "Nei, jeg har lagt inn alle") "Nei, jeg har lagt inn alle"
                                    |> Knapp.toHtml
                                , Knapp.knapp (BrukerVilRedigereArbeidserfaring "Rediger arbeidserfaring") "Rediger arbeidserfaring"
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                StartNyArbeidserfaring typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ viewTypeaheadRegistrerYrke typeaheadState
                        ]

                VenterPåAnimasjonFørFullføring _ ->
                    div [] []

                HeltFerdig ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "inputkolonne" ]
                            [ Knapp.knapp GåTilNesteSeksjon "Gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

                HeltFerdigUtenArbeidsErfaring ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "inputkolonne" ]
                            [ Knapp.knapp GåTilNesteSeksjon "Gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

        MeldingsLogg.MeldingerGjenstår ->
            div [] []


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


viewTypeaheadRegistrerYrke : TypeaheadState Yrke -> Html Msg
viewTypeaheadRegistrerYrke typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Hvilken stilling/yrke har du?", onInput = BrukerOppdatererYrke, onTypeaheadChange = BrukerTrykkerTypeaheadTast }
        |> Typeahead.withInputId (inputIdTilString YrkeTypeaheadId)
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestionRegistrerYrke typeaheadState)
        |> Typeahead.toHtml


typeaheadStateSuggestionsTilViewSuggestionRegistrerYrke : TypeaheadState Yrke -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSuggestionRegistrerYrke typeaheadState =
    typeaheadState
        |> TypeaheadState.map
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
        |> Typeahead.typeahead { label = "Stilling/yrke", onInput = YrkeRedigeringsfeltEndret, onTypeaheadChange = BrukerTrykkerTypeaheadTastIOppsummering }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestionOppsummering typeaheadState)
        |> Typeahead.toHtml


typeaheadStateSuggestionsTilViewSuggestionOppsummering : TypeaheadState Yrke -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSuggestionOppsummering typeaheadState =
    typeaheadState
        |> TypeaheadState.map
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


lagArbeidserfaringKnapper : Model -> List (Html Msg)
lagArbeidserfaringKnapper (Model info) =
    info.arbeidserfaringListe
        |> List.map
            (\arbeidserfaring ->
                let
                    text =
                        Maybe.withDefault "" (Cv.Arbeidserfaring.yrkeString arbeidserfaring)
                            ++ ", "
                            ++ Maybe.withDefault "" (Cv.Arbeidserfaring.arbeidsgiver arbeidserfaring)
                in
                Knapp.knapp (BrukerHarValgtArbeidserfaringÅRedigere arbeidserfaring text) text
                    |> Knapp.toHtml
            )


lagTekstInputKnapp : String -> String -> Msg -> Html Msg
lagTekstInputKnapp knappeTekst inputTekst msg =
    Knapp.knapp msg knappeTekst
        |> (if inputTekst /= "" then
                Knapp.withEnabled Knapp.Enabled

            else
                Knapp.withEnabled Knapp.Disabled
           )
        |> Knapp.toHtml


lagÅrInputKnapp : String -> String -> Msg -> Html Msg
lagÅrInputKnapp knappeTekst inputTekst msg =
    Knapp.knapp msg knappeTekst
        |> (if inputTekst /= "" && Dato.validerÅr inputTekst then
                Knapp.withEnabled Knapp.Enabled

            else
                Knapp.withEnabled Knapp.Disabled
           )
        |> Knapp.toHtml


lagMessageKnapp : String -> Msg -> Html Msg
lagMessageKnapp knappeTekst msg =
    Knapp.knapp msg knappeTekst
        |> Knapp.withClass Knapp.UtdanningsNivåKnapp
        |> Knapp.toHtml


lagFraMånedKnapp : Dato.Måned -> Html Msg
lagFraMånedKnapp måned =
    måned
        |> Dato.månedTilString
        |> Knapp.knapp (BrukerTrykketFraMånedKnapp måned)
        |> Knapp.withClass Knapp.MånedKnapp
        |> Knapp.toHtml


lagTilMånedKnapp : Dato.Måned -> Html Msg
lagTilMånedKnapp måned =
    måned
        |> Dato.månedTilString
        |> Knapp.knapp (BrukerTrykketTilMånedKnapp måned)
        |> Knapp.withClass Knapp.MånedKnapp
        |> Knapp.toHtml



--
--lagÅrInput : ArbeidserfaringSkjema.Felt -> String -> Html Msg
--lagÅrInput felt inputTekst =
--    let
--        inputfield =
--            inputTekst
--                |> Input.input { label = "År", msg = SkjemaEndret felt }
--                |> Input.withClass Input.År
--    in
--    if not (Dato.validerÅr inputTekst) && inputTekst /= "" then
--        inputfield
--            |> Input.withFeilmelding "Vennligst skriv inn et gyldig årstall"
--            |> Input.toHtml
--
--    else
--        inputfield
--            |> Input.toHtml
--
--lagRedigerDatoInput : ArbeidserfaringSkjema -> Html Msg
--lagRedigerDatoInput arbeidserfaringSkjema =
--    div []
--        [ Select.select "Måned"
--            (SkjemaEndret ArbeidserfaringSkjema.FraMåned)
--            [ ( "Januar", "Januar" )
--            , ( "Februar", "Februar" )
--            , ( "Mars", "Mars" )
--            , ( "April", "April" )
--            , ( "Mai", "Mai" )
--            , ( "Juni", "Juni" )
--            , ( "Juli", "Juli" )
--            , ( "August", "August" )
--            , ( "September", "September" )
--            , ( "Oktober", "Oktober" )
--            , ( "November", "November" )
--            , ( "Desember", "Desember" )
--            ]
--            |> Select.withSelected (arbeidserfaringSkjema |> ArbeidserfaringSkjema.fraDato |> Dato.måned |> Dato.månedTilString)
--            |> Select.toHtml
--        , arbeidserfaringSkjema
--            |> ArbeidserfaringSkjema.fraDato
--            |> Dato.år
--            |> String.fromInt
--            |> (\string ->
--                    if string == "0" then
--                        ""
--
--                    else
--                        string
--               )
--            |> lagÅrInput ArbeidserfaringSkjema.FraÅr
--
--        -- |> Input.input { label = "År", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.FraÅr }
--        -- |> Input.toHtml
--        , arbeidserfaringSkjema
--            |> ArbeidserfaringSkjema.nåværende
--            |> Checkbox.checkbox "Nåværende" (ArbeidserfaringBoolSkjemaEndret ArbeidserfaringSkjema.Naavarende)
--            |> Checkbox.toHtml
--        , if ArbeidserfaringSkjema.nåværende arbeidserfaringSkjema then
--            text ""
--
--          else
--            div []
--                [ Select.select "Måned"
--                    (SkjemaEndret ArbeidserfaringSkjema.TilMåned)
--                    [ ( "Januar", "Januar" )
--                    , ( "Februar", "Februar" )
--                    , ( "Mars", "Mars" )
--                    , ( "April", "April" )
--                    , ( "Mai", "Mai" )
--                    , ( "Juni", "Juni" )
--                    , ( "Juli", "Juli" )
--                    , ( "August", "August" )
--                    , ( "September", "September" )
--                    , ( "Oktober", "Oktober" )
--                    , ( "November", "November" )
--                    , ( "Desember", "Desember" )
--                    ]
--                    |> Select.withSelected
--                        (arbeidserfaringSkjema
--                            |> ArbeidserfaringSkjema.tilDato
--                            |> Maybe.withDefault (Dato.fraStringTilDato "1970-01")
--                            |> Dato.måned
--                            |> Dato.månedTilString
--                        )
--                    |> Select.toHtml
--                , case ArbeidserfaringSkjema.tilDato arbeidserfaringSkjema of
--                    Just dato ->
--                        dato
--                            |> Dato.år
--                            |> String.fromInt
--                            |> (\string ->
--                                    if string == "0" then
--                                        ""
--
--                                    else
--                                        string
--                               )
--                            |> lagÅrInput ArbeidserfaringSkjema.TilÅr
--
--                    Nothing ->
--                        arbeidserfaringSkjema
--                            |> ArbeidserfaringSkjema.tilDato
--                            |> Maybe.withDefault (Dato.fraStringTilDato "1970-01")
--                            |> Dato.år
--                            |> String.fromInt
--                            |> (\string ->
--                                    if string == "0" then
--                                        ""
--
--                                    else
--                                        string
--                               )
--                            |> lagÅrInput ArbeidserfaringSkjema.TilÅr
--                ]
--        ]


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
