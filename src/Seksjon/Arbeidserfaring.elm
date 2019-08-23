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
import Dato exposing (Dato)
import DebugStatus exposing (DebugStatus)
import Feilmelding
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Input as Input
import FrontendModuler.InputInt as InputInt
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Select as Select
import FrontendModuler.Textarea as Textarea
import FrontendModuler.Typeahead as Typeahead
import Html exposing (Attribute, Html, button, div, input, label, option, select, text)
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error)
import List.Extra as List
import Melding exposing (Melding)
import MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Process
import SamtaleAnimasjon
import Skjema.ArbeidserfaringSkjema as ArbeidserfaringSkjema exposing (ArbeidserfaringSkjema, TypeaheadFelt(..), ValidertArbeidserfaringSkjema)
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


arbeidserfaringListe : Model -> List Arbeidserfaring
arbeidserfaringListe (Model info) =
    info.arbeidserfaringListe


aktivSamtale : Model -> Samtale
aktivSamtale (Model info) =
    info.aktivSamtale


hentAAregArbeidserfaring : Model -> Cmd Msg
hentAAregArbeidserfaring (Model info) =
    Api.getAAreg HentetAAregArbeidserfaring


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
    | BrukerHarValgtArbeidserfaringÅRedigere ArbeidserfaringSkjema String
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
    | ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.Felt String
    | ArbeidserfaringBoolSkjemaEndret ArbeidserfaringSkjema.Felt
    | BrukerTrykkerPåSlettArbeidserfaring
    | ArbeidserfaringSlettet (Result Http.Error (List Arbeidserfaring))
    | BrukerTrykkerPåLagreArbeidserfaringKnapp String ValidertArbeidserfaringSkjema
    | BrukerTrykkerPåLagreArbeidserfaringKnappMenSkjemaValidererIkke
    | ArbeidserfaringLagret (Result Http.Error (List Arbeidserfaring))
    | NyArbeidserfaring
    | FerdigMedArbeidserfaring String
    | StartÅSkrive
    | FullFørMelding
    | ViewportSatt (Result Dom.Error ())
    | GåTilNesteSeksjon
    | ErrorLogget (Result Http.Error ())


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
    | RegistrereNaavarende FraDatoInfo
    | RegistrereTilMåned TilDatoInfo
    | RegistrereTilÅr TilDatoInfo
    | VisOppsummering ValidertArbeidserfaringSkjema
    | RedigerOppsummering ArbeidserfaringSkjema
    | SletterArbeidserfaring ArbeidserfaringSkjema
    | FerdigMedÅSletteArbeidserfaring
    | LagreArbeidserfaring ValidertArbeidserfaringSkjema
    | LagringFeilet Http.Error ValidertArbeidserfaringSkjema
    | SpørOmBrukerVilLeggeInnMer
    | StartNyArbeidserfaring (TypeaheadState Yrke)
    | VenterPåAnimasjonFørFullføring String
    | HeltFerdig
    | HeltFerdigUtenArbeidsErfaring


type alias YrkeInfo =
    { yrke : String }


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
    , fraMåned : Dato.Måned
    , fraÅr : String
    , naavarende : Bool
    }


type alias TilDatoInfo =
    { tidligereInfo : FraDatoInfo
    , tilMåned : Dato.Måned
    , tilÅr : String
    }


type alias Oppsummering =
    { yrke : String
    , jobbTittel : String
    , bedriftNavn : String
    , lokasjon : String
    , arbeidsoppgaver : String
    , fraDato : Dato
    , naavarende : Bool
    , tilDato : Maybe Dato
    , styrkkode : String
    , konseptId : Int
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
    , fraMåned = Dato.Januar
    , fraÅr = ""
    , naavarende = False
    }


fraDatoInfoTilTilDatoInfo : FraDatoInfo -> TilDatoInfo
fraDatoInfoTilTilDatoInfo fraDatoInfo =
    { tidligereInfo = fraDatoInfo
    , tilMåned = Dato.Januar
    , tilÅr = ""
    }


tilDatoTilSkjema : TilDatoInfo -> ValidertArbeidserfaringSkjema
tilDatoTilSkjema tilDatoInfo =
    ArbeidserfaringSkjema.nyttValidertSkjema
        { yrke =
            tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo
        , jobbTittel = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.jobbtittel
        , bedriftNavn = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.bedriftNavn
        , lokasjon = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.lokasjon
        , arbeidsoppgaver = tilDatoInfo.tidligereInfo.tidligereInfo.arbeidsoppgaver
        , fraDato = Dato.fraStringTilDato (tilDatoInfo.tidligereInfo.fraÅr ++ "-" ++ (tilDatoInfo.tidligereInfo.fraMåned |> Dato.månedTilString))
        , naavarende = tilDatoInfo.tidligereInfo.naavarende
        , tilDato =
            if tilDatoInfo.tidligereInfo.naavarende then
                Nothing

            else
                Just (Dato.fraStringTilDato (tilDatoInfo.tilÅr ++ "-" ++ (tilDatoInfo.tilMåned |> Dato.månedTilString)))
        , styrkkode =
            tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo
                |> Yrke.styrkkode
        , konseptId =
            tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo
                |> Yrke.konseptId
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

        BrukerHarValgtArbeidserfaringÅRedigere skjema knappeTekst ->
            ( skjema
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

                RedigerOppsummering skjema ->
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
                    ( RegistrereNaavarende datoInfo
                        |> nesteSamtaleSteg model (Melding.svar [ datoInfo.fraÅr ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerSvarerJaTilNaavarende knappeTekst ->
            case model.aktivSamtale of
                RegistrereNaavarende datoInfo ->
                    ( datoInfo
                        |> fraDatoInfoTilTilDatoInfo
                        |> setNaavarendeTilTrue
                        |> tilDatoTilSkjema
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
                RegistrereNaavarende fraDatoInfo ->
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
                    ( tilDatoInfo
                        |> tilDatoTilSkjema
                        |> VisOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ tilDatoInfo.tilÅr ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRedigereOppsummering knappeTekst ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    ( skjema
                        |> ArbeidserfaringSkjema.tilArbeidserfaringSkjema
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

        BrukerHovrerOverTypeaheadSuggestionIOppsummering yrke ->
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

        ArbeidserfaringStringSkjemaEndret felt string ->
            case model.aktivSamtale of
                RedigerOppsummering arbeidserfaringSkjema ->
                    ( Model
                        { model
                            | aktivSamtale =
                                ArbeidserfaringSkjema.oppdaterStringFelt arbeidserfaringSkjema felt string
                                    |> RedigerOppsummering
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrereFraÅr fraDatoInfo ->
                    ( Model
                        { model
                            | aktivSamtale =
                                RegistrereFraÅr { fraDatoInfo | fraÅr = string }
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrereTilÅr tildDatoInfo ->
                    ( Model
                        { model
                            | aktivSamtale =
                                RegistrereTilÅr { tildDatoInfo | tilÅr = string }
                        }
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
                                ArbeidserfaringSkjema.toggleBool arbeidserfaringSkjema felt
                                    |> RedigerOppsummering
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykkerPåSlettArbeidserfaring ->
            case model.aktivSamtale of
                RedigerOppsummering skjema ->
                    ( skjema
                        |> SletterArbeidserfaring
                        |> nesteSamtaleSteg model (Melding.svar [ "Slett" ])
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd model.debugStatus
                        , skjema
                            |> ArbeidserfaringSkjema.id
                            |> Maybe.withDefault ""
                            |> Api.deleteArbeidserfaring ArbeidserfaringSlettet
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        ArbeidserfaringSlettet result ->
            case model.aktivSamtale of
                SletterArbeidserfaring skjema ->
                    case result of
                        Ok liste ->
                            ( FerdigMedÅSletteArbeidserfaring
                                |> oppdaterSamtalesteg { model | arbeidserfaringListe = liste }
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, Cmd.none )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykkerPåLagreArbeidserfaringKnapp brukerSvar validertSkjema ->
            case model.aktivSamtale of
                RedigerOppsummering skjema ->
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

                VisOppsummering skjema ->
                    ( skjema
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
                            ( SpørOmBrukerVilLeggeInnMer
                                |> oppdaterSamtalesteg { model | arbeidserfaringListe = arbeidserfaringer }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( LagringFeilet error arbeidserfaringSkjema
                                |> nesteSamtaleSteg model (Melding.spørsmål [ "Noe gikk galt med lagringen" ])
                            , Cmd.none
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

        ErrorLogget result ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        ViewportSatt result ->
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
                ( VenterPåAnimasjonFørFullføring "Kjempebra jobba! 👍 Nå kan en arbeidsgiver se om du har den erfaringen de leter etter."
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
                    ( Model model, Cmd.none )
                        |> IkkeFerdig


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding info nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        MeldingsLogg.FerdigAnimert ferdigAnimertSamtale ->
            case info.aktivSamtale of
                VenterPåAnimasjonFørFullføring _ ->
                    Ferdig ferdigAnimertSamtale

                _ ->
                    ( Model
                        { info
                            | seksjonsMeldingsLogg = nyMeldingsLogg
                        }
                    , SamtaleAnimasjon.scrollTilBunn ViewportSatt
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


fullførSeksjonHvisMeldingsloggErFerdig : ModelInfo -> SamtaleStatus
fullførSeksjonHvisMeldingsloggErFerdig modelInfo =
    case MeldingsLogg.ferdigAnimert modelInfo.seksjonsMeldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            Ferdig ferdigAnimertMeldingsLogg

        MeldingerGjenstår ->
            ( Model { modelInfo | aktivSamtale = VenterPåAnimasjonFørFullføring "" }, Cmd.none )
                |> IkkeFerdig


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


leggTilIArbeidserfaring : List Arbeidserfaring -> ModelInfo -> Samtale -> Model
leggTilIArbeidserfaring arbeidserfaringFraAareg modelInfo samtaleSeksjon =
    Model
        { modelInfo
            | aktivSamtale = samtaleSeksjon
            , arbeidserfaringListe = modelInfo.arbeidserfaringListe ++ arbeidserfaringFraAareg
        }


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

        FerdigMedÅSletteArbeidserfaring ->
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


setNaavarendeTilTrue : TilDatoInfo -> TilDatoInfo
setNaavarendeTilTrue info =
    { info | tidligereInfo = setTilTrue info.tidligereInfo }


setTilTrue : FraDatoInfo -> FraDatoInfo
setTilTrue fraDatoInfo =
    { fraDatoInfo | naavarende = True }


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

        IngenArbeidserfaringFraAareg liste ->
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

        VisArbeidserfaringFraAareg liste ->
            [ Melding.spørsmål [ "" ] ]

        RegistrerYrke yrkeInfo ->
            [ Melding.spørsmål [ "Nå skal du legge inn arbeidserfaring. La oss begynne med det siste arbeidsforholdet." ]
            , Melding.spørsmål [ "Først må du velge et yrke. Begynn å skriv, velg fra listen med forslag som kommer opp." ]
            , Melding.spørsmål [ "Du må velge et av forslagene, da kan arbeidsgivere finne deg når de søker etter folk." ]
            ]

        SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
            [ Melding.spørsmål
                [ "Stemte yrket du la inn, eller ønsker du å gi det et nytt navn?"
                , "Navnet du skriver vil vises på CV-en din"
                ]
            ]

        EndreJobbtittel jobbtittelInfo ->
            []

        RegistrereBedriftNavn beriftnavnsInfo ->
            [ Melding.spørsmål [ "Hvilken bedrift jobber eller jobbet du i?" ] ]

        RegistrereSted lokasjonInfo ->
            [ Melding.spørsmål [ "Hvor holder bedriften til?" ] ]

        RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
            [ Melding.spørsmål [ "Fortell om hvilke arbeidsoppgaver du har hatt, hva du har lært og hva som var rollen din." ] ]

        RegistrereFraMåned periodeInfo ->
            [ Melding.spørsmål [ "Hvilken måned begynte du i jobben?" ] ]

        RegistrereFraÅr periodeInfo ->
            [ Melding.spørsmål [ "Hvilket år begynte du i jobben?" ] ]

        RegistrereNaavarende periodeInfo ->
            [ Melding.spørsmål [ "Jobber du fremdeles i " ++ periodeInfo.tidligereInfo.tidligereInfo.tidligereInfo.bedriftNavn ++ "?" ] ]

        RegistrereTilMåned periodeInfo ->
            [ Melding.spørsmål [ "Hvilken måned sluttet du i jobben?" ] ]

        RegistrereTilÅr periodeInfo ->
            [ Melding.spørsmål [ "Hvilket år sluttet du i jobben?" ] ]

        VisOppsummering validertSkjema ->
            [ Melding.spørsmål
                [ hentFraDato (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema)
                    ++ " - "
                    ++ (if ArbeidserfaringSkjema.naavarende (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema) == True then
                            "nåværende"

                        else
                            hentTilDato (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema)
                       )
                , Melding.tomLinje
                , "Stilling/Yrke: " ++ hentStilling validertSkjema
                , "Bedriftnavn: " ++ ArbeidserfaringSkjema.bedriftNavn (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema)
                , "Sted: " ++ ArbeidserfaringSkjema.lokasjon (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema)
                , Melding.tomLinje
                , "Arbeidsoppgaver: "
                , ArbeidserfaringSkjema.arbeidsoppgaver (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema)
                , Melding.tomLinje
                , "Er informasjonen riktig?"
                ]
            ]

        RedigerOppsummering skjema ->
            [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

        LagreArbeidserfaring validertSkjema ->
            [ Melding.spørsmål [ "Flott! Da har du lagret en arbeidserfaring" ]
            , Melding.spørsmål
                [ "Stilling/Yrke " ++ hentStilling validertSkjema
                , "Bedriftnavn: " ++ ArbeidserfaringSkjema.bedriftNavn (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema)
                , "Sted: " ++ ArbeidserfaringSkjema.lokasjon (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema)
                , "Arbeidsoppgaver: " ++ ArbeidserfaringSkjema.arbeidsoppgaver (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema)
                , "Fra: " ++ hentFraDato (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema)
                , if ArbeidserfaringSkjema.naavarende (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema) == True then
                    "Nåværende jobb"

                  else
                    hentTilDato (ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema)
                ]
            , Melding.spørsmål [ "Har du flere arbeidserfaringer du ønsker å legge inn?" ]
            ]

        LagringFeilet error arbeidserfaringSkjema ->
            []

        SletterArbeidserfaring arbeidserfaringSkjema ->
            [ Melding.spørsmål [ "Sletter arbeidserfaring..." ] ]

        FerdigMedÅSletteArbeidserfaring ->
            [ Melding.spørsmål [ "Arbeidserfaring slettet!" ]
            , Melding.spørsmål [ "Vil du legge til flere?" ]
            ]

        SpørOmBrukerVilLeggeInnMer ->
            [ Melding.spørsmål [ "Har du flere arbeidserfaringer du ønsker å legge inn?" ] ]

        StartNyArbeidserfaring _ ->
            [ Melding.spørsmål [ "Da begynner vi på nytt med å registrere yrke. Husk at du kan endre tittel som kommer på CVen senere" ] ]

        VenterPåAnimasjonFørFullføring string ->
            [ Melding.spørsmål [ string ] ]

        HeltFerdig ->
            [ Melding.spørsmål [ "Kjempebra jobba!😊 Nå kan en arbeidsgiver se om du har den erfaringen de leter etter. " ] ]

        HeltFerdigUtenArbeidsErfaring ->
            [ Melding.spørsmål [ "Det var synd! Du kan alltid komme tilbake og legge til om du kommer på noe!" ] ]


hentStilling : ValidertArbeidserfaringSkjema -> String
hentStilling validertSkjema =
    let
        skjema =
            ArbeidserfaringSkjema.tilArbeidserfaringSkjema validertSkjema
    in
    if ArbeidserfaringSkjema.jobbTittel skjema == "" then
        validertSkjema
            |> ArbeidserfaringSkjema.yrke
            |> Yrke.label

    else
        ArbeidserfaringSkjema.jobbTittel skjema


hentFraDato : ArbeidserfaringSkjema -> String
hentFraDato skjema =
    let
        år =
            skjema |> ArbeidserfaringSkjema.fraDato |> Dato.år |> String.fromInt

        maaned =
            skjema |> ArbeidserfaringSkjema.fraDato |> Dato.måned |> Dato.månedTilString
    in
    maaned
        ++ " "
        ++ år


hentTilDato : ArbeidserfaringSkjema -> String
hentTilDato skjema =
    if ArbeidserfaringSkjema.naavarende skjema == True then
        ""

    else
        let
            dato =
                skjema
                    |> ArbeidserfaringSkjema.tilDato
                    |> Maybe.withDefault (ArbeidserfaringSkjema.fraDato skjema)
        in
        (dato |> Dato.måned |> Dato.månedTilString |> String.toLower) ++ " " ++ (dato |> Dato.år |> String.fromInt)



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model info) =
    case MeldingsLogg.ferdigAnimert info.seksjonsMeldingsLogg of
        MeldingsLogg.FerdigAnimert ferdigAnimertMeldingsLogg ->
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

                IngenArbeidserfaringFraAareg liste ->
                    div [] []

                VisArbeidserfaringFraAareg liste ->
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
                                |> Input.input
                                    { label = "Stilling/yrke som vil vises i CV-en", msg = BrukerOppdatererJobbtittelFelt }
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
                                |> Textarea.toHtml
                            , BrukerVilRegistrereFraMåned
                                |> lagTekstInputKnapp "Gå videre" arbeidsoppgaverInfo.arbeidsoppgaver
                            ]
                        ]

                RegistrereFraMåned fraDatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ div [ class "knapperad-wrapper" ]
                                    [ Dato.Januar
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.Februar
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.Mars
                                        |> lagFraMånedKnapp fraDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.April
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.Mai
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.Juni
                                        |> lagFraMånedKnapp fraDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.Juli
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.August
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.September
                                        |> lagFraMånedKnapp fraDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.Oktober
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.November
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.Desember
                                        |> lagFraMånedKnapp fraDatoInfo
                                    ]
                                ]
                            ]
                        ]

                RegistrereFraÅr fraDatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema-int" ]
                            [ div [ class "inputkolonne" ]
                                [ fraDatoInfo.fraÅr
                                    |> lagÅrInput ArbeidserfaringSkjema.FraÅr
                                , BrukerVilRegistrereNaavarende
                                    |> lagÅrInputKnapp "Gå videre" fraDatoInfo.fraÅr
                                ]
                            ]
                        ]

                RegistrereNaavarende fraDatoInfo ->
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

                RegistrereTilMåned tilDatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ div [ class "knapperad-wrapper" ]
                                    [ Dato.Januar
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.Februar
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.Mars
                                        |> lagTilMånedKnapp tilDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.April
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.Mai
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.Juni
                                        |> lagTilMånedKnapp tilDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.Juli
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.August
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.September
                                        |> lagTilMånedKnapp tilDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.Oktober
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.November
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.Desember
                                        |> lagTilMånedKnapp tilDatoInfo
                                    ]
                                ]
                            ]
                        ]

                RegistrereTilÅr tilDatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema-int" ]
                            [ tilDatoInfo.tilÅr
                                |> lagÅrInput ArbeidserfaringSkjema.TilÅr
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
                            , if
                                ArbeidserfaringSkjema.jobbTittel skjema
                                    == ""
                              then
                                div [] []

                              else
                                skjema
                                    |> ArbeidserfaringSkjema.jobbTittel
                                    |> Input.input { label = "Jobbtittel", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.JobbTittel }
                                    |> Input.toHtml
                            , skjema
                                |> ArbeidserfaringSkjema.bedriftNavn
                                |> Input.input { label = "Bedriftens navn", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.BedriftNavn }
                                |> Input.toHtml
                            , skjema
                                |> ArbeidserfaringSkjema.lokasjon
                                |> Input.input { label = "Sted/land", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.Lokasjon }
                                |> Input.toHtml
                            , skjema
                                |> ArbeidserfaringSkjema.arbeidsoppgaver
                                |> Textarea.textarea { label = "Arbeidsoppgaver", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.Arbeidsoppgaver }
                                |> Textarea.toHtml
                            , skjema
                                |> lagRedigerDatoInput
                            , div [ class "inputrad" ]
                                [ case ArbeidserfaringSkjema.valider skjema of
                                    Just validertSkjema ->
                                        "Utfør endringene"
                                            |> Knapp.knapp (BrukerTrykkerPåLagreArbeidserfaringKnapp "Utfør endringene" validertSkjema)
                                            |> Knapp.toHtml

                                    Nothing ->
                                        "Utfør endringene"
                                            |> Knapp.knapp BrukerTrykkerPåLagreArbeidserfaringKnappMenSkjemaValidererIkke
                                            |> Knapp.withEnabled Knapp.Disabled
                                            |> Knapp.toHtml
                                , "Slett"
                                    |> Knapp.knapp BrukerTrykkerPåSlettArbeidserfaring
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                LagreArbeidserfaring arbeidserfaringSkjema ->
                    div [] []

                LagringFeilet error arbeidserfaringSkjema ->
                    div [] []

                SletterArbeidserfaring arbeidserfaringSkjema ->
                    div [] []

                FerdigMedÅSletteArbeidserfaring ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "inputrad-innhold" ]
                            [ div [ class "inputrad" ]
                                [ Knapp.knapp
                                    NyArbeidserfaring
                                    "Ja, legg til en arbeidserfaring"
                                    |> Knapp.toHtml
                                ]
                            , div [ class "inputrad" ]
                                [ Knapp.knapp (FerdigMedArbeidserfaring "Nei, jeg har lagt inn alle") "Nei, jeg har lagt inn alle"
                                    |> Knapp.toHtml
                                ]
                            , if List.isEmpty info.arbeidserfaringListe then
                                div [] []

                              else
                                div [ class "inputrad" ]
                                    [ Knapp.knapp (BrukerVilRedigereArbeidserfaring "Rediger arbeidserfaring") "Rediger arbeidserfaring"
                                        |> Knapp.toHtml
                                    ]
                            ]
                        ]

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


viewTypeaheadRegistrerYrke : TypeaheadState Yrke -> Html Msg
viewTypeaheadRegistrerYrke typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Hvilken stilling/yrke har du?", onInput = BrukerOppdatererYrke, onTypeaheadChange = BrukerTrykkerTypeaheadTast }
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
            (\arbErf ->
                let
                    text =
                        Maybe.withDefault "" (Cv.Arbeidserfaring.yrke arbErf)
                            ++ ", "
                            ++ Maybe.withDefault "" (Cv.Arbeidserfaring.arbeidsgiver arbErf)
                in
                Knapp.knapp (BrukerHarValgtArbeidserfaringÅRedigere (arbeidserfaringTilSkjema arbErf) text) text
                    |> Knapp.toHtml
            )


arbeidserfaringTilSkjema : Cv.Arbeidserfaring.Arbeidserfaring -> ArbeidserfaringSkjema
arbeidserfaringTilSkjema arbeidserfaring =
    let
        yrk =
            Maybe.withDefault "" (Cv.Arbeidserfaring.yrke arbeidserfaring)

        styrkKode =
            Maybe.withDefault "" (Cv.Arbeidserfaring.yrke arbeidserfaring)

        konsept =
            Maybe.withDefault "" (Cv.Arbeidserfaring.konseptid arbeidserfaring)

        ferdigYrke =
            Yrke.fraString yrk styrkKode konsept
    in
    ArbeidserfaringSkjema.nyttValidertSkjema
        { yrke = ferdigYrke
        , jobbTittel = Maybe.withDefault "" (Cv.Arbeidserfaring.yrkeFritekst arbeidserfaring)
        , bedriftNavn = Maybe.withDefault "" (Cv.Arbeidserfaring.arbeidsgiver arbeidserfaring)
        , lokasjon = Maybe.withDefault "" (Cv.Arbeidserfaring.sted arbeidserfaring)
        , arbeidsoppgaver = Maybe.withDefault "" (Cv.Arbeidserfaring.beskrivelse arbeidserfaring)
        , fraDato =
            Maybe.withDefault "2007-09" (Cv.Arbeidserfaring.fradato arbeidserfaring)
                |> Dato.fraStringTilDato
        , naavarende = Cv.Arbeidserfaring.navarende arbeidserfaring
        , tilDato =
            if Cv.Arbeidserfaring.navarende arbeidserfaring then
                Nothing

            else
                Just
                    (Maybe.withDefault "1970-01" (Cv.Arbeidserfaring.tildato arbeidserfaring)
                        |> Dato.fraStringTilDato
                    )
        , styrkkode =
            Maybe.withDefault "" (Cv.Arbeidserfaring.styrkkode arbeidserfaring)
        , konseptId =
            Maybe.withDefault "1" (Cv.Arbeidserfaring.konseptid arbeidserfaring)
                |> String.toInt
                |> Maybe.withDefault 1
        , id = Just (Cv.Arbeidserfaring.id arbeidserfaring)
        }
        |> ArbeidserfaringSkjema.tilArbeidserfaringSkjema


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


lagFraMånedKnapp : FraDatoInfo -> Dato.Måned -> Html Msg
lagFraMånedKnapp fraDatoInfo måned =
    let
        msg =
            måned
                |> BrukerTrykketFraMånedKnapp
    in
    måned
        |> Dato.månedTilString
        |> Knapp.knapp msg
        |> Knapp.withClass Knapp.MånedKnapp
        |> Knapp.toHtml


lagTilMånedKnapp : TilDatoInfo -> Dato.Måned -> Html Msg
lagTilMånedKnapp tilDatoInfo måned =
    let
        msg =
            måned
                |> BrukerTrykketTilMånedKnapp
    in
    måned
        |> Dato.månedTilString
        |> Knapp.knapp msg
        |> Knapp.withClass Knapp.MånedKnapp
        |> Knapp.toHtml


lagÅrInput : ArbeidserfaringSkjema.Felt -> String -> Html Msg
lagÅrInput felt inputTekst =
    let
        inputfield =
            inputTekst
                |> Input.input { label = "År", msg = ArbeidserfaringStringSkjemaEndret felt }
                |> Input.withClass Input.År
    in
    if not (Dato.validerÅr inputTekst) && inputTekst /= "" then
        inputfield
            |> Input.withFeilmelding "Vennligst skriv inn et gyldig årstall"
            |> Input.toHtml

    else
        inputfield
            |> Input.toHtml


lagRedigerDatoInput : ArbeidserfaringSkjema -> Html Msg
lagRedigerDatoInput arbeidserfaringSkjema =
    div []
        [ Select.select "Måned"
            (ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.FraMåned)
            [ ( "Januar", "Januar" )
            , ( "Februar", "Februar" )
            , ( "Mars", "Mars" )
            , ( "April", "April" )
            , ( "Mai", "Mai" )
            , ( "Juni", "Juni" )
            , ( "Juli", "Juli" )
            , ( "August", "August" )
            , ( "September", "September" )
            , ( "Oktober", "Oktober" )
            , ( "November", "November" )
            , ( "Desember", "Desember" )
            ]
            |> Select.withSelected (arbeidserfaringSkjema |> ArbeidserfaringSkjema.fraDato |> Dato.måned |> Dato.månedTilString)
            |> Select.toHtml
        , arbeidserfaringSkjema
            |> ArbeidserfaringSkjema.fraDato
            |> Dato.år
            |> String.fromInt
            |> (\string ->
                    if string == "0" then
                        ""

                    else
                        string
               )
            |> lagÅrInput ArbeidserfaringSkjema.FraÅr

        -- |> Input.input { label = "År", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.FraÅr }
        -- |> Input.toHtml
        , arbeidserfaringSkjema
            |> ArbeidserfaringSkjema.naavarende
            |> Checkbox.checkbox "Nåværende" (ArbeidserfaringBoolSkjemaEndret ArbeidserfaringSkjema.Naavarende)
            |> Checkbox.toHtml
        , if ArbeidserfaringSkjema.naavarende arbeidserfaringSkjema then
            text ""

          else
            div []
                [ Select.select "Måned"
                    (ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.TilMåned)
                    [ ( "Januar", "Januar" )
                    , ( "Februar", "Februar" )
                    , ( "Mars", "Mars" )
                    , ( "April", "April" )
                    , ( "Mai", "Mai" )
                    , ( "Juni", "Juni" )
                    , ( "Juli", "Juli" )
                    , ( "August", "August" )
                    , ( "September", "September" )
                    , ( "Oktober", "Oktober" )
                    , ( "November", "November" )
                    , ( "Desember", "Desember" )
                    ]
                    |> Select.withSelected
                        (arbeidserfaringSkjema
                            |> ArbeidserfaringSkjema.tilDato
                            |> Maybe.withDefault (Dato.fraStringTilDato "1970-01")
                            |> Dato.måned
                            |> Dato.månedTilString
                        )
                    |> Select.toHtml
                , case ArbeidserfaringSkjema.tilDato arbeidserfaringSkjema of
                    Just dato ->
                        dato
                            |> Dato.år
                            |> String.fromInt
                            |> (\string ->
                                    if string == "0" then
                                        ""

                                    else
                                        string
                               )
                            |> lagÅrInput ArbeidserfaringSkjema.TilÅr

                    Nothing ->
                        arbeidserfaringSkjema
                            |> ArbeidserfaringSkjema.tilDato
                            |> Maybe.withDefault (Dato.fraStringTilDato "1970-01")
                            |> Dato.år
                            |> String.fromInt
                            |> (\string ->
                                    if string == "0" then
                                        ""

                                    else
                                        string
                               )
                            |> lagÅrInput ArbeidserfaringSkjema.TilÅr
                ]
        ]


postEllerPutArbeidserfaring : (Result Error (List Arbeidserfaring) -> msg) -> ArbeidserfaringSkjema.ValidertArbeidserfaringSkjema -> Cmd msg
postEllerPutArbeidserfaring msgConstructor skjema =
    case ArbeidserfaringSkjema.idValidert skjema of
        Just id ->
            Api.putArbeidserfaring msgConstructor skjema id

        Nothing ->
            Api.postArbeidserfaring msgConstructor skjema


arbeidserfaringToString : List Arbeidserfaring -> List String
arbeidserfaringToString arbeidserfaringsListe =
    List.map
        (\el ->
            case Cv.Arbeidserfaring.fradato el of
                Just fraDato ->
                    case Cv.Arbeidserfaring.tildato el of
                        Just tilDato ->
                            [ (fraDato
                                |> Dato.fraStringTilDato
                                |> Dato.måned
                                |> Dato.månedTilString
                              )
                                ++ " "
                                ++ (fraDato
                                        |> Dato.fraStringTilDato
                                        |> Dato.år
                                        |> String.fromInt
                                   )
                                ++ " - "
                                ++ (tilDato
                                        |> Dato.fraStringTilDato
                                        |> Dato.måned
                                        |> Dato.månedTilString
                                   )
                                ++ " "
                                ++ (tilDato
                                        |> Dato.fraStringTilDato
                                        |> Dato.år
                                        |> String.fromInt
                                   )
                            ]
                                ++ [ ((if Cv.Arbeidserfaring.yrkeFritekst el == Nothing then
                                        Cv.Arbeidserfaring.yrke el

                                       else
                                        Cv.Arbeidserfaring.yrkeFritekst el
                                      )
                                        |> Maybe.withDefault ""
                                     )
                                        ++ " hos "
                                        ++ (Cv.Arbeidserfaring.arbeidsgiver el |> Maybe.withDefault "")
                                   ]
                                ++ (if List.elemIndex el arbeidserfaringsListe == Just (List.length arbeidserfaringsListe - 1) then
                                        [ "" ]

                                    else
                                        [ "\u{00A0}" ]
                                   )

                        Nothing ->
                            [ (fraDato
                                |> Dato.fraStringTilDato
                                |> Dato.måned
                                |> Dato.månedTilString
                              )
                                ++ " "
                                ++ (fraDato
                                        |> Dato.fraStringTilDato
                                        |> Dato.år
                                        |> String.fromInt
                                   )
                                ++ " - Nåværende"
                            ]
                                ++ [ ((if Cv.Arbeidserfaring.yrkeFritekst el == Nothing then
                                        Cv.Arbeidserfaring.yrke el

                                       else
                                        Cv.Arbeidserfaring.yrkeFritekst el
                                      )
                                        |> Maybe.withDefault ""
                                     )
                                        ++ " hos "
                                        ++ (Cv.Arbeidserfaring.arbeidsgiver el |> Maybe.withDefault "")
                                   ]
                                ++ (if List.elemIndex el arbeidserfaringsListe == Just (List.length arbeidserfaringsListe - 1) then
                                        [ "" ]

                                    else
                                        [ "\u{00A0}" ]
                                   )

                Nothing ->
                    [ "" ]
        )
        arbeidserfaringsListe
        |> List.concat


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError ErrorLogget)
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
                            , Melding.spørsmål (arbeidserfaringToString arbeidserfaringsListe)
                            , Melding.spørsmål [ "Vil du legge til mer?" ]
                            ]
                   )
        , arbeidserfaringListe = arbeidserfaringsListe
        , aktivSamtale = Intro
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )
