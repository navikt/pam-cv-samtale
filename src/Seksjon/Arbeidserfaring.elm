module Seksjon.Arbeidserfaring exposing (FraDatoInfo, Model, Msg, Oppsummering, SamtaleStatus(..), TilDatoInfo, init, meldingsLogg, update, viewBrukerInput)

import Api
import Cv.Arbeidserfaring exposing (Arbeidserfaring)
import Dato exposing (Dato)
import Feilmelding
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Select as Select
import FrontendModuler.Typeahead as Typeahead
import Html exposing (Attribute, Html, button, div, input, label, option, select, text)
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Melding exposing (Melding)
import MeldingsLogg exposing (MeldingsLogg)
import Skjema.ArbeidserfaringSkjema as ArbeidserfaringSkjema exposing (ArbeidserfaringSkjema)
import TypeaheadState exposing (TypeaheadState)
import YrkeTypeahead exposing (YrkeTypeahead(..))



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , arbeidserfaringListe : List Arbeidserfaring
    , aktivSamtale : Samtale
    }


arbeidserfaringListe : Model -> List Arbeidserfaring
arbeidserfaringListe (Model info) =
    info.arbeidserfaringListe


aktivSamtale : Model -> Samtale
aktivSamtale (Model info) =
    info.aktivSamtale


hentAAregArbeidserfaring : Model -> Cmd Msg
hentAAregArbeidserfaring (Model info) =
    Api.hentAAreg HentetAAregArbeidserfaring


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )



--- UPDATE ---


type Msg
    = BrukerOppretterNyArbeidserfaring
    | HentAAregArbeidserfaring
    | HentetAAregArbeidserfaring (Result Http.Error (List Arbeidserfaring))
    | BrukerOppdatererYrke String
    | BrukerTrykkerTypeaheadTast Typeahead.Operation
    | BrukerOppdtarerYrkeTypeahead YrkeTypeahead
    | BrukerHovrerOverTypeaheadSuggestion YrkeTypeahead
    | BrukerVelgerYrke YrkeTypeahead
    | HentetYrkeTypeahead (Result Http.Error (List YrkeTypeahead))
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
    | BrukerSvarerJaTilNaavarende
    | BrukerSvarerNeiTilNaavarende
    | BrukerTrykketTilMånedKnapp Dato.Måned
    | BrukerOppdatererTilÅr String
    | BrukerVilGåTilOppsummering
    | BrukerVilRedigereOppsummering
    | YrkeRedigeringsfeltEndret String
    | ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.Felt String
    | ArbeidserfaringBoolSkjemaEndret ArbeidserfaringSkjema.Felt
    | BrukerTrykkerPåLagreArbeidserfaringKnapp String
    | ArbeidserfaringLagret (Result Http.Error (List Arbeidserfaring))
    | NyArbeidserfaring
    | Ferdig
    | ErrorLogget (Result Http.Error ())


type Samtale
    = Intro
    | HenterFraAareg
    | HentetFraAAreg
    | IkkeHentetFraAAreg
    | IngenArbeidserfaringFraAareg (List Arbeidserfaring)
    | VisArbeidserfaringFraAareg (List Arbeidserfaring)
    | RegistrerYrke (TypeaheadState YrkeTypeahead)
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
    | VisOppsummering ArbeidserfaringSkjema
    | RedigerOppsummering ArbeidserfaringSkjema.ArbeidserfaringSkjema
    | LagreArbeidserfaring ArbeidserfaringSkjema
    | LagringFeilet Http.Error ArbeidserfaringSkjema
    | SpørOmBrukerVilLeggeInnMer
    | StartNyArbeidserfaring (TypeaheadState YrkeTypeahead)


type alias YrkeInfo =
    { yrke : String }


type alias JobbtittelInfo =
    { tidligereInfo : YrkeTypeahead
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


yrkeInfoTilJobbtittelInfo : YrkeTypeahead -> JobbtittelInfo
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
    , fraÅr = "1990"
    , naavarende = False
    }


fraDatoInfoTilTilDatoInfo : FraDatoInfo -> TilDatoInfo
fraDatoInfoTilTilDatoInfo fraDatoInfo =
    { tidligereInfo = fraDatoInfo
    , tilMåned = Dato.Januar
    , tilÅr = ""
    }


tilDatoTilSkjema : TilDatoInfo -> ArbeidserfaringSkjema.ArbeidserfaringSkjema
tilDatoTilSkjema tilDatoInfo =
    ArbeidserfaringSkjema.ArbeidserfaringSkjema
        { yrke =
            tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo
        , jobbTittel = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.jobbtittel
        , bedriftNavn = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.bedriftNavn
        , lokasjon = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.lokasjon
        , arbeidsoppgaver = tilDatoInfo.tidligereInfo.tidligereInfo.arbeidsoppgaver
        , fraDato = Dato.tilDato (tilDatoInfo.tidligereInfo.fraÅr ++ "-" ++ (tilDatoInfo.tidligereInfo.fraMåned |> Dato.månedTilString))
        , naavarende = tilDatoInfo.tidligereInfo.naavarende
        , tilDato =
            if tilDatoInfo.tidligereInfo.naavarende then
                Nothing

            else
                Just (Dato.tilDato (tilDatoInfo.tilÅr ++ "-" ++ (tilDatoInfo.tilMåned |> Dato.månedTilString)))
        , styrkkode =
            tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo
                |> YrkeTypeahead.styrkkode
        , konseptId =
            tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo
                |> YrkeTypeahead.konseptId
        }


update : Msg -> Model -> SamtaleStatus
update msg (Model info) =
    case msg of
        HentAAregArbeidserfaring ->
            ( Model
                { info
                    | seksjonsMeldingsLogg =
                        info.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (Melding.svar [ "Ja, jeg har arbeidserfaring" ])
                    , aktivSamtale = HenterFraAareg
                }
            , Api.hentAAreg HentetAAregArbeidserfaring
            )
                |> IkkeFerdig

        HentetAAregArbeidserfaring result ->
            case result of
                Ok arbeidserfaringFraAAreg ->
                    ( info
                        |> visAaregResultat arbeidserfaringFraAAreg
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Err error ->
                    ( IkkeHentetFraAAreg
                        |> nesteSamtaleSteg info (Melding.svar [ "Ja, jeg har arbeidserfaring" ])
                    , logFeilmelding error "Hente fra Aareg"
                    )
                        |> IkkeFerdig

        BrukerOppretterNyArbeidserfaring ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerYrke
                |> nesteSamtaleSteg info (Melding.svar [ "Ja, jeg har arbeidserfaring" ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererYrke string ->
            case info.aktivSamtale of
                RegistrerYrke typeaheadState ->
                    ( Model
                        { info
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue string
                                    |> RegistrerYrke
                        }
                    , Api.hentYrkeTypeahead HentetYrkeTypeahead string
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        HentetYrkeTypeahead result ->
            case info.aktivSamtale of
                RegistrerYrke typeaheadState ->
                    case result of
                        Ok suggestions ->
                            ( typeaheadState
                                |> TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> RegistrerYrke
                                |> oppdaterSamtalesteg info
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model info, logFeilmelding error "Hente fra Aareg" )
                                |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdtarerYrkeTypeahead yrkeTypahead ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        BrukerHovrerOverTypeaheadSuggestion yrkeTypeahead ->
            case info.aktivSamtale of
                RegistrerYrke typeaheadState ->
                    ( Model
                        { info
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateActive yrkeTypeahead
                                    |> RegistrerYrke
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykkerTypeaheadTast operation ->
            case info.aktivSamtale of
                RegistrerYrke typeaheadState ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { info
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
                                { info
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
                                    brukerVelgerYrke info active

                                Nothing ->
                                    ( Model
                                        { info
                                            | aktivSamtale = RegistrerYrke typeaheadState
                                        }
                                    , Cmd.none
                                    )
                                        |> IkkeFerdig

                        Typeahead.MouseLeaveSuggestions ->
                            ( Model
                                { info
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.removeActive
                                            |> RegistrerYrke
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerVelgerYrke yrkesTypeahead ->
            case info.aktivSamtale of
                RegistrerYrke _ ->
                    brukerVelgerYrke info yrkesTypeahead

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerVilEndreJobbtittel jobbtittelInfo ->
            ( EndreJobbtittel jobbtittelInfo
                |> nesteSamtaleSteg info (Melding.svar [ "Nei, legg til et nytt navn" ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererJobbtittelFelt string ->
            case info.aktivSamtale of
                EndreJobbtittel jobbtittelInfo ->
                    ( Model
                        { info
                            | aktivSamtale = EndreJobbtittel { jobbtittelInfo | jobbtittel = string }
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereBedriftnavn knappeTekst ->
            case info.aktivSamtale of
                EndreJobbtittel jobbtittelInfo ->
                    ( jobbtittelInfo
                        |> jobbtittelInfoTilBedriftnavnsInfo
                        |> RegistrereBedriftNavn
                        |> nesteSamtaleSteg info (Melding.svar [ knappeTekst ])
                    , Cmd.none
                    )
                        |> IkkeFerdig

                SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
                    ( jobbtittelInfo
                        |> jobbtittelInfoTilBedriftnavnsInfo
                        |> RegistrereBedriftNavn
                        |> nesteSamtaleSteg info (Melding.svar [ knappeTekst ])
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererBedriftnavn string ->
            case info.aktivSamtale of
                RegistrereBedriftNavn beriftnavnsInfo ->
                    ( Model
                        { info
                            | aktivSamtale = RegistrereBedriftNavn { beriftnavnsInfo | bedriftNavn = string }
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereSted ->
            case info.aktivSamtale of
                RegistrereBedriftNavn bedriftnavnInfo ->
                    ( bedriftnavnInfo
                        |> bedriftnavnsInfoTilLokasjonInfo
                        |> RegistrereSted
                        |> nesteSamtaleSteg info (Melding.svar [ bedriftnavnInfo.bedriftNavn ])
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererSted string ->
            case info.aktivSamtale of
                RegistrereSted stedInfo ->
                    ( Model
                        { info
                            | aktivSamtale = RegistrereSted { stedInfo | lokasjon = string }
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereArbeidsoppgaver ->
            case info.aktivSamtale of
                RegistrereSted stedInfo ->
                    ( stedInfo
                        |> stedInfoTilArbeidsoppgaverInfo
                        |> RegistrereArbeidsoppgaver
                        |> nesteSamtaleSteg info (Melding.svar [ stedInfo.lokasjon ])
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererArbeidsoppgaver string ->
            case info.aktivSamtale of
                RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
                    ( Model
                        { info
                            | aktivSamtale =
                                RegistrereArbeidsoppgaver { arbeidsoppgaverInfo | arbeidsoppgaver = string }
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereFraMåned ->
            case info.aktivSamtale of
                RegistrereArbeidsoppgaver arbeidsOppgaveInfo ->
                    ( arbeidsOppgaveInfo
                        |> arbeidsoppgaverInfoTilfraDatoInfo
                        |> RegistrereFraMåned
                        |> nesteSamtaleSteg info (Melding.svar [ arbeidsOppgaveInfo.arbeidsoppgaver ])
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykketFraMånedKnapp måned ->
            case info.aktivSamtale of
                RegistrereFraMåned fraDatoInfo ->
                    ( måned
                        |> setFraMåned fraDatoInfo
                        |> RegistrereFraÅr
                        |> nesteSamtaleSteg info
                            (Melding.svar
                                [ måned
                                    |> Dato.månedTilString
                                ]
                            )
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererFraÅr string ->
            case info.aktivSamtale of
                RegistrereFraÅr fraDatoInfo ->
                    ( Model
                        { info
                            | aktivSamtale =
                                RegistrereFraÅr { fraDatoInfo | fraÅr = string }
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereNaavarende ->
            case info.aktivSamtale of
                RegistrereFraÅr datoInfo ->
                    ( RegistrereNaavarende datoInfo
                        |> nesteSamtaleSteg info (Melding.svar [ datoInfo.fraÅr ])
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerSvarerJaTilNaavarende ->
            case info.aktivSamtale of
                RegistrereNaavarende datoInfo ->
                    ( datoInfo
                        |> fraDatoInfoTilTilDatoInfo
                        |> setNaavarendeTilTrue
                        |> tilDatoTilSkjema
                        |> VisOppsummering
                        |> nesteSamtaleSteg info (Melding.svar [ "Ja" ])
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerSvarerNeiTilNaavarende ->
            case info.aktivSamtale of
                RegistrereNaavarende fraDatoInfo ->
                    ( fraDatoInfo
                        |> fraDatoInfoTilTilDatoInfo
                        |> RegistrereTilMåned
                        |> nesteSamtaleSteg info (Melding.svar [ "Nei" ])
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykketTilMånedKnapp måned ->
            case info.aktivSamtale of
                RegistrereTilMåned tilDatoInfo ->
                    ( RegistrereTilÅr tilDatoInfo
                        |> nesteSamtaleSteg info
                            (Melding.svar
                                [ måned
                                    |> Dato.månedTilString
                                ]
                            )
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerOppdatererTilÅr string ->
            case info.aktivSamtale of
                RegistrereTilÅr tilDatoInfo ->
                    ( Model
                        { info
                            | aktivSamtale =
                                RegistrereTilÅr { tilDatoInfo | tilÅr = string }
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerVilGåTilOppsummering ->
            case info.aktivSamtale of
                RegistrereTilÅr tilDatoInfo ->
                    ( tilDatoInfo
                        |> tilDatoTilSkjema
                        |> VisOppsummering
                        |> nesteSamtaleSteg info (Melding.svar [ tilDatoInfo.tilÅr ])
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRedigereOppsummering ->
            case info.aktivSamtale of
                VisOppsummering skjema ->
                    ( skjema
                        |> RedigerOppsummering
                        |> nesteSamtaleSteg info
                            (Melding.svar [ "Nei, jeg vil endre" ])
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        YrkeRedigeringsfeltEndret string ->
            case info.aktivSamtale of
                RedigerOppsummering arbeidserfaringSkjema ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        ArbeidserfaringStringSkjemaEndret felt string ->
            case info.aktivSamtale of
                RedigerOppsummering arbeidserfaringSkjema ->
                    ( Model
                        { info
                            | aktivSamtale =
                                ArbeidserfaringSkjema.oppdaterStringFelt arbeidserfaringSkjema felt string
                                    |> RedigerOppsummering
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        ArbeidserfaringBoolSkjemaEndret felt ->
            case info.aktivSamtale of
                RedigerOppsummering arbeidserfaringSkjema ->
                    ( Model
                        { info
                            | aktivSamtale =
                                ArbeidserfaringSkjema.toggleBool arbeidserfaringSkjema felt
                                    |> RedigerOppsummering
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykkerPåLagreArbeidserfaringKnapp brukerSvar ->
            case info.aktivSamtale of
                RedigerOppsummering skjema ->
                    ( skjema
                        |> LagreArbeidserfaring
                        |> nesteSamtaleSteg info
                            (Melding.svar [ brukerSvar ])
                    , skjema
                        |> Api.lagreArbeidserfaring ArbeidserfaringLagret
                    )
                        |> IkkeFerdig

                VisOppsummering skjema ->
                    ( skjema
                        |> LagreArbeidserfaring
                        |> nesteSamtaleSteg info
                            (Melding.svar [ brukerSvar ])
                    , skjema
                        |> Api.lagreArbeidserfaring ArbeidserfaringLagret
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        ArbeidserfaringLagret result ->
            case info.aktivSamtale of
                LagreArbeidserfaring arbeidserfaringSkjema ->
                    case result of
                        Ok arbeidserfaringer ->
                            ( SpørOmBrukerVilLeggeInnMer
                                |> oppdaterSamtalesteg { info | arbeidserfaringListe = arbeidserfaringer }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( LagringFeilet error arbeidserfaringSkjema
                                |> nesteSamtaleSteg info (Melding.spørsmål [ "Noe gikk galt med lagringen" ])
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        NyArbeidserfaring ->
            ( ""
                |> TypeaheadState.init
                |> StartNyArbeidserfaring
                |> nesteSamtaleSteg info (Melding.svar [ "Ja, legg til en arbeidserfaring" ])
            , Cmd.none
            )
                |> IkkeFerdig

        Ferdig ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        ErrorLogget result ->
            ( Model info, Cmd.none )
                |> IkkeFerdig


brukerVelgerYrke : ModelInfo -> YrkeTypeahead -> SamtaleStatus
brukerVelgerYrke info yrkesTypeahead =
    ( yrkesTypeahead
        |> yrkeInfoTilJobbtittelInfo
        |> SpørOmBrukerVilEndreJobbtittel
        |> nesteSamtaleSteg info (Melding.svar [ YrkeTypeahead.label yrkesTypeahead ])
    , Cmd.none
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


lagTypeaheadKnapper : YrkeTypeahead -> Html Msg
lagTypeaheadKnapper yrkeTypahead =
    yrkeTypahead
        |> YrkeTypeahead.label
        |> Knapp.knapp (BrukerOppdtarerYrkeTypeahead yrkeTypahead)
        |> Knapp.toHtml


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


setNaavarendeTilTrue : TilDatoInfo -> TilDatoInfo
setNaavarendeTilTrue info =
    { info | tidligereInfo = setTilTrue info.tidligereInfo }


setTilTrue : FraDatoInfo -> FraDatoInfo
setTilTrue fraDatoInfo =
    { fraDatoInfo | naavarende = True }


setFraMåned : FraDatoInfo -> Dato.Måned -> FraDatoInfo
setFraMåned fraDatoInfo måned =
    { fraDatoInfo | fraMåned = måned }


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg personaliaSeksjon =
    case personaliaSeksjon of
        Intro ->
            [ Melding.spørsmål [ "Nå skal vi registrere arbeidserfaringen din" ] ]

        HenterFraAareg ->
            [ Melding.spørsmål [ "bla bla bla" ] ]

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
            [ Melding.spørsmål
                [ "Nå skal du legge inn en og en av arbeidserfaringene dine. Da setter vi igang :)"
                ]
            , Melding.spørsmål
                [ "Først må du velge et yrke. Begynn og skriv og velg fra forslagene som kommer opp"
                , "Grunnen til at du må velge et av forslagene er fordi arbeidsgiverene skal kunne finne deg i søket sitt."
                ]
            , Melding.spørsmål
                [ "Hvis du ikke finner yrket ditt, velg det nærmeste. Hvis yrket du velger ikke stemmer helt, kan du endre navnet etterpå"
                ]
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
            [ Melding.spørsmål [ "Hva er navnet på bedriften du jobbet i?" ] ]

        RegistrereSted lokasjonInfo ->
            [ Melding.spørsmål [ "Hvor holder bedriften til?" ] ]

        RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
            [ Melding.spørsmål
                [ "Fortell arbeidsgivere hvilke arbeidsoppgaver du har hatt, hva du har lært og hva som var rollen din."
                ]
            ]

        RegistrereFraMåned periodeInfo ->
            [ Melding.spørsmål [ "Hvilken måned begynte du i jobben?" ] ]

        RegistrereFraÅr periodeInfo ->
            [ Melding.spørsmål [ "Hvilket år begynte du i jobben?" ] ]

        RegistrereNaavarende periodeInfo ->
            [ Melding.spørsmål [ "Jobber du fremdeles her?" ] ]

        RegistrereTilMåned periodeInfo ->
            [ Melding.spørsmål [ "Hvilken måned sluttet du i jobben?" ] ]

        RegistrereTilÅr periodeInfo ->
            [ Melding.spørsmål [ "Hvilket år sluttet du i jobben?" ] ]

        VisOppsummering skjema ->
            [ Melding.spørsmål
                [ "Er informasjonen du la inn riktig?"
                , "Stilling/Yrke: " ++ hentStilling skjema
                , "Bedriftnanv: " ++ ArbeidserfaringSkjema.bedriftNavn skjema
                , "Sted: " ++ ArbeidserfaringSkjema.lokasjon skjema
                , "Arbeidsoppgaver: " ++ ArbeidserfaringSkjema.arbeidsoppgaver skjema
                , "Fra: " ++ hentFraDato skjema
                , if ArbeidserfaringSkjema.naavarende skjema == True then
                    "Nåværende jobb"

                  else
                    hentTilDato skjema
                ]
            ]

        RedigerOppsummering skjema ->
            []

        LagreArbeidserfaring arbeidserfaringSkjema ->
            [ Melding.spørsmål [ "Lagrer arbeidserfaring..." ]
            , Melding.spørsmål [ "Har du flere arbeidserfaringer du ønsker å legge inn?" ]
            ]

        LagringFeilet error arbeidserfaringSkjema ->
            []

        SpørOmBrukerVilLeggeInnMer ->
            [ Melding.spørsmål [ "Har du flere arbeidserfaringer du ønsker å legge inn?" ] ]

        StartNyArbeidserfaring _ ->
            [ Melding.spørsmål [ "Da begynner vi på nytt med å registrere yrke. Husk at du kan endre tittel som kommer på CVen senere" ] ]


hentStilling : ArbeidserfaringSkjema -> String
hentStilling skjema =
    if ArbeidserfaringSkjema.jobbTittel skjema == "" then
        skjema
            |> ArbeidserfaringSkjema.yrke
            |> YrkeTypeahead.label

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
        "Til: " ++ (dato |> Dato.måned |> Dato.månedTilString) ++ " " ++ (dato |> Dato.år |> String.fromInt)



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model info) =
    case info.aktivSamtale of
        Intro ->
            div [ class "inputrad" ]
                [ Knapp.knapp BrukerOppretterNyArbeidserfaring "Ja, jeg har arbeidserfaring"
                    |> Knapp.toHtml
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
                [ viewTypeahead typeaheadState
                ]

        SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
            div [ class "skjmawrapper" ]
                [ div [ class "inputrad" ]
                    [ (jobbtittelInfo
                        |> BrukerVilEndreJobbtittel
                        |> Knapp.knapp
                      )
                        "Nei, legg til nytt navn"
                        |> Knapp.toHtml
                    , (BrukerVilRegistrereBedriftnavn "Ja, det stemmer"
                        |> Knapp.knapp
                      )
                        "Ja, det stemmer"
                        |> Knapp.toHtml
                    ]
                ]

        EndreJobbtittel jobbtittelInfo ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ jobbtittelInfo.jobbtittel
                        |> Input.input
                            { label = "Stilling/yrke som vil vises i CV", msg = BrukerOppdatererJobbtittelFelt }
                        |> Input.toHtml
                    ]
                , div [ class "inputrad" ]
                    [ BrukerVilRegistrereBedriftnavn "Gå videre"
                        |> lagTekstInputKnapp "Gå videre" jobbtittelInfo.jobbtittel
                    ]
                ]

        RegistrereBedriftNavn bedriftnanvsInfo ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ bedriftnanvsInfo.bedriftNavn
                        |> Input.input { label = "Bedriftens navn", msg = BrukerOppdatererBedriftnavn }
                        |> Input.toHtml
                    ]
                , div [ class "inputrad" ]
                    [ BrukerVilRegistrereSted
                        |> lagTekstInputKnapp "Gå videre" bedriftnanvsInfo.bedriftNavn
                    ]
                ]

        RegistrereSted stedInfo ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ stedInfo.lokasjon
                        |> Input.input { label = "Sted/land", msg = BrukerOppdatererSted }
                        |> Input.toHtml
                    ]
                , div [ class "inputrad" ]
                    [ BrukerVilRegistrereArbeidsoppgaver
                        |> lagTekstInputKnapp "Gå videre" stedInfo.lokasjon
                    ]
                ]

        RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ arbeidsoppgaverInfo.arbeidsoppgaver
                        |> Input.input { label = "Hvilke arbeidsoppgaver gjorde du?", msg = BrukerOppdatererArbeidsoppgaver }
                        |> Input.toHtml
                    ]
                , div [ class "inputrad" ]
                    [ BrukerVilRegistrereFraMåned
                        |> lagTekstInputKnapp "Gå videre" arbeidsoppgaverInfo.arbeidsoppgaver
                    ]
                ]

        RegistrereFraMåned fraDatoInfo ->
            div [ class "skjema-wrapper" ]
                [ div [ class "inputrad" ]
                    [ Dato.Januar
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.Februar
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.Mars
                        |> lagFraMånedKnapp fraDatoInfo
                    ]
                , div [ class "inputrad" ]
                    [ Dato.April
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.Mai
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.Juni
                        |> lagFraMånedKnapp fraDatoInfo
                    ]
                , div [ class "inputrad" ]
                    [ Dato.Juli
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.August
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.September
                        |> lagFraMånedKnapp fraDatoInfo
                    ]
                , div [ class "inputrad" ]
                    [ Dato.Oktober
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.November
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.Desember
                        |> lagFraMånedKnapp fraDatoInfo
                    ]
                ]

        RegistrereFraÅr fraDatoInfo ->
            div [ class "skjemawrapper" ]
                [ div [ class "skjema" ]
                    [ fraDatoInfo.fraÅr
                        |> Input.input { label = "Hvilket år begynte du der?", msg = BrukerOppdatererFraÅr }
                        |> Input.toHtml
                    ]
                , div [ class "inputrad" ]
                    [ BrukerVilRegistrereNaavarende
                        |> lagÅrInputKnapp "Gå videre" fraDatoInfo.fraÅr
                    ]
                ]

        RegistrereNaavarende fraDatoInfo ->
            div []
                [ div [ class "inputrad" ]
                    [ BrukerSvarerJaTilNaavarende
                        |> lagMessageKnapp "Ja"
                    , BrukerSvarerNeiTilNaavarende
                        |> lagMessageKnapp "Nei"
                    ]
                ]

        RegistrereTilMåned tilDatoInfo ->
            div [ class "skjema-wrapper" ]
                [ div [ class "inputrad" ]
                    [ Dato.Januar
                        |> lagTilMånedKnapp tilDatoInfo
                    , Dato.Februar
                        |> lagTilMånedKnapp tilDatoInfo
                    , Dato.Mars
                        |> lagTilMånedKnapp tilDatoInfo
                    ]
                , div [ class "inputrad" ]
                    [ Dato.April
                        |> lagTilMånedKnapp tilDatoInfo
                    , Dato.Mai
                        |> lagTilMånedKnapp tilDatoInfo
                    , Dato.Juni
                        |> lagTilMånedKnapp tilDatoInfo
                    ]
                , div [ class "inputrad" ]
                    [ Dato.Juli
                        |> lagTilMånedKnapp tilDatoInfo
                    , Dato.August
                        |> lagTilMånedKnapp tilDatoInfo
                    , Dato.September
                        |> lagTilMånedKnapp tilDatoInfo
                    ]
                , div [ class "inputrad" ]
                    [ Dato.Oktober
                        |> lagTilMånedKnapp tilDatoInfo
                    , Dato.November
                        |> lagTilMånedKnapp tilDatoInfo
                    , Dato.Desember
                        |> lagTilMånedKnapp tilDatoInfo
                    ]
                ]

        RegistrereTilÅr tilDatoInfo ->
            div []
                [ label [] [ text "Skriv inn år" ]
                , input
                    [ tilDatoInfo.tilÅr |> value
                    , BrukerOppdatererTilÅr
                        |> onInput
                    ]
                    []
                , BrukerVilGåTilOppsummering
                    |> lagÅrInputKnapp "Gå videre" tilDatoInfo.tilÅr
                ]

        VisOppsummering skjema ->
            div []
                [ BrukerTrykkerPåLagreArbeidserfaringKnapp "Ja, informasjonen er riktig"
                    |> lagMessageKnapp "Ja, informasjonen er riktig"
                , BrukerVilRedigereOppsummering
                    |> lagMessageKnapp "Nei, jeg vil endre"
                ]

        RedigerOppsummering skjema ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ skjema
                        |> ArbeidserfaringSkjema.yrke
                        |> YrkeTypeahead.label
                        |> Input.input { label = "Yrke", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.Yrke }
                        |> Input.toHtml
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
                        |> Input.input { label = "Bedriftnavn", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.BedriftNavn }
                        |> Input.toHtml
                    , skjema
                        |> ArbeidserfaringSkjema.lokasjon
                        |> Input.input { label = "Sted", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.Lokasjon }
                        |> Input.toHtml
                    , skjema
                        |> ArbeidserfaringSkjema.arbeidsoppgaver
                        |> Input.input { label = "Arbeidsoppgaver", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.Arbeidsoppgaver }
                        |> Input.toHtml
                    , skjema
                        |> lagRedigerDatoInput
                    , div [ class "inputrad" ]
                        [ "Utfør endringene"
                            |> Knapp.knapp (BrukerTrykkerPåLagreArbeidserfaringKnapp "Utfør endringene")
                            |> Knapp.toHtml
                        ]
                    ]
                ]

        LagreArbeidserfaring arbeidserfaringSkjema ->
            div [] []

        LagringFeilet error arbeidserfaringSkjema ->
            div [] []

        SpørOmBrukerVilLeggeInnMer ->
            div [ class "skjema-wrapper" ]
                [ div [ class "inputrad" ]
                    [ Knapp.knapp
                        NyArbeidserfaring
                        "Ja, legg til en arbeidserfaring"
                        |> Knapp.toHtml
                    , Knapp.knapp Ferdig "Nei, jeg har lagt inn alle"
                        |> Knapp.toHtml
                    ]
                ]

        StartNyArbeidserfaring typeaheadState ->
            div [ class "skjema-wrapper" ]
                [ viewTypeahead typeaheadState
                ]


viewTypeahead : TypeaheadState YrkeTypeahead -> Html Msg
viewTypeahead typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Yrke", onInput = BrukerOppdatererYrke, onTypeaheadChange = BrukerTrykkerTypeaheadTast }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestion typeaheadState)
        |> Typeahead.toHtml


typeaheadStateSuggestionsTilViewSuggestion : TypeaheadState YrkeTypeahead -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSuggestion typeaheadState =
    typeaheadState
        |> TypeaheadState.map
            (\activeState suggestion ->
                { innhold = YrkeTypeahead.label suggestion
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
            |> Input.input { label = "År", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.FraÅr }
            |> Input.toHtml
        , input
            [ type_ "checkbox"
            , arbeidserfaringSkjema
                |> ArbeidserfaringSkjema.naavarende
                |> checked
            , ArbeidserfaringBoolSkjemaEndret ArbeidserfaringSkjema.Naavarende
                |> onClick
            ]
            [ text "Nåværende jobb" ]
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
                            |> Maybe.withDefault (ArbeidserfaringSkjema.fraDato arbeidserfaringSkjema)
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
                            |> Input.input { label = "År", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.TilÅr }
                            |> Input.toHtml

                    Nothing ->
                        text ""
                ]
        ]


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError ErrorLogget)
        |> Maybe.withDefault Cmd.none


init : MeldingsLogg -> Model
init gammelMeldingsLogg =
    Model
        { seksjonsMeldingsLogg =
            gammelMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Har du arbeidserfaring du vil legge inn?" ] ]
        , arbeidserfaringListe = []
        , aktivSamtale = Intro
        }
