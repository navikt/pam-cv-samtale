module Seksjon.Sertifikat exposing
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
import Cv.Sertifikat exposing (Sertifikat)
import Dato exposing (Måned(..), År, datoTilString)
import DebugStatus exposing (DebugStatus)
import Feilmelding
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp exposing (Enabled(..))
import FrontendModuler.ManedKnapper as MånedKnapper
import FrontendModuler.Typeahead as Typeahead
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import SertifikatTypeahead exposing (SertifikatTypeahead)
import Skjema.Sertifikat as SertifikatSkjema exposing (SertifikatSkjema, TypeaheadFelt(..), Utløpsdato(..), ValidertSertifikatSkjema)
import Task
import Typeahead.TypeaheadState as TypeaheadState exposing (TypeaheadState)



-- MODEL --


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , sertifikatListe : List Sertifikat
    , debugStatus : DebugStatus
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Sertifikat) FerdigAnimertMeldingsLogg


type Samtale
    = RegistrerSertifikatFelt (TypeaheadState SertifikatTypeahead)
    | RegistrerUtsteder UtstederInfo
    | RegistrerFullførtMåned FullførtDatoInfo
    | RegistrerFullførtÅr FullførtDatoInfo
    | SpørOmUtløpsdatoFinnes ValidertFullførtDatoInfo
    | RegistrerUtløperMåned UtløpsdatoInfo
    | RegistrerUtløperÅr UtløpsdatoInfo
    | VisOppsummering ValidertSertifikatSkjema
    | EndreOpplysninger SertifikatSkjema
    | VisOppsummeringEtterEndring ValidertSertifikatSkjema
    | LagrerSkjema ValidertSertifikatSkjema
    | LagringFeilet Http.Error ValidertSertifikatSkjema
    | VenterPåAnimasjonFørFullføring (List Sertifikat)


type SkjemaEndring
    = Utsteder String
    | FullførtMåned String
    | FullførtÅr String
    | FullførtÅrMistetFokus
    | UtløperIkkeToggled
    | UtløperMåned String
    | UtløperÅr String
    | UtløperÅrMistetFokus


type alias UtstederInfo =
    { sertifikatFelt : SertifikatTypeahead
    , utsteder : String
    }


type alias FullførtDatoInfo =
    { sertifikat : SertifikatTypeahead
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : String
    , visFeilmeldingFullførtÅr : Bool
    }


type alias ValidertFullførtDatoInfo =
    { sertifikat : SertifikatTypeahead
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : År
    }


type alias UtløpsdatoInfo =
    { forrigeFeltInfo : ValidertFullførtDatoInfo
    , utløperMåned : Måned
    , utløperÅr : String
    , visFeilmeldingUtløperÅr : Bool
    }


sertifikatFeltTilUtsteder : SertifikatTypeahead -> UtstederInfo
sertifikatFeltTilUtsteder input =
    { sertifikatFelt = input
    , utsteder = ""
    }


utstederTilFullførtDato : UtstederInfo -> FullførtDatoInfo
utstederTilFullførtDato input =
    { sertifikat = input.sertifikatFelt
    , utsteder = input.utsteder
    , fullførtMåned = Januar
    , fullførtÅr = ""
    , visFeilmeldingFullførtÅr = False
    }


validertFullførtDatoTilUtløpsdato : ValidertFullførtDatoInfo -> UtløpsdatoInfo
validertFullførtDatoTilUtløpsdato input =
    { forrigeFeltInfo = input
    , utløperMåned = Januar
    , utløperÅr = ""
    , visFeilmeldingUtløperÅr = False
    }


validertFullførtDatoTilSkjema : ValidertFullførtDatoInfo -> ValidertSertifikatSkjema
validertFullførtDatoTilSkjema input =
    SertifikatSkjema.initValidertSkjema
        { sertifikatFelt = input.sertifikat
        , utsteder = input.utsteder
        , fullførtMåned = input.fullførtMåned
        , fullførtÅr = input.fullførtÅr
        , utløpsdato = IkkeOppgitt
        , id = Nothing
        }


utløpsdatoTilSkjema : UtløpsdatoInfo -> År -> ValidertSertifikatSkjema
utløpsdatoTilSkjema info år =
    SertifikatSkjema.initValidertSkjema
        { sertifikatFelt = info.forrigeFeltInfo.sertifikat
        , utsteder = info.forrigeFeltInfo.utsteder
        , fullførtMåned = info.forrigeFeltInfo.fullførtMåned
        , fullførtÅr = info.forrigeFeltInfo.fullførtÅr
        , utløpsdato = Oppgitt info.utløperMåned år
        , id = Nothing
        }



--UPDATE--


type Msg
    = VilOppdatereSertifikatFelt String
    | HentetTypeahead (Result Http.Error (List SertifikatTypeahead))
    | HovrerOverTypeaheadSuggestion SertifikatTypeahead
    | TrykkerTypeaheadTast Typeahead.Operation
    | VelgerSertifikatFraTypeahead SertifikatTypeahead
    | VilRegistrereUtsteder
    | OppdaterUtsteder String
    | FullførtMånedValgt Dato.Måned
    | VilRegistrereFullførtÅr
    | OppdatererFullførtÅr String
    | VilRegistrereUtløperMåned
    | VilIkkeRegistrereUtløpesdato
    | UtløperMånedValgt Dato.Måned
    | VilRegistrereUtløperÅr
    | OppdatererUtløperÅr String
    | VilLagreSertifikat
    | VilEndreOpplysninger
    | SkjemaEndret SkjemaEndring
    | VilLagreEndretSkjema
    | SertifikatLagret (Result Http.Error (List Sertifikat))
    | FerdigMedSertifikat String
    | StartÅSkrive
    | FullførMelding
    | ViewportSatt (Result Dom.Error ())
    | FokusSatt (Result Dom.Error ())
    | ÅrMisterFokus
    | ErrorLogget


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        VilOppdatereSertifikatFelt string ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadState ->
                    ( typeaheadState
                        |> TypeaheadState.updateValue string
                        |> RegistrerSertifikatFelt
                        |> oppdaterSamtaleSteg model
                    , Api.getSertifikatTypeahead HentetTypeahead string
                    )
                        |> IkkeFerdig

                EndreOpplysninger skjema ->
                    ( string
                        |> SertifikatSkjema.oppdaterSertifikatFelt skjema
                        |> EndreOpplysninger
                        |> oppdaterSamtaleSteg model
                    , Api.getSertifikatTypeahead HentetTypeahead string
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetTypeahead result ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadState ->
                    case result of
                        Ok suggestions ->
                            ( typeaheadState
                                |> TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> RegistrerSertifikatFelt
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hent SertifikatTypeahead" )
                                |> IkkeFerdig

                EndreOpplysninger skjema ->
                    case result of
                        Ok suggestions ->
                            ( TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> SertifikatSkjema.mapTypeaheadState skjema
                                |> EndreOpplysninger
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hent SertifikatTypeahead" )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HovrerOverTypeaheadSuggestion typeahead ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadState ->
                    ( typeaheadState
                        |> TypeaheadState.updateActive typeahead
                        |> RegistrerSertifikatFelt
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        TrykkerTypeaheadTast operation ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadState ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( typeaheadState
                                |> TypeaheadState.arrowUp
                                |> RegistrerSertifikatFelt
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.ArrowDown ->
                            ( typeaheadState
                                |> TypeaheadState.arrowDown
                                |> RegistrerSertifikatFelt
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            case TypeaheadState.getActive typeaheadState of
                                Just active ->
                                    brukerVelgerSertifikatFelt model active

                                Nothing ->
                                    ( Model model
                                    , Cmd.none
                                    )
                                        |> IkkeFerdig

                        Typeahead.MouseLeaveSuggestions ->
                            ( typeaheadState
                                |> TypeaheadState.removeActive
                                |> RegistrerSertifikatFelt
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                EndreOpplysninger skjema ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( TypeaheadState.arrowUp
                                |> SertifikatSkjema.mapTypeaheadState skjema
                                |> EndreOpplysninger
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.ArrowDown ->
                            ( TypeaheadState.arrowDown
                                |> SertifikatSkjema.mapTypeaheadState skjema
                                |> EndreOpplysninger
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            ( skjema
                                |> SertifikatSkjema.velgAktivtSertifikatITypeahead
                                |> EndreOpplysninger
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.MouseLeaveSuggestions ->
                            ( TypeaheadState.removeActive
                                |> SertifikatSkjema.mapTypeaheadState skjema
                                |> EndreOpplysninger
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VelgerSertifikatFraTypeahead typeahead ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt _ ->
                    brukerVelgerSertifikatFelt model typeahead

                EndreOpplysninger skjema ->
                    ( skjema
                        |> SertifikatSkjema.setSertifikatFelt typeahead
                        |> EndreOpplysninger
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        VilRegistrereUtsteder ->
            case model.aktivSamtale of
                RegistrerUtsteder input ->
                    ( input
                        |> utstederTilFullførtDato
                        |> RegistrerFullførtMåned
                        |> nesteSamtaleSteg model (Melding.svar [ input.utsteder ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        OppdaterUtsteder string ->
            case model.aktivSamtale of
                RegistrerUtsteder utsteder ->
                    ( { utsteder | utsteder = string }
                        |> RegistrerUtsteder
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        FullførtMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerFullførtMåned fullførtDatoInfo ->
                    ( måned
                        |> setFullførtMåned fullførtDatoInfo
                        |> RegistrerFullførtÅr
                        |> nesteSamtaleSteg model
                            (Melding.svar [ måned |> Dato.månedTilString ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilRegistrereFullførtÅr ->
            case model.aktivSamtale of
                RegistrerFullførtÅr fullførtDatoInfo ->
                    case Dato.stringTilÅr fullførtDatoInfo.fullførtÅr of
                        Just fullførtÅr ->
                            ( { sertifikat = fullførtDatoInfo.sertifikat
                              , utsteder = fullførtDatoInfo.utsteder
                              , fullførtMåned = fullførtDatoInfo.fullførtMåned
                              , fullførtÅr = fullførtÅr
                              }
                                |> SpørOmUtløpsdatoFinnes
                                |> nesteSamtaleSteg model (Melding.svar [ fullførtDatoInfo.fullførtÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { fullførtDatoInfo | visFeilmeldingFullførtÅr = True }
                                |> RegistrerFullførtÅr
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererFullførtÅr string ->
            case model.aktivSamtale of
                RegistrerFullførtÅr fullførtDatoInfo ->
                    ( { fullførtDatoInfo | fullførtÅr = string }
                        |> RegistrerFullførtÅr
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilIkkeRegistrereUtløpesdato ->
            case model.aktivSamtale of
                SpørOmUtløpsdatoFinnes fullførtDatoInfo ->
                    ( fullførtDatoInfo
                        |> validertFullførtDatoTilSkjema
                        |> VisOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, sertifiseringen utløper ikke" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilRegistrereUtløperMåned ->
            case model.aktivSamtale of
                SpørOmUtløpsdatoFinnes fullførtDatoInfo ->
                    ( fullførtDatoInfo
                        |> validertFullførtDatoTilUtløpsdato
                        |> RegistrerUtløperMåned
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, sertifiseringen utløper" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        UtløperMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerUtløperMåned utløpsdatoInfo ->
                    ( { utløpsdatoInfo | utløperMåned = måned }
                        |> RegistrerUtløperÅr
                        |> nesteSamtaleSteg model
                            (Melding.svar [ måned |> Dato.månedTilString ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilRegistrereUtløperÅr ->
            case model.aktivSamtale of
                RegistrerUtløperÅr utløpsdatoInfo ->
                    case Dato.stringTilÅr utløpsdatoInfo.utløperÅr of
                        Just utløperÅr ->
                            ( utløpsdatoTilSkjema utløpsdatoInfo utløperÅr
                                |> VisOppsummering
                                |> nesteSamtaleSteg model (Melding.svar [ utløpsdatoInfo.utløperÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { utløpsdatoInfo | visFeilmeldingUtløperÅr = True }
                                |> RegistrerUtløperÅr
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererUtløperÅr string ->
            case model.aktivSamtale of
                RegistrerUtløperÅr utløpsdatoInfo ->
                    ( { utløpsdatoInfo | utløperÅr = string }
                        |> RegistrerUtløperÅr
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        ÅrMisterFokus ->
            case model.aktivSamtale of
                RegistrerFullførtÅr fullførtDatoInfo ->
                    ( { fullførtDatoInfo | visFeilmeldingFullførtÅr = True }
                        |> RegistrerFullførtÅr
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerUtløperÅr utløpsdatoInfo ->
                    ( { utløpsdatoInfo | visFeilmeldingUtløperÅr = True }
                        |> RegistrerUtløperÅr
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilEndreOpplysninger ->
            case model.aktivSamtale of
                VisOppsummering validertSertifikatSkjema ->
                    updateEtterVilEndreSkjema model validertSertifikatSkjema

                VisOppsummeringEtterEndring validertSertifikatSkjema ->
                    updateEtterVilEndreSkjema model validertSertifikatSkjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreOpplysninger sertifikatSkjema ->
                    ( sertifikatSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> EndreOpplysninger
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilLagreEndretSkjema ->
            case model.aktivSamtale of
                EndreOpplysninger skjema ->
                    case SertifikatSkjema.valider skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> VisOppsummeringEtterEndring
                                |> nesteSamtaleSteg model (Melding.svar (validertSkjemaTilSetninger validertSkjema))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> SertifikatSkjema.visAlleFeilmeldinger
                                |> EndreOpplysninger
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreSertifikat ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                VisOppsummeringEtterEndring skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                LagringFeilet _ skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, prøv på nytt" ])

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SertifikatLagret result ->
            case model.aktivSamtale of
                LagrerSkjema sertifikatSkjema ->
                    case result of
                        Ok sertifikater ->
                            ( Model
                                { model
                                    | aktivSamtale = VenterPåAnimasjonFørFullføring sertifikater
                                    , seksjonsMeldingsLogg =
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Nå er sertifiseringen din lagt til i CV-en!" ] ]
                                }
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( LagringFeilet error sertifikatSkjema
                                |> nesteSamtaleStegUtenMelding model
                            , sertifikatSkjema
                                |> SertifikatSkjema.encode
                                |> Api.logErrorWithRequestBody ErrorLogget "Lagre sertifikat" error
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        FerdigMedSertifikat knappeTekst ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt _ ->
                    ( model.sertifikatListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                LagringFeilet _ _ ->
                    ( model.sertifikatListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, gå videre" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
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
                    |> Task.perform (always FullførMelding)
                ]
            )
                |> IkkeFerdig

        FullførMelding ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.fullførMelding
                |> updateEtterFullførtMelding model

        ViewportSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )

        FokusSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )


setFullførtMåned : FullførtDatoInfo -> Dato.Måned -> FullførtDatoInfo
setFullførtMåned fullførtDatoInfo måned =
    { fullførtDatoInfo | fullførtMåned = måned }


oppdaterSamtalesteg : ModelInfo -> Samtale -> Model
oppdaterSamtalesteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }


oppdaterSkjema : SkjemaEndring -> SertifikatSkjema -> SertifikatSkjema
oppdaterSkjema endring skjema =
    case endring of
        Utsteder string ->
            SertifikatSkjema.oppdaterUtsteder skjema string

        FullførtMåned månedString ->
            månedString
                |> Dato.stringTilMåned
                |> SertifikatSkjema.oppdaterFullførtMåned skjema

        FullførtÅr string ->
            SertifikatSkjema.oppdaterFullførtÅr skjema string

        UtløperMåned månedString ->
            månedString
                |> Dato.stringTilMåned
                |> SertifikatSkjema.oppdaterUtløperMåned skjema

        UtløperÅr string ->
            SertifikatSkjema.oppdaterUtløperÅr skjema string

        UtløperIkkeToggled ->
            SertifikatSkjema.toggleUtløperIkke skjema

        FullførtÅrMistetFokus ->
            SertifikatSkjema.visFeilmeldingFullførtÅr skjema

        UtløperÅrMistetFokus ->
            SertifikatSkjema.visFeilmeldingUtløperÅr skjema


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring sertifikatListe ->
                    Ferdig sertifikatListe ferdigAnimertSamtale

                _ ->
                    ( Model
                        { model
                            | seksjonsMeldingsLogg =
                                nyMeldingsLogg
                        }
                    , Cmd.batch
                        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        , settFokus model.aktivSamtale
                        ]
                    )
                        |> IkkeFerdig

        MeldingerGjenstår ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        nyMeldingsLogg
                }
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig


brukerVelgerSertifikatFelt : ModelInfo -> SertifikatTypeahead -> SamtaleStatus
brukerVelgerSertifikatFelt info sertifikatTypeahead =
    ( sertifikatTypeahead
        |> sertifikatFeltTilUtsteder
        |> RegistrerUtsteder
        |> nesteSamtaleSteg info (Melding.svar [ SertifikatTypeahead.label sertifikatTypeahead ])
    , lagtTilSpørsmålCmd info.debugStatus
    )
        |> IkkeFerdig


updateEtterVilEndreSkjema : ModelInfo -> ValidertSertifikatSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model skjema =
    ( skjema
        |> SertifikatSkjema.tilUvalidertSkjema
        |> EndreOpplysninger
        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> ValidertSertifikatSkjema -> Melding -> SamtaleStatus
updateEtterLagreKnappTrykket model skjema melding =
    ( skjema
        |> LagrerSkjema
        |> nesteSamtaleSteg model melding
    , postEllerPutSertifikat SertifikatLagret skjema
    )
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


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError (always ErrorLogget))
        |> Maybe.withDefault Cmd.none


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg modelInfo melding samtale =
    Model
        { modelInfo
            | aktivSamtale = samtale
            , seksjonsMeldingsLogg =
                modelInfo.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)
        }


nesteSamtaleStegUtenMelding : ModelInfo -> Samtale -> Model
nesteSamtaleStegUtenMelding model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg sertifikatSeksjon =
    case sertifikatSeksjon of
        RegistrerSertifikatFelt _ ->
            [ Melding.spørsmål [ "Hva slags sertifikat eller sertifisering har du?" ]
            , Melding.spørsmål
                [ "Kanskje du har truckførerbevis T1, eller noe helt annet? :)" ]
            ]

        RegistrerUtsteder _ ->
            [ Melding.spørsmål
                [ "Hvilken organisasjon sertifiserte deg?" ]
            , Melding.spørsmål
                [ "Er du usikker på hvem som har ansvar for sertifiseringen? Det vil ofte stå på beviset ditt" ]
            ]

        RegistrerFullførtMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned fullførte du sertifiseringen?" ]
            ]

        RegistrerFullførtÅr _ ->
            [ Melding.spørsmål [ "Hvilket år fullførte du sertifiseringen?" ]
            ]

        SpørOmUtløpsdatoFinnes _ ->
            [ Melding.spørsmål [ "Har sertifiseringen en utløpsdato?" ]
            ]

        RegistrerUtløperMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned utløper sertifiseringen din?" ]
            ]

        RegistrerUtløperÅr _ ->
            [ Melding.spørsmål [ "Hvilket år utløper du sertifiseringen din?" ]
            ]

        VisOppsummering skjema ->
            [ Melding.spørsmål
                ([ "Du har lagt inn dette:"
                 , Melding.tomLinje
                 ]
                    ++ (validertSkjemaTilSetninger skjema
                            ++ [ Melding.tomLinje
                               , "Er informasjonen riktig?"
                               ]
                       )
                )
            ]

        EndreOpplysninger _ ->
            []

        VisOppsummeringEtterEndring _ ->
            [ Melding.spørsmål [ "Er informasjonen riktig nå?" ] ]

        LagrerSkjema _ ->
            []

        LagringFeilet _ _ ->
            [ Melding.spørsmål
                [ "Oops... Jeg klarte ikke å lagre sertifikatet. Vil du prøve på nytt?" ]
            ]

        VenterPåAnimasjonFørFullføring _ ->
            []


validertSkjemaTilSetninger : ValidertSertifikatSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    let
        skjema =
            SertifikatSkjema.tilUvalidertSkjema validertSkjema
    in
    [ "Sertifisering/sertifikat: " ++ (validertSkjema |> SertifikatSkjema.sertifikatFeltValidert |> SertifikatTypeahead.label)
    , "Utsteder: " ++ SertifikatSkjema.utsteder skjema
    , "Fullført: " ++ Dato.datoTilString (SertifikatSkjema.fullførtMåned skjema) (SertifikatSkjema.fullførtÅrValidert validertSkjema)
    , "Utløper: " ++ utløpsdatoTilString (SertifikatSkjema.utløpsdatoValidert validertSkjema)
    ]


utløpsdatoTilString : Utløpsdato -> String
utløpsdatoTilString utløpsdato =
    case utløpsdato of
        Oppgitt måned_ år_ ->
            datoTilString måned_ år_

        IkkeOppgitt ->
            ""


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        RegistrerSertifikatFelt _ ->
            settFokusCmd SertifikatTypeaheadId

        RegistrerUtsteder _ ->
            settFokusCmd UtstederId

        RegistrerFullførtÅr _ ->
            settFokusCmd FullførtÅrId

        RegistrerUtløperÅr _ ->
            settFokusCmd UtløperÅrId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    inputId
        |> inputIdTilString
        |> Dom.focus
        |> Task.attempt FokusSatt



-- View --


type InputId
    = SertifikatTypeaheadId
    | UtstederId
    | FullførtÅrId
    | UtløperÅrId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        UtstederId ->
            "sertifikat-utsteder-id"

        FullførtÅrId ->
            "sertifikat-fullførtår-id"

        SertifikatTypeaheadId ->
            "sertifikat-typeahead-id"

        UtløperÅrId ->
            "sertifikat-utløperår-id"


viewTypeaheadSertifikatFelt : TypeaheadState SertifikatTypeahead -> Html Msg
viewTypeaheadSertifikatFelt typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Sertifisering eller sertifikat", onInput = VilOppdatereSertifikatFelt, onTypeaheadChange = TrykkerTypeaheadTast, inputId = inputIdTilString SertifikatTypeaheadId }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSertifikatFelt typeaheadState)
        |> Typeahead.toHtml


typeaheadStateSuggestionsTilViewSertifikatFelt : TypeaheadState SertifikatTypeahead -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSertifikatFelt typeaheadState =
    typeaheadState
        |> TypeaheadState.mapSuggestions
            (\activeState suggestion ->
                { innhold = SertifikatTypeahead.label suggestion
                , onClick = VelgerSertifikatFraTypeahead suggestion
                , onActive = HovrerOverTypeaheadSuggestion suggestion
                , active =
                    case activeState of
                        TypeaheadState.Active ->
                            True

                        TypeaheadState.NotActive ->
                            False
                }
            )


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ viewTypeaheadSertifikatFelt typeaheadState
                            ]
                        ]

                RegistrerUtsteder input ->
                    Containers.inputMedGåVidereKnapp VilRegistrereUtsteder
                        [ input.utsteder
                            |> Input.input { label = "Utsteder", msg = OppdaterUtsteder }
                            |> Input.withOnEnter VilRegistrereUtsteder
                            |> Input.withId (inputIdTilString UtstederId)
                            |> Input.toHtml
                        ]

                RegistrerFullførtMåned _ ->
                    MånedKnapper.månedKnapper FullførtMånedValgt

                RegistrerFullførtÅr fullførtDatoInfo ->
                    Containers.inputMedGåVidereKnapp VilRegistrereFullførtÅr
                        [ div [ class "år-wrapper" ]
                            [ fullførtDatoInfo.fullførtÅr
                                |> Input.input { label = "År", msg = OppdatererFullførtÅr }
                                |> Input.withClass "aar"
                                |> Input.withOnEnter VilRegistrereFullførtÅr
                                |> Input.withOnBlur ÅrMisterFokus
                                |> Input.withId (inputIdTilString FullførtÅrId)
                                |> Input.withMaybeFeilmelding
                                    (fullførtDatoInfo.fullførtÅr
                                        |> Dato.feilmeldingÅr
                                        |> maybeHvisTrue fullførtDatoInfo.visFeilmeldingFullførtÅr
                                    )
                                |> Input.toHtml
                            ]
                        ]

                SpørOmUtløpsdatoFinnes _ ->
                    Containers.knapper Flytende
                        [ "Ja, sertifiseringen utløper"
                            |> Knapp.knapp VilRegistrereUtløperMåned
                            |> Knapp.toHtml
                        , "Nei, sertifiseringen utløper ikke"
                            |> Knapp.knapp VilIkkeRegistrereUtløpesdato
                            |> Knapp.toHtml
                        ]

                RegistrerUtløperMåned _ ->
                    MånedKnapper.månedKnapper UtløperMånedValgt

                RegistrerUtløperÅr utløpsdatoInfo ->
                    Containers.inputMedGåVidereKnapp VilRegistrereUtløperÅr
                        [ div [ class "år-wrapper" ]
                            [ utløpsdatoInfo.utløperÅr
                                |> Input.input { label = "År", msg = OppdatererUtløperÅr }
                                |> Input.withClass "aar"
                                |> Input.withOnEnter VilRegistrereUtløperÅr
                                |> Input.withOnBlur ÅrMisterFokus
                                |> Input.withId (inputIdTilString UtløperÅrId)
                                |> Input.withMaybeFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue utløpsdatoInfo.visFeilmeldingUtløperÅr) utløpsdatoInfo.utløperÅr)
                                |> Input.toHtml
                            ]
                        ]

                VisOppsummering _ ->
                    viewBekreftOppsummering

                VisOppsummeringEtterEndring _ ->
                    viewBekreftOppsummering

                EndreOpplysninger skjema ->
                    Containers.skjema { lagreMsg = VilLagreEndretSkjema, lagreKnappTekst = "Lagre endringer" }
                        [ case SertifikatSkjema.sertifikatTypeahed skjema of
                            SuggestionValgt sertifikat ->
                                sertifikat
                                    |> SertifikatTypeahead.label
                                    |> Input.input { label = "Sertifisering", msg = VilOppdatereSertifikatFelt }
                                    |> Input.toHtml

                            SuggestionIkkeValgt typeaheadState ->
                                viewTypeahead typeaheadState
                        , skjema
                            |> SertifikatSkjema.utsteder
                            |> Input.input { label = "Utsteder", msg = Utsteder >> SkjemaEndret }
                            |> Input.toHtml
                        , div [ class "DatoInput-fra-til-rad" ]
                            [ DatoInput.datoInput
                                { label = "Fullført"
                                , onMånedChange = FullførtMåned >> SkjemaEndret
                                , måned = SertifikatSkjema.fullførtMåned skjema
                                , onÅrChange = FullførtÅr >> SkjemaEndret
                                , år = SertifikatSkjema.fullførtÅr skjema
                                }
                                |> DatoInput.withMaybeFeilmeldingÅr (SertifikatSkjema.feilmeldingFullførtÅr skjema)
                                |> DatoInput.withOnBlurÅr (SkjemaEndret FullførtÅrMistetFokus)
                                |> DatoInput.toHtml
                            , if not (SertifikatSkjema.utløperIkke skjema) then
                                DatoInput.datoInput
                                    { label = "Utløper"
                                    , onMånedChange = UtløperMåned >> SkjemaEndret
                                    , måned = SertifikatSkjema.utløperMåned skjema
                                    , onÅrChange = UtløperÅr >> SkjemaEndret
                                    , år = SertifikatSkjema.utløperÅr skjema
                                    }
                                    |> DatoInput.withMaybeFeilmeldingÅr (SertifikatSkjema.feilmeldingUtløperÅr skjema)
                                    |> DatoInput.withOnBlurÅr (SkjemaEndret UtløperÅrMistetFokus)
                                    |> DatoInput.toHtml

                              else
                                text ""
                            ]
                        , skjema
                            |> SertifikatSkjema.utløperIkke
                            |> Checkbox.checkbox "Sertifiseringen utløper ikke" (SkjemaEndret UtløperIkkeToggled)
                            |> Checkbox.toHtml
                        ]

                LagrerSkjema _ ->
                    div [] []

                LagringFeilet _ _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp VilLagreSertifikat "Ja, prøv på nytt"
                            |> Knapp.toHtml
                        , Knapp.knapp (FerdigMedSertifikat "Nei, gå videre") "Nei, gå videre"
                            |> Knapp.toHtml
                        ]

                VenterPåAnimasjonFørFullføring _ ->
                    div [] []

        MeldingerGjenstår ->
            text ""


viewTypeahead : TypeaheadState SertifikatTypeahead -> Html Msg
viewTypeahead typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Sertifisering eller sertifikat", onInput = VilOppdatereSertifikatFelt, onTypeaheadChange = TrykkerTypeaheadTast, inputId = inputIdTilString SertifikatTypeaheadId }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestion typeaheadState)
        |> Typeahead.toHtml


viewBekreftOppsummering : Html Msg
viewBekreftOppsummering =
    Containers.knapper Flytende
        [ Knapp.knapp VilLagreSertifikat "Ja, informasjonen er riktig"
            |> Knapp.toHtml
        , Knapp.knapp VilEndreOpplysninger "Nei, jeg vil endre"
            |> Knapp.toHtml
        ]


typeaheadStateSuggestionsTilViewSuggestion : TypeaheadState SertifikatTypeahead -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSuggestion typeaheadState =
    typeaheadState
        |> TypeaheadState.mapSuggestions
            (\activeState suggestion ->
                { innhold = SertifikatTypeahead.label suggestion
                , onClick = VelgerSertifikatFraTypeahead suggestion
                , onActive = HovrerOverTypeaheadSuggestion suggestion
                , active =
                    case activeState of
                        TypeaheadState.Active ->
                            True

                        TypeaheadState.NotActive ->
                            False
                }
            )


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


postEllerPutSertifikat : (Result Error (List Sertifikat) -> msg) -> SertifikatSkjema.ValidertSertifikatSkjema -> Cmd msg
postEllerPutSertifikat msgConstructor skjema =
    case SertifikatSkjema.id skjema of
        Just id ->
            Api.putSertifikat msgConstructor skjema id

        Nothing ->
            Api.postSertifikat msgConstructor skjema



-- INIT --


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List Sertifikat -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg sertifikatListe =
    let
        aktivSamtale =
            ""
                |> TypeaheadState.init
                |> RegistrerSertifikatFelt
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , sertifikatListe = sertifikatListe
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )
