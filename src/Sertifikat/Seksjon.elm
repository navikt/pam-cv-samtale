module Sertifikat.Seksjon exposing
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
import FrontendModuler.Knapp as Knapp
import FrontendModuler.ManedKnapper as MånedKnapper
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import Sertifikat.Skjema as Skjema exposing (SertifikatFelt(..), SertifikatSkjema, Utløpsdato(..), ValidertSertifikatSkjema)
import SertifikatTypeahead exposing (SertifikatTypeahead)
import Task
import Typeahead.Typeahead as Typeahead exposing (GetSuggestionStatus(..))



--- MODEL ---


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
    = RegistrerSertifikatFelt (Typeahead.Model SertifikatTypeahead)
    | RegistrerUtsteder UtstederInfo
    | RegistrerFullførtMåned FullførtDatoInfo
    | RegistrerFullførtÅr FullførtDatoInfo
    | SpørOmUtløpsdatoFinnes ValidertFullførtDatoInfo
    | RegistrerUtløperMåned UtløpsdatoInfo
    | RegistrerUtløperÅr UtløpsdatoInfo
    | VisOppsummering ValidertSertifikatSkjema
    | EndreOpplysninger (Typeahead.Model SertifikatTypeahead) SertifikatSkjema
    | VisOppsummeringEtterEndring ValidertSertifikatSkjema
    | LagrerSkjema ValidertSertifikatSkjema
    | LagringFeilet Http.Error ValidertSertifikatSkjema
    | VenterPåAnimasjonFørFullføring (List Sertifikat)


type alias UtstederInfo =
    { sertifikatFelt : SertifikatFelt
    , utsteder : String
    }


type alias FullførtDatoInfo =
    { sertifikat : SertifikatFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : String
    , visFeilmeldingFullførtÅr : Bool
    }


type alias ValidertFullførtDatoInfo =
    { sertifikat : SertifikatFelt
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


sertifikatFeltTilUtsteder : SertifikatFelt -> UtstederInfo
sertifikatFeltTilUtsteder sertifikatFelt =
    { sertifikatFelt = sertifikatFelt
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
    Skjema.initValidertSkjema
        { sertifikatFelt = input.sertifikat
        , utsteder = input.utsteder
        , fullførtMåned = input.fullførtMåned
        , fullførtÅr = input.fullførtÅr
        , utløpsdato = IkkeOppgitt
        , id = Nothing
        }


utløpsdatoTilSkjema : UtløpsdatoInfo -> År -> ValidertSertifikatSkjema
utløpsdatoTilSkjema info år =
    Skjema.initValidertSkjema
        { sertifikatFelt = info.forrigeFeltInfo.sertifikat
        , utsteder = info.forrigeFeltInfo.utsteder
        , fullførtMåned = info.forrigeFeltInfo.fullførtMåned
        , fullførtÅr = info.forrigeFeltInfo.fullførtÅr
        , utløpsdato = Oppgitt info.utløperMåned år
        , id = Nothing
        }



--- UPDATE ---


type Msg
    = TypeaheadMsg (Typeahead.Msg SertifikatTypeahead)
    | HentetTypeahead (Result Http.Error (List SertifikatTypeahead))
    | VilRegistrereSertifikat
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


type SkjemaEndring
    = Utsteder String
    | FullførtMåned String
    | FullførtÅr String
    | FullførtÅrMistetFokus
    | UtløperIkkeToggled
    | UtløperMåned String
    | UtløperÅr String
    | UtløperÅrMistetFokus


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        TypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadModel ->
                    updateSamtaleTypeahead model typeaheadMsg typeaheadModel

                EndreOpplysninger typeaheadModel skjema ->
                    let
                        ( nyTypeaheadModel, getSuggestionsStatus, _ ) =
                            Typeahead.update SertifikatTypeahead.label typeaheadMsg typeaheadModel
                    in
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> tilSertifikatFelt
                            |> Skjema.oppdaterSertifikat skjema
                            |> EndreOpplysninger nyTypeaheadModel
                            |> oppdaterSamtaleSteg model
                        , case getSuggestionsStatus of
                            GetSuggestionsForInput query ->
                                Api.getSertifikatTypeahead HentetTypeahead query

                            DoNothing ->
                                Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetTypeahead result ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadModel ->
                    case result of
                        Ok suggestions ->
                            ( suggestions
                                |> Typeahead.updateSuggestions SertifikatTypeahead.label typeaheadModel
                                |> RegistrerSertifikatFelt
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hent SertifikatTypeahead" )
                                |> IkkeFerdig

                EndreOpplysninger typeaheadModel skjema ->
                    case result of
                        Ok suggestions ->
                            ( EndreOpplysninger (Typeahead.updateSuggestions SertifikatTypeahead.label typeaheadModel suggestions) skjema
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hent SertifikatTypeahead" )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereSertifikat ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadModel ->
                    case Typeahead.selected typeaheadModel of
                        Just sertifikat ->
                            sertifikat
                                |> SertifikatFraTypeahead
                                |> brukerVelgerSertifikatFelt model

                        Nothing ->
                            typeaheadModel
                                |> Typeahead.inputValue
                                |> Egendefinert
                                |> brukerVelgerSertifikatFelt model

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

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
                EndreOpplysninger typeaheadModel sertifikatSkjema ->
                    ( sertifikatSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> EndreOpplysninger typeaheadModel
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilLagreEndretSkjema ->
            case model.aktivSamtale of
                EndreOpplysninger typeaheadModel skjema ->
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
                                |> Skjema.visAlleFeilmeldinger
                                |> EndreOpplysninger typeaheadModel
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
                                |> Skjema.encode
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


initSamtaleTypeahead : Typeahead.Model SertifikatTypeahead
initSamtaleTypeahead =
    Typeahead.init
        { value = ""
        , label = "Sertifisering eller sertifikat"
        , id = inputIdTilString SertifikatTypeaheadId
        , toString = SertifikatTypeahead.label
        }


initSkjemaTypeahead : ValidertSertifikatSkjema -> Typeahead.Model SertifikatTypeahead
initSkjemaTypeahead skjema =
    case Skjema.sertifikatFeltValidert skjema of
        SertifikatFraTypeahead sertifikatTypeahead ->
            Typeahead.initWithSelected
                { selected = sertifikatTypeahead
                , label = "Sertifisering eller sertifikat"
                , id = inputIdTilString SertifikatTypeaheadId
                , toString = SertifikatTypeahead.label
                }

        Egendefinert inputValue ->
            Typeahead.init
                { value = inputValue
                , label = "Sertifisering eller sertifikat"
                , id = inputIdTilString SertifikatTypeaheadId
                , toString = SertifikatTypeahead.label
                }


tilSertifikatFelt : Typeahead.Model SertifikatTypeahead -> SertifikatFelt
tilSertifikatFelt typeaheadModel =
    case Typeahead.selected typeaheadModel of
        Just sertifikatTypeahead ->
            SertifikatFraTypeahead sertifikatTypeahead

        Nothing ->
            Egendefinert (Typeahead.inputValue typeaheadModel)


updateSamtaleTypeahead : ModelInfo -> Typeahead.Msg SertifikatTypeahead -> Typeahead.Model SertifikatTypeahead -> SamtaleStatus
updateSamtaleTypeahead model msg typeaheadModel =
    let
        ( nyTypeaheadModel, getSuggestionsStatus, submitStatus ) =
            Typeahead.update SertifikatTypeahead.label msg typeaheadModel
    in
    case submitStatus of
        Typeahead.Submit ->
            case Typeahead.selected typeaheadModel of
                Just sertifikat ->
                    sertifikat
                        |> SertifikatFraTypeahead
                        |> brukerVelgerSertifikatFelt model

                Nothing ->
                    typeaheadModel
                        |> Typeahead.inputValue
                        |> Egendefinert
                        |> brukerVelgerSertifikatFelt model

        Typeahead.NoSubmit ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerSertifikatFelt
                    |> oppdaterSamtaleSteg model
                , case getSuggestionsStatus of
                    GetSuggestionsForInput string ->
                        Api.getSertifikatTypeahead HentetTypeahead string

                    DoNothing ->
                        Cmd.none
                )


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
            Skjema.oppdaterUtsteder skjema string

        FullførtMåned månedString ->
            månedString
                |> Dato.stringTilMåned
                |> Skjema.oppdaterFullførtMåned skjema

        FullførtÅr string ->
            Skjema.oppdaterFullførtÅr skjema string

        UtløperMåned månedString ->
            månedString
                |> Dato.stringTilMåned
                |> Skjema.oppdaterUtløperMåned skjema

        UtløperÅr string ->
            Skjema.oppdaterUtløperÅr skjema string

        UtløperIkkeToggled ->
            Skjema.toggleUtløperIkke skjema

        FullførtÅrMistetFokus ->
            Skjema.visFeilmeldingFullførtÅr skjema

        UtløperÅrMistetFokus ->
            Skjema.visFeilmeldingUtløperÅr skjema


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


brukerVelgerSertifikatFelt : ModelInfo -> SertifikatFelt -> SamtaleStatus
brukerVelgerSertifikatFelt info sertifikatFelt =
    ( sertifikatFelt
        |> sertifikatFeltTilUtsteder
        |> RegistrerUtsteder
        |> nesteSamtaleSteg info (Melding.svar [ sertifikatFeltTilString sertifikatFelt ])
    , lagtTilSpørsmålCmd info.debugStatus
    )
        |> IkkeFerdig


sertifikatFeltTilString : SertifikatFelt -> String
sertifikatFeltTilString sertifikatFelt =
    case sertifikatFelt of
        SertifikatFraTypeahead sertifikatTypeahead ->
            SertifikatTypeahead.label sertifikatTypeahead

        Egendefinert inputValue ->
            inputValue


updateEtterVilEndreSkjema : ModelInfo -> ValidertSertifikatSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model skjema =
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> EndreOpplysninger (initSkjemaTypeahead skjema)
        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
    , Cmd.batch
        [ lagtTilSpørsmålCmd model.debugStatus
        , skjema
            |> Skjema.sertifikatString
            |> Api.getSertifikatTypeahead HentetTypeahead
        ]
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

        EndreOpplysninger _ _ ->
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
            Skjema.tilUvalidertSkjema validertSkjema
    in
    [ "Sertifisering/sertifikat: " ++ (validertSkjema |> Skjema.sertifikatString)
    , "Utsteder: " ++ Skjema.utsteder skjema
    , "Fullført: " ++ Dato.datoTilString (Skjema.fullførtMåned skjema) (Skjema.fullførtÅrValidert validertSkjema)
    , "Utløper: " ++ utløpsdatoTilString (Skjema.utløpsdatoValidert validertSkjema)
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



--- VIEW ---


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


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadModel ->
                    Containers.typeaheadMedGåVidereKnapp VilRegistrereSertifikat
                        [ Typeahead.view SertifikatTypeahead.label Nothing typeaheadModel
                            |> Html.map TypeaheadMsg
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

                EndreOpplysninger typeaheadModel skjema ->
                    Containers.skjema { lagreMsg = VilLagreEndretSkjema, lagreKnappTekst = "Lagre endringer" }
                        [ Typeahead.view SertifikatTypeahead.label Nothing typeaheadModel
                            |> Html.map TypeaheadMsg
                        , skjema
                            |> Skjema.utsteder
                            |> Input.input { label = "Utsteder", msg = Utsteder >> SkjemaEndret }
                            |> Input.toHtml
                        , div [ class "DatoInput-fra-til-rad" ]
                            [ DatoInput.datoInput
                                { label = "Fullført"
                                , onMånedChange = FullførtMåned >> SkjemaEndret
                                , måned = Skjema.fullførtMåned skjema
                                , onÅrChange = FullførtÅr >> SkjemaEndret
                                , år = Skjema.fullførtÅr skjema
                                }
                                |> DatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingFullførtÅr skjema)
                                |> DatoInput.withOnBlurÅr (SkjemaEndret FullførtÅrMistetFokus)
                                |> DatoInput.toHtml
                            , if not (Skjema.utløperIkke skjema) then
                                DatoInput.datoInput
                                    { label = "Utløper"
                                    , onMånedChange = UtløperMåned >> SkjemaEndret
                                    , måned = Skjema.utløperMåned skjema
                                    , onÅrChange = UtløperÅr >> SkjemaEndret
                                    , år = Skjema.utløperÅr skjema
                                    }
                                    |> DatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingUtløperÅr skjema)
                                    |> DatoInput.withOnBlurÅr (SkjemaEndret UtløperÅrMistetFokus)
                                    |> DatoInput.toHtml

                              else
                                text ""
                            ]
                        , skjema
                            |> Skjema.utløperIkke
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


viewBekreftOppsummering : Html Msg
viewBekreftOppsummering =
    Containers.knapper Flytende
        [ Knapp.knapp VilLagreSertifikat "Ja, informasjonen er riktig"
            |> Knapp.toHtml
        , Knapp.knapp VilEndreOpplysninger "Nei, jeg vil endre"
            |> Knapp.toHtml
        ]


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


postEllerPutSertifikat : (Result Error (List Sertifikat) -> msg) -> Skjema.ValidertSertifikatSkjema -> Cmd msg
postEllerPutSertifikat msgConstructor skjema =
    case Skjema.id skjema of
        Just id ->
            Api.putSertifikat msgConstructor skjema id

        Nothing ->
            Api.postSertifikat msgConstructor skjema



--- INIT ---


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List Sertifikat -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg sertifikatListe =
    let
        aktivSamtale =
            RegistrerSertifikatFelt initSamtaleTypeahead
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
