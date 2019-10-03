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
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp exposing (Enabled(..))
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
import TypeaheadState exposing (TypeaheadState)



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
    | EndreOppsummering SertifikatSkjema
    | OppsummeringEtterEndring ValidertSertifikatSkjema --TODO: rename til HarEndret?
    | LagrerSkjema ValidertSertifikatSkjema
    | LagringFeilet Http.Error ValidertSertifikatSkjema
    | LeggInnMer
    | VenterPåAnimasjonFørFullføring (List Sertifikat)


type SkjemaEndring
    = Utsteder String
    | FullførtMåned String
    | FullførtÅr String
    | UtløperMåned String
    | UtløperÅr String


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
    = VilRegistrereSertifikat
    | VilOppdatereSertifikat String
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
    | VilLagreEtterOppsumering
    | VilEndreOppsumering
    | SkjemaEndret SkjemaEndring
    | VilLagreEndretSkjema
    | SertifikatLagret (Result Http.Error (List Sertifikat))
    | FerdigMedSertifikat String
    | StartÅSkrive
    | FullførMelding
    | ViewportSatt (Result Dom.Error ())
    | FokusSatt (Result Dom.Error ())
    | ErrorLogget


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        VilRegistrereSertifikat ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerSertifikatFelt
                |> nesteSamtaleSteg model (Melding.svar [ "Test123 Sertifisering/sertifikat" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        HentetTypeahead result ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadState ->
                    case result of
                        Ok suggestions ->
                            ( typeaheadState
                                |> TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> RegistrerSertifikatFelt
                                |> oppdaterSamtaleSteg model
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente FagbrevTypeahead" )
                                |> IkkeFerdig

                EndreOppsummering skjema ->
                    case result of
                        Ok suggestions ->
                            ( TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> SertifikatSkjema.mapTypeaheadState skjema
                                |> EndreOppsummering
                                |> oppdaterSamtaleSteg model
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente AutorisasjonTypeahead" )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HovrerOverTypeaheadSuggestion typeahead ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateActive typeahead
                                    |> RegistrerSertifikatFelt
                        }
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
                            ( Model
                                { model
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.arrowUp
                                            |> RegistrerSertifikatFelt
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
                                            |> RegistrerSertifikatFelt
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            case TypeaheadState.getActive typeaheadState of
                                Just active ->
                                    brukerVelgerSertifikatFelt model active

                                Nothing ->
                                    ( Model
                                        { model
                                            | aktivSamtale = RegistrerSertifikatFelt typeaheadState
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
                                            |> RegistrerSertifikatFelt
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                EndreOppsummering skjema ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        TypeaheadState.arrowUp
                                            |> SertifikatSkjema.mapTypeaheadState skjema
                                            |> EndreOppsummering
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.ArrowDown ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        TypeaheadState.arrowDown
                                            |> SertifikatSkjema.mapTypeaheadState skjema
                                            |> EndreOppsummering
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        skjema
                                            |> SertifikatSkjema.velgAktivtSertifikatITypeahead
                                            |> EndreOppsummering
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.MouseLeaveSuggestions ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        TypeaheadState.removeActive
                                            |> SertifikatSkjema.mapTypeaheadState skjema
                                            |> EndreOppsummering
                                }
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

                EndreOppsummering _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
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
                    ( Model
                        { model
                            | aktivSamtale = RegistrerUtsteder { utsteder | utsteder = string }
                        }
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

        VilLagreEtterOppsumering ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    IkkeFerdig
                        ( skjema
                            |> LagrerSkjema
                            |> nesteSamtaleSteg model (Melding.svar [ "Ja, informasjonen er riktig" ])
                        , Cmd.batch
                            [ Api.postSertifikat SertifikatLagret skjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                OppsummeringEtterEndring skjema ->
                    IkkeFerdig
                        ( skjema
                            |> LagrerSkjema
                            |> nesteSamtaleSteg model (Melding.svar [ "Ja, informasjonen er riktig" ])
                        , Cmd.batch
                            [ Api.postSertifikat SertifikatLagret skjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilEndreOppsumering ->
            case model.aktivSamtale of
                VisOppsummering validertSertifikatSkjema ->
                    ( validertSertifikatSkjema
                        |> SertifikatSkjema.tilUvalidertSkjema
                        |> EndreOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                OppsummeringEtterEndring validertSertifikatSkjema ->
                    ( validertSertifikatSkjema
                        |> SertifikatSkjema.tilUvalidertSkjema
                        |> EndreOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreOppsummering sertifikatSkjema ->
                    ( sertifikatSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> EndreOppsummering
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilLagreEndretSkjema ->
            case model.aktivSamtale of
                EndreOppsummering skjema ->
                    case SertifikatSkjema.valider skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> OppsummeringEtterEndring
                                |> nesteSamtaleSteg model (Melding.svar (validertSkjemaTilSetninger validertSkjema))
                            , Cmd.batch
                                [ lagtTilSpørsmålCmd model.debugStatus
                                ]
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> SertifikatSkjema.gjørAlleFeilmeldingerSynlig
                                |> EndreOppsummering
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SertifikatLagret result ->
            case model.aktivSamtale of
                LagrerSkjema sertifikatSkjema ->
                    case result of
                        {-
                           Ok sertifikater ->
                               ( nesteSamtaleStegUtenMelding { model | sertifikatListe = sertifikater } LeggInnMer
                               , lagtTilSpørsmålCmd model.debugStatus
                               )
                        -}
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
                                |> nesteSamtaleSteg model (Melding.spørsmål [ "Noe gikk galt med lagringen" ])
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

                LeggInnMer ->
                    ( model.sertifikatListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilOppdatereSertifikat string ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue string
                                    |> RegistrerSertifikatFelt
                        }
                    , Cmd.batch
                        [ Api.getSertifikatTypeahead HentetTypeahead string
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                EndreOppsummering skjema ->
                    ( Model
                        { model
                            | aktivSamtale =
                                string
                                    |> SertifikatSkjema.oppdaterSertifikatFelt skjema
                                    |> EndreOppsummering
                        }
                    , Api.getSertifikatTypeahead HentetTypeahead string
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

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

        ViewportSatt result ->
            IkkeFerdig ( Model model, Cmd.none )

        FokusSatt result ->
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
                [ "Er du usikker på hvem som har ansvar for"
                    ++ "sertifiseringen? Det vil ofte stå på beviset ditt"
                ]
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

        EndreOppsummering _ ->
            []

        OppsummeringEtterEndring _ ->
            [ Melding.spørsmål [ "Er informasjonen riktig nå?" ] ]

        LagrerSkjema _ ->
            []

        LeggInnMer ->
            [ Melding.spørsmål
                [ "Nå er sertifiseringen din lagt til i CV-en!"
                ]
            , Melding.spørsmål
                [ "Vil du legge til flere kategorier?"
                ]
            ]

        LagringFeilet _ _ ->
            [ Melding.spørsmål
                [ "Oops... Jeg klarte ikke å lagre sertifikatet."
                    ++ " Vil du prøve på nytt?"
                ]
            ]

        -- hva er dette?--
        VenterPåAnimasjonFørFullføring list ->
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
        RegistrerUtsteder _ ->
            settFokusCmd UtstederInput

        RegistrerFullførtÅr _ ->
            settFokusCmd FullførtÅrInput

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
    = SertifikatTypeaheadInput
    | UtstederInput
    | FullførtÅrInput


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        UtstederInput ->
            "sertifikat-registrer-utsteder"

        FullførtÅrInput ->
            "sertifikat-registrer-fullført-år"

        SertifikatTypeaheadInput ->
            "sertifikat-felt-typeahead"


viewTypeaheadSertifikatFelt : TypeaheadState SertifikatTypeahead -> Html Msg
viewTypeaheadSertifikatFelt typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Sertifisering eller sertifikat", onInput = VilOppdatereSertifikat, onTypeaheadChange = TrykkerTypeaheadTast }
        |> Typeahead.withInputId (inputIdTilString SertifikatTypeaheadInput)
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSertifikatFelt typeaheadState)
        |> Typeahead.toHtml


typeaheadStateSuggestionsTilViewSertifikatFelt : TypeaheadState SertifikatTypeahead -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSertifikatFelt typeaheadState =
    typeaheadState
        |> TypeaheadState.map
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
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ input.utsteder
                                |> Input.input { label = "Utsteder", msg = OppdaterUtsteder }
                                |> Input.withOnEnter VilRegistrereUtsteder
                                |> Input.withId (inputIdTilString UtstederInput)
                                |> Input.toHtml
                            , Knapp.knapp VilRegistrereUtsteder "Gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

                RegistrerFullførtMåned _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ div [ class "knapperad-wrapper" ]
                                    [ lagFullførtMånedKnapp Januar
                                    , lagFullførtMånedKnapp Februar
                                    , lagFullførtMånedKnapp Mars
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagFullførtMånedKnapp April
                                    , lagFullførtMånedKnapp Mai
                                    , lagFullførtMånedKnapp Juni
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagFullførtMånedKnapp Juli
                                    , lagFullførtMånedKnapp August
                                    , lagFullførtMånedKnapp September
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagFullførtMånedKnapp Oktober
                                    , lagFullførtMånedKnapp November
                                    , lagFullførtMånedKnapp Desember
                                    ]
                                ]
                            ]
                        ]

                RegistrerFullførtÅr fullførtDatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema-int" ]
                            [ div [ class "inputkolonne" ]
                                [ fullførtDatoInfo.fullførtÅr
                                    |> Input.input { label = "År", msg = OppdatererFullførtÅr }
                                    |> Input.withClass Input.År
                                    |> Input.withOnEnter VilRegistrereFullførtÅr
                                    --|> Input.withOnBlur FraÅrMisterFokus
                                    |> Input.withId (inputIdTilString FullførtÅrInput)
                                    --|> Input.withMaybeFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue fullførtDatoInfo.visFeilmeldingFullførtDato) fullførtDatoInfo.fullførtÅr)
                                    |> Input.withMaybeFeilmelding
                                        (fullførtDatoInfo.fullførtÅr
                                            |> Dato.feilmeldingÅr
                                            |> maybeHvisTrue fullførtDatoInfo.visFeilmeldingFullførtÅr
                                        )
                                    |> Input.toHtml
                                , Knapp.knapp VilRegistrereFullførtÅr "Gå videre"
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                SpørOmUtløpsdatoFinnes _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ div [ class "inputkolonne-innhold" ]
                                    [ "Ja, sertifiseringen utløper"
                                        |> Knapp.knapp VilRegistrereUtløperMåned
                                        |> Knapp.withClass Knapp.SpråknivåKnapp
                                        --TODO: endre klasse
                                        |> Knapp.toHtml
                                    , "Nei, sertifiseringen utløper ikke"
                                        |> Knapp.knapp VilIkkeRegistrereUtløpesdato
                                        --|> Knapp.withClass Knapp.SpråknivåKnapp TODO: endre klasse
                                        |> Knapp.toHtml
                                    ]
                                ]
                            ]
                        ]

                RegistrerUtløperMåned _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ div [ class "knapperad-wrapper" ]
                                    [ lagMånedKnapp Januar UtløperMånedValgt
                                    , lagMånedKnapp Februar UtløperMånedValgt
                                    , lagMånedKnapp Mars UtløperMånedValgt
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagMånedKnapp April UtløperMånedValgt
                                    , lagMånedKnapp Mai UtløperMånedValgt
                                    , lagMånedKnapp Juni UtløperMånedValgt
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagMånedKnapp Juli UtløperMånedValgt
                                    , lagMånedKnapp August UtløperMånedValgt
                                    , lagMånedKnapp September UtløperMånedValgt
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ lagMånedKnapp Oktober UtløperMånedValgt
                                    , lagMånedKnapp November UtløperMånedValgt
                                    , lagMånedKnapp Desember UtløperMånedValgt
                                    ]
                                ]
                            ]
                        ]

                RegistrerUtløperÅr utløpsdatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema-int" ]
                            [ div [ class "inputkolonne" ]
                                [ utløpsdatoInfo.utløperÅr
                                    |> Input.input { label = "År", msg = OppdatererUtløperÅr }
                                    |> Input.withClass Input.År
                                    |> Input.withOnEnter VilRegistrereUtløperÅr
                                    --|> Input.withOnBlur FraÅrMisterFokus
                                    |> Input.withId (inputIdTilString FullførtÅrInput)
                                    --|> Input.withMaybeFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue fullførtDatoInfo.visFeilmeldingFullførtDato) fullførtDatoInfo.fullførtÅr)
                                    |> Input.withMaybeFeilmelding
                                        (utløpsdatoInfo.utløperÅr
                                            |> Dato.feilmeldingÅr
                                            |> maybeHvisTrue utløpsdatoInfo.visFeilmeldingUtløperÅr
                                        )
                                    |> Input.toHtml
                                , Knapp.knapp VilRegistrereUtløperÅr "Gå videre"
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                VisOppsummering _ ->
                    viewBekreftOppsummering

                OppsummeringEtterEndring _ ->
                    viewBekreftOppsummering

                EndreOppsummering skjema ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ case SertifikatSkjema.sertifikatTypeahed skjema of
                                SertifikatTypeahead sertifikat ->
                                    sertifikat
                                        |> SertifikatTypeahead.label
                                        |> Input.input { label = "Sertifisering", msg = VilOppdatereSertifikat }
                                        |> Input.toHtml

                                Typeahead typeaheadState ->
                                    viewTypeahead typeaheadState
                            , skjema
                                |> SertifikatSkjema.utsteder
                                |> Input.input { label = "Utsteder", msg = Utsteder >> SkjemaEndret }
                                |> Input.toHtml
                            , div [ class "DatoInput-fra-til-rad" ]
                                --todo:endre klasse
                                [ DatoInput.datoInput
                                    { label = "Fullført"
                                    , onMånedChange = FullførtMåned >> SkjemaEndret
                                    , måned = SertifikatSkjema.fullførtMåned skjema
                                    , onÅrChange = FullførtÅr >> SkjemaEndret
                                    , år = SertifikatSkjema.fullførtÅr skjema
                                    }
                                    |> DatoInput.withMaybeFeilmeldingÅr (SertifikatSkjema.feilmeldingFullførtÅr skjema)
                                    --|> DatoInput.withOnBlurÅr (SkjemaEndret FraÅrBlurred)
                                    |> DatoInput.toHtml
                                , DatoInput.datoInput
                                    { label = "Utløper"
                                    , onMånedChange = UtløperMåned >> SkjemaEndret
                                    , måned = SertifikatSkjema.utløperMåned skjema
                                    , onÅrChange = UtløperÅr >> SkjemaEndret
                                    , år = SertifikatSkjema.utløperÅr skjema
                                    }
                                    |> DatoInput.withMaybeFeilmeldingÅr (SertifikatSkjema.feilmeldingUtløperÅr skjema)
                                    --|> DatoInput.withOnBlurÅr (SkjemaEndret UtløperÅrBlurred)
                                    |> DatoInput.toHtml
                                ]
                            , div [ class "inputrad" ]
                                [ "Lagre endringer"
                                    |> Knapp.knapp VilLagreEndretSkjema
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                LeggInnMer ->
                    div [] []

                LagrerSkjema _ ->
                    div [] []

                LagringFeilet _ _ ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ Knapp.knapp VilLagreEndretSkjema "Ja, prøv på nytt"
                                |> Knapp.toHtml
                            , Knapp.knapp (FerdigMedSertifikat "Nei, gå videre") "Nei, gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

                VenterPåAnimasjonFørFullføring list ->
                    div [] []

        MeldingerGjenstår ->
            text ""


viewTypeahead : TypeaheadState SertifikatTypeahead -> Html Msg
viewTypeahead typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Sertifisering eller sertifikat", onInput = VilOppdatereSertifikat, onTypeaheadChange = TrykkerTypeaheadTast }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestion typeaheadState)
        |> Typeahead.withInputId (inputIdTilString SertifikatTypeaheadInput)
        |> Typeahead.toHtml


viewBekreftOppsummering : Html Msg
viewBekreftOppsummering =
    div [ class "skjema-wrapper" ]
        [ div [ class "skjema" ]
            [ div [ class "inputkolonne" ]
                [ Knapp.knapp VilLagreEtterOppsumering "Ja, informasjonen er riktig"
                    |> Knapp.toHtml
                , Knapp.knapp VilEndreOppsumering "Nei, jeg vil endre"
                    |> Knapp.toHtml
                ]
            ]
        ]


typeaheadStateSuggestionsTilViewSuggestion : TypeaheadState SertifikatTypeahead -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSuggestion typeaheadState =
    typeaheadState
        |> TypeaheadState.map
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


lagFullførtMånedKnapp : Dato.Måned -> Html Msg
lagFullførtMånedKnapp måned =
    måned
        |> Dato.månedTilString
        |> Knapp.knapp (FullførtMånedValgt måned)
        |> Knapp.withClass Knapp.MånedKnapp
        |> Knapp.toHtml


lagMånedKnapp : Dato.Måned -> (Dato.Måned -> Msg) -> Html Msg
lagMånedKnapp måned onClick =
    måned
        |> Dato.månedTilString
        |> Knapp.knapp (onClick måned)
        |> Knapp.withClass Knapp.MånedKnapp
        |> Knapp.toHtml


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
