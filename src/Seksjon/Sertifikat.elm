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
import Dato exposing (Måned(..), År, stringTilÅr)
import DebugStatus exposing (DebugStatus)
import Feilmelding
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp exposing (Enabled(..))
import FrontendModuler.Typeahead as Typeahead
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import SertifikatFelt exposing (SertifikatFelt)
import Skjema.Sertifikat as SertifikatSkjema exposing (Felt(..), SertifikatSkjema, TypeaheadFelt(..), Utløpsdato(..), ValidertSertifikatSkjema)
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
    = RegistrerSertifikatFelt (TypeaheadState SertifikatFelt)
    | RegistrerUtsteder UtstederInfo
    | RegistrerFullførtMåned FullførtDatoInfo
    | RegistrerFullførtÅr FullførtDatoInfo
    | SpørOmUtløpsdatoFinnes ValidertFullførtDatoInfo
    | RegistrerUtløperMåned UtløperDatoInfo
    | RegistrerUtløperÅr UtløperDatoInfo
    | Oppsummering ValidertSertifikatSkjema
    | OppsummeringEtterEndring ValidertSertifikatSkjema
    | EndreOppsummering SertifikatSkjema
    | LagreSkjema ValidertSertifikatSkjema
    | LagringFeilet Http.Error ValidertSertifikatSkjema
    | LeggInnMer
    | VentePåAnimasjonFørFullføring (List Sertifikat)


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


type alias UtløperDatoInfo =
    { forrigeFeltInfo : ValidertFullførtDatoInfo
    , utløperMåned : Måned
    , utløperÅr : String
    , visFeilmeldingUtløperÅr : Bool
    }


sertifikatFeltTilUtsteder : SertifikatFelt -> UtstederInfo
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


validertFullførtDatoTilUtløperDato : ValidertFullførtDatoInfo -> UtløperDatoInfo
validertFullførtDatoTilUtløperDato input =
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
        }


utløperDatoTilSkjema : UtløperDatoInfo -> År -> ValidertSertifikatSkjema
utløperDatoTilSkjema info år =
    SertifikatSkjema.initValidertSkjema
        { sertifikatFelt = info.forrigeFeltInfo.sertifikat
        , utsteder = info.forrigeFeltInfo.utsteder
        , fullførtMåned = info.forrigeFeltInfo.fullførtMåned
        , fullførtÅr = info.forrigeFeltInfo.fullførtÅr
        , utløpsdato = Oppgitt info.utløperMåned år
        }



--UPDATE--


type Msg
    = VilRegistrereSertifikat
    | FerdigMedSertifikat String
    | VilOppdatereSertifikat String
    | HentetTypeahead (Result Http.Error (List SertifikatFelt))
    | HovrerOverTypeaheadSuggestion SertifikatFelt
    | TrykkerTypeaheadTast Typeahead.Operation
    | VelgerSertifikatFraTypeahead SertifikatFelt
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
    | VilLagreOppsumering
    | VilEndreOppsumering
    | VilLagreUvalidertSertifikatSkjema
    | SertifikatLagret (Result Http.Error (List Sertifikat))
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

        FerdigMedSertifikat knappeTekst ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt _ ->
                    ( model.sertifikatListe
                        |> VentePåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                LeggInnMer ->
                    ( model.sertifikatListe
                        |> VentePåAnimasjonFørFullføring
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

        VilLagreOppsumering ->
            case model.aktivSamtale of
                Oppsummering skjema ->
                    IkkeFerdig
                        ( skjema
                            |> LagreSkjema
                            |> nesteSamtaleSteg model (Melding.svar [ "Ja, informasjonen er riktig" ])
                        , Cmd.batch
                            [ Api.postSertifikat SertifikatLagret skjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                OppsummeringEtterEndring skjema ->
                    IkkeFerdig
                        ( skjema
                            |> LagreSkjema
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
                Oppsummering validertSertifikatSkjema ->
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

        SertifikatLagret result ->
            case model.aktivSamtale of
                LagreSkjema sertifikatSkjema ->
                    case result of
                        Ok sertifikater ->
                            ( oppdaterSamtalesteg { model | sertifikatListe = sertifikater } LeggInnMer
                            , Cmd.none
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

        VilLagreUvalidertSertifikatSkjema ->
            IkkeFerdig ( Model model, Cmd.none )

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

        UtløperMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerUtløperMåned utløperDatoInfo ->
                    ( { utløperDatoInfo | utløperMåned = måned }
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
                RegistrerUtløperÅr utløperDatoInfo ->
                    case Dato.stringTilÅr utløperDatoInfo.utløperÅr of
                        Just utløperÅr ->
                            ( utløperDatoTilSkjema utløperDatoInfo utløperÅr
                                |> Oppsummering
                                |> nesteSamtaleSteg model (Melding.svar [ utløperDatoInfo.utløperÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { utløperDatoInfo | visFeilmeldingUtløperÅr = True }
                                |> RegistrerUtløperÅr
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererUtløperÅr string ->
            case model.aktivSamtale of
                RegistrerUtløperÅr utløperDatoInfo ->
                    ( { utløperDatoInfo | utløperÅr = string }
                        |> RegistrerUtløperÅr
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilRegistrereUtløperMåned ->
            case model.aktivSamtale of
                SpørOmUtløpsdatoFinnes fullførtDatoInfo ->
                    ( fullførtDatoInfo
                        |> validertFullførtDatoTilUtløperDato
                        |> RegistrerUtløperMåned
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, sertifiseringen utløper" ])
                    , lagtTilSpørsmålCmd model.debugStatus
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
                        |> Oppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, sertifiseringen utløper ikke" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig


setFullførtMåned : FullførtDatoInfo -> Dato.Måned -> FullførtDatoInfo
setFullførtMåned fullførtDatoInfo måned =
    { fullførtDatoInfo | fullførtMåned = måned }


oppdaterSamtalesteg : ModelInfo -> Samtale -> Model
oppdaterSamtalesteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VentePåAnimasjonFørFullføring sertifikatListe ->
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
brukerVelgerSertifikatFelt info sertifikatTypeahead =
    ( sertifikatTypeahead
        |> sertifikatFeltTilUtsteder
        |> RegistrerUtsteder
        |> nesteSamtaleSteg info (Melding.svar [ SertifikatFelt.label sertifikatTypeahead ])
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

        Oppsummering skjema ->
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

        LagreSkjema _ ->
            [ Melding.spørsmål
                [ "Nå er sertifiseringen din lagt til i CV-en!"
                ]
            , Melding.spørsmål
                [ "Vil du legge til flere sertifiseringer? Velg kategorien Sertifikat/sertifisering igjen"
                ]
            , Melding.spørsmål
                [ "Vil du legge til flere kategorier?"
                ]
            ]

        LagringFeilet skjema _ ->
            [ Melding.spørsmål
                [ "Oops... Jeg klarte ikke å lagre sertifikatet."
                    ++ "Vil du prøve på nytt?"
                ]
            ]

        -- hva er dette?--
        VentePåAnimasjonFørFullføring list ->
            []

        OppsummeringEtterEndring validertSertifikatSkjema ->
            []

        EndreOppsummering sertifikatSkjema ->
            []

        LeggInnMer ->
            []


validertSkjemaTilSetninger : ValidertSertifikatSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    let
        skjema =
            SertifikatSkjema.tilUvalidertSkjema validertSkjema
    in
    [ Melding.tomLinje
    , "Sertifisering: "
    , "Utsteder: "
    , "Fullført: "
    , "Utløper: "
    ]


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


viewTypeaheadSertifikatFelt : TypeaheadState SertifikatFelt -> Html Msg
viewTypeaheadSertifikatFelt typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Sertifisering eller sertifikat", onInput = VilOppdatereSertifikat, onTypeaheadChange = TrykkerTypeaheadTast }
        |> Typeahead.withInputId (inputIdTilString SertifikatTypeaheadInput)
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSertifikatFelt typeaheadState)
        |> Typeahead.toHtml


typeaheadStateSuggestionsTilViewSertifikatFelt : TypeaheadState SertifikatFelt -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSertifikatFelt typeaheadState =
    typeaheadState
        |> TypeaheadState.map
            (\activeState suggestion ->
                { innhold = SertifikatFelt.label suggestion
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

                LeggInnMer ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ Knapp.knapp VilRegistrereSertifikat "Legg til flere"
                                |> Knapp.toHtml
                            , "Har ingen flere fagbrev å legge til"
                                |> Knapp.knapp (FerdigMedSertifikat "Jeg er ferdig med å legge til fagbrev etc")
                                |> Knapp.toHtml
                            ]
                        ]

                Oppsummering _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp VilLagreOppsumering "Ja, informasjonen er riktig"
                                    |> Knapp.toHtml
                                , Knapp.knapp VilEndreOppsumering "Nei, jeg vil endre"
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                OppsummeringEtterEndring _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp VilLagreOppsumering "Ja, informasjonen er riktig"
                                    |> Knapp.toHtml
                                , Knapp.knapp VilEndreOppsumering "Nei, jeg vil endre"
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                EndreOppsummering skjema ->
                    text ""

                {-
                   div [ class "skjema-wrapper" ]
                       [ div [ class "skjema" ]
                           [ case SertifikatSkjema skjema of
                               SertifikatFelt sertifikat ->
                                   sertifikat
                                       |> SertifikatFelt.label
                                      |> Input.input { label = "Stilling/yrke", msg = YrkeRedigeringsfeltEndret }
                                        |> Input.toHtml

                               Typeahead typeaheadState ->
                                   viewTypeaheadOppsummering typeaheadState
                           , skjema
                               |> SertifikatSkjema.beskrivelse
                               |> Textarea.textarea { label = "Beskrivelse", msg = OppdaterSertifikatBeskrivelse }
                               |> Textarea.withMaybeFeilmelding (SertifikatSkjema.beskrivelse skjema |> feilmeldingBeskrivelsesfelt)
                               |> Textarea.toHtml
                           , case SertifikatSkjema.validertSertifikatSkjema skjema of
                               Just validertSertifikatSkjema ->
                                   div [ class "inputkolonne" ]
                                       [ Knapp.knapp (LagrerSertifikatSkjema validertSertifikatSkjema) "Lagre"
                                           |> Knapp.toHtml
                                       ]

                               Nothing ->
                                   div [ class "inputkolonne" ]
                                       [ Knapp.knapp VilLagreUvalidertSertifikatSkjema "Lagre"
                                           |> Knapp.withEnabled Disabled
                                           |> Knapp.toHtml
                                       ]
                           ]
                       ]
                -}
                LagringFeilet _ _ ->
                    div [] []

                VentePåAnimasjonFørFullføring list ->
                    text ""

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
                                        --|> Knapp.withClass Knapp.SpråknivåKnapp TODO: endre klasse
                                        |> Knapp.toHtml
                                    , "Nei, sertifiseringen utløper ikke"
                                        |> Knapp.knapp VilIkkeRegistrereUtløpesdato
                                        --|> Knapp.withClass Knapp.SpråknivåKnapp TODO: endre klasse
                                        |> Knapp.toHtml
                                    ]
                                ]
                            ]
                        ]

                RegistrerUtløperMåned utløpsdatoInfo ->
                    text ""

                RegistrerUtløperÅr utløpsdatoInfo ->
                    text ""

                LagreSkjema validertSertifikatSkjema ->
                    text ""

        MeldingerGjenstår ->
            text ""


viewTypeahead : TypeaheadState SertifikatFelt -> Html Msg
viewTypeahead typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Sertifisering eller sertifikat", onInput = VilOppdatereSertifikat, onTypeaheadChange = TrykkerTypeaheadTast }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestion typeaheadState)
        |> Typeahead.withInputId (inputIdTilString SertifikatTypeaheadInput)
        |> Typeahead.toHtml


typeaheadStateSuggestionsTilViewSuggestion : TypeaheadState SertifikatFelt -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSuggestion typeaheadState =
    typeaheadState
        |> TypeaheadState.map
            (\activeState suggestion ->
                { innhold = SertifikatFelt.label suggestion
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


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing



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
