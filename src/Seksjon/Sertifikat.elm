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
import Dato exposing (Måned(..), År)
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
import Skjema.Sertifikat as SertifikatSkjema exposing (SertifikatSkjema, TypeaheadFelt(..), ValidertSertifikatSkjema)
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


type Samtale
    = Intro
    | RegistrerSertifikat (TypeaheadState SertifikatFelt)
    | RegistrerUtsteder UtstederFelt
    | RegistrerFullførtMåned FullførtDatoFelt
    | RegistrerFullførtÅr FullførtDatoFelt
    | RegistrerUtløpsdato
    | RegistrerutløperMåned UtløpsdatoInfo
    | RegistrerUtløperÅr UtløpsdatoInfo
    | Oppsummering ValidertSertifikatSkjema
    | OppsummeringEtterEndring ValidertSertifikatSkjema
    | EndreOppsummering SertifikatSkjema
    | LagreSkjema ValidertSertifikatSkjema
    | LagringFeilet Http.Error ValidertSertifikatSkjema
    | LeggInnMer
    | VentePåAnimasjonFørFullføring (List Sertifikat)


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Sertifikat) FerdigAnimertMeldingsLogg


type alias FullførtDatoFelt =
    { forrigeFelt : UtstederFelt
    , måned : Måned
    , år : String
    , visFeilmeldingFullførtDato : Bool
    }


type alias UtstederFelt =
    { forrigeFelt : SertifikatFelt
    , utsteder : String
    }


type alias UtløpsdatoInfo =
    { måned : Måned
    , år : String
    }


sertifikatFeltTilUtsteder : SertifikatFelt -> UtstederFelt
sertifikatFeltTilUtsteder input =
    { forrigeFelt = input, utsteder = "" }


utstederTilFullførtDatoFelt : UtstederFelt -> FullførtDatoFelt
utstederTilFullførtDatoFelt input =
    { forrigeFelt = input
    , måned = Januar
    , år = ""
    , visFeilmeldingFullførtDato = False
    }



--UPDATE--


type Msg
    = VilRegistrereSertifikat
    | FerdigMedSertifikat String
    | VilOppdatereSertifikat String
    | VilRegistrereUtsteder
    | OppdatererUtsteder String
    | HentetTypeahead (Result Http.Error (List SertifikatFelt))
    | HovrerOverTypeaheadSuggestion SertifikatFelt
    | TrykkerTypeaheadTast Typeahead.Operation
    | VelgerSertifikatFraTypeahead SertifikatFelt
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
                |> RegistrerSertifikat
                |> nesteSamtaleSteg model (Melding.svar [ "Test123 Sertifisering/sertifikat" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        FerdigMedSertifikat knappeTekst ->
            case model.aktivSamtale of
                Intro ->
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
                RegistrerSertifikat typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue string
                                    |> RegistrerSertifikat
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
                RegistrerSertifikat typeaheadState ->
                    case result of
                        Ok suggestions ->
                            ( typeaheadState
                                |> TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> RegistrerSertifikat
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
                RegistrerSertifikat typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateActive typeahead
                                    |> RegistrerSertifikat
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        TrykkerTypeaheadTast operation ->
            case model.aktivSamtale of
                RegistrerSertifikat typeaheadState ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.arrowUp
                                            |> RegistrerSertifikat
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
                                            |> RegistrerSertifikat
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
                                            | aktivSamtale = RegistrerSertifikat typeaheadState
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
                                            |> RegistrerSertifikat
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
                            |> nesteSamtaleSteg model (Melding.svar [ "Ja, jeg vil informasjonen er riktig" ])
                        , Cmd.batch
                            [ Api.postSertifikat SertifikatLagret skjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                OppsummeringEtterEndring skjema ->
                    IkkeFerdig
                        ( skjema
                            |> LagreSkjema
                            |> nesteSamtaleSteg model (Melding.svar [ "Ja, jeg vil informasjonen er riktig" ])
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
                RegistrerSertifikat _ ->
                    brukerVelgerSertifikatFelt model typeahead

                EndreOppsummering _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        VilRegistrereUtsteder ->
            case model.aktivSamtale of
                RegistrerUtsteder verdi ->
                    ( verdi
                        |> utstederTilFullførtDatoFelt
                        |> RegistrerFullførtMåned
                        |> nesteSamtaleSteg model (Melding.svar [ verdi.utsteder ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        OppdatererUtsteder string ->
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
        Intro ->
            []

        RegistrerSertifikat _ ->
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

        RegistrerUtløpsdato ->
            [ Melding.spørsmål [ "Har sertifiseringen en utløpsdato?" ]
            ]

        RegistrerutløperMåned _ ->
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
        |> Typeahead.typeahead { label = "Hvilken stilling/yrke har du?", onInput = VilOppdatereSertifikat, onTypeaheadChange = TrykkerTypeaheadTast }
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
                RegistrerSertifikat typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ viewTypeaheadSertifikatFelt typeaheadState
                            ]
                        ]

                RegistrerUtsteder input ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ input.utsteder
                                |> Input.input { label = "Utsteder", msg = OppdatererUtsteder }
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

                Intro ->
                    text ""

                RegistrerFullførtMåned fullførtDatoInfo ->
                    text ""

                RegistrerFullførtÅr fullførtDatoInfo ->
                    text ""

                RegistrerUtløpsdato ->
                    text ""

                RegistrerutløperMåned utløpsdatoInfo ->
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



-- INIT --


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List Sertifikat -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg sertifikatListe =
    let
        aktivSamtale =
            Intro
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
