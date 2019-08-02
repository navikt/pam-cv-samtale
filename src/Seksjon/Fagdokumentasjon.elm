module Seksjon.Fagdokumentasjon exposing
    ( Model
    , ModelInfo
    , Msg
    , SamtaleStatus(..)
    , init
    , initAutorisasjon
    , initFagbrev
    , initMesterbrev
    , lagtTilSpørsmålCmd
    , meldingsLogg
    , update
    , viewBrukerInput
    )

import Api
import Browser.Dom as Dom
import Cv.Fagdokumentasjon as Fagdokumentasjon exposing (Fagdokumentasjon)
import Feilmelding
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Select as Select
import FrontendModuler.Typeahead as Typeahead
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Konsept exposing (Konsept)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import Skjema.Fagdokumentasjon as Skjema exposing (FagdokumentasjonSkjema)
import Task
import TypeaheadState exposing (TypeaheadState)


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , fagdokumentasjonListe : List Fagdokumentasjon
    }


type Samtale
    = Intro (List Fagdokumentasjon)
    | RegistrerType
    | RegistrerFagbrev (TypeaheadState Konsept)
    | RegistrerFagbrevBeskrivelse BeskrivelseInfo
    | RegistrerMesterbrev (TypeaheadState Konsept)
    | RegistrerMesterbrevBeskrivelse BeskrivelseInfo
    | RegistrerAutorisasjon (TypeaheadState Konsept)
    | RegistrerAutorisasjonBeskrivelse BeskrivelseInfo
    | FagdokumentasjonLagret
    | LagringFeilet Http.Error
    | LeggTilFlereFagdokumentasjoner Skjema.FagdokumentasjonSkjema
    | VenterPåAnimasjonFørFullføring (List Fagdokumentasjon)


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Fagdokumentasjon) FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type alias FagdokumentasjonTypeInfo =
    { fagdokumentasjontype : String }


type alias BeskrivelseInfo =
    { forrige : Konsept, beskrivelse : String }


forrigetilBeskrivelseInfo : Konsept -> BeskrivelseInfo
forrigetilBeskrivelseInfo konseptTypeahead =
    { forrige = konseptTypeahead, beskrivelse = "" }



--UPDATE--


type Msg
    = BrukerVilRegistrereFagdokumentasjon
    | FerdigMedFagdokumentasjon String
    | BrukerVilRegistrereFagbrev
    | BrukerOppdatererFagdokumentasjon String
    | HentetTypeahead (Result Http.Error (List Konsept))
    | BrukerHovrerOverTypeaheadSuggestion Konsept
    | BrukerVelgerKonsept Konsept
    | BrukerTrykkerTypeaheadTast Typeahead.Operation
    | BrukerVilRegistrereMesterbrev
    | BrukerVilRegistrereAutorisasjon
    | BrukerVilRegistrereFagbrevBeskrivelse
    | BrukerVilRegistrereMesterbrevBeskrivelse
    | BrukerVilRegistrereAutorisasjonBeskrivelse
    | OppdaterFagdokumentasjonBeskrivelse String
    | FagbrevSendtTilApi (Result Http.Error (List Fagdokumentasjon))
    | MesterbrevSendtTilApi (Result Http.Error (List Fagdokumentasjon))
    | AutorisasjonSendtTilApi (Result Http.Error (List Fagdokumentasjon))
    | StartÅSkrive
    | FullførMelding
    | ViewportSatt (Result Dom.Error ())
    | ErrorLogget (Result Http.Error ())


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        BrukerVilRegistrereFagdokumentasjon ->
            case model.aktivSamtale of
                Intro fagdokumentasjonListe ->
                    if List.isEmpty fagdokumentasjonListe then
                        IkkeFerdig
                            ( nesteSamtaleSteg model (Melding.svar [ "Jeg vil registrere fagbrev etc" ]) RegistrerType
                            , lagtTilSpørsmålCmd
                            )

                    else
                        IkkeFerdig
                            ( nesteSamtaleSteg model (Melding.svar [ "Jeg har enda flere fagbrev etc jeg ønsker å legge til " ]) RegistrerType
                            , lagtTilSpørsmålCmd
                            )

                LeggTilFlereFagdokumentasjoner _ ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Legg til flere" ]) RegistrerType
                        , lagtTilSpørsmålCmd
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FerdigMedFagdokumentasjon knappeTekst ->
            case model.aktivSamtale of
                Intro _ ->
                    ( model.fagdokumentasjonListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                LeggTilFlereFagdokumentasjoner _ ->
                    ( model.fagdokumentasjonListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereFagbrev ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerFagbrev
                |> nesteSamtaleSteg model (Melding.svar [ "Registrer Fagbrev/Svennebrev" ])
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        BrukerVilRegistrereMesterbrev ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerMesterbrev
                |> nesteSamtaleSteg model (Melding.svar [ "Registrer Mesterbrev/Svennebrev" ])
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        BrukerVilRegistrereAutorisasjon ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerAutorisasjon
                |> nesteSamtaleSteg model (Melding.svar [ "Registrer en Autorisasjon" ])
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        BrukerOppdatererFagdokumentasjon string ->
            case model.aktivSamtale of
                RegistrerFagbrev typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue string
                                    |> RegistrerFagbrev
                        }
                    , Cmd.batch
                        [ Api.getFagbrevTypeahead HentetTypeahead string
                        , lagtTilSpørsmålCmd
                        ]
                    )
                        |> IkkeFerdig

                RegistrerMesterbrev typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue string
                                    |> RegistrerMesterbrev
                        }
                    , Cmd.batch
                        [ Api.getMesterbrevTypeahead HentetTypeahead string
                        , lagtTilSpørsmålCmd
                        ]
                    )
                        |> IkkeFerdig

                RegistrerAutorisasjon typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue string
                                    |> RegistrerAutorisasjon
                        }
                    , Cmd.batch
                        [ Api.getAutorisasjonTypeahead HentetTypeahead string
                        , lagtTilSpørsmålCmd
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetTypeahead result ->
            case model.aktivSamtale of
                RegistrerFagbrev typeaheadState ->
                    case result of
                        Ok suggestions ->
                            ( typeaheadState
                                |> TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> RegistrerFagbrev
                                |> oppdaterSamtaleSteg model
                            , lagtTilSpørsmålCmd
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente FagbrevTypeahead" )
                                |> IkkeFerdig

                RegistrerMesterbrev typeaheadState ->
                    case result of
                        Ok suggestions ->
                            ( typeaheadState
                                |> TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> RegistrerMesterbrev
                                |> oppdaterSamtaleSteg model
                            , lagtTilSpørsmålCmd
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente MesterbrevTypeahead" )
                                |> IkkeFerdig

                RegistrerAutorisasjon typeaheadState ->
                    case result of
                        Ok suggestions ->
                            ( typeaheadState
                                |> TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> RegistrerAutorisasjon
                                |> oppdaterSamtaleSteg model
                            , lagtTilSpørsmålCmd
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente AutorisasjonTypeahead" )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerHovrerOverTypeaheadSuggestion typeahead ->
            case model.aktivSamtale of
                RegistrerFagbrev typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateActive typeahead
                                    |> RegistrerFagbrev
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerMesterbrev typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateActive typeahead
                                    |> RegistrerMesterbrev
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerAutorisasjon typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateActive typeahead
                                    |> RegistrerAutorisasjon
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykkerTypeaheadTast operation ->
            case model.aktivSamtale of
                RegistrerFagbrev typeaheadState ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.arrowUp
                                            |> RegistrerFagbrev
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
                                            |> RegistrerFagbrev
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            case TypeaheadState.getActive typeaheadState of
                                Just active ->
                                    ( active
                                        |> forrigetilBeskrivelseInfo
                                        |> RegistrerFagbrevBeskrivelse
                                        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label active ])
                                    , lagtTilSpørsmålCmd
                                    )
                                        |> IkkeFerdig

                                Nothing ->
                                    ( Model
                                        { model
                                            | aktivSamtale = RegistrerFagbrev typeaheadState
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
                                            |> RegistrerFagbrev
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                RegistrerMesterbrev typeaheadState ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.arrowUp
                                            |> RegistrerMesterbrev
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
                                            |> RegistrerMesterbrev
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            case TypeaheadState.getActive typeaheadState of
                                Just active ->
                                    ( active
                                        |> forrigetilBeskrivelseInfo
                                        |> RegistrerMesterbrevBeskrivelse
                                        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label active ])
                                    , lagtTilSpørsmålCmd
                                    )
                                        |> IkkeFerdig

                                Nothing ->
                                    ( Model
                                        { model
                                            | aktivSamtale = RegistrerMesterbrev typeaheadState
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
                                            |> RegistrerMesterbrev
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                RegistrerAutorisasjon typeaheadState ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.arrowUp
                                            |> RegistrerAutorisasjon
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
                                            |> RegistrerAutorisasjon
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            case TypeaheadState.getActive typeaheadState of
                                Just active ->
                                    ( active
                                        |> forrigetilBeskrivelseInfo
                                        |> RegistrerAutorisasjonBeskrivelse
                                        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label active ])
                                    , lagtTilSpørsmålCmd
                                    )
                                        |> IkkeFerdig

                                Nothing ->
                                    ( Model
                                        { model
                                            | aktivSamtale = RegistrerAutorisasjon typeaheadState
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
                                            |> RegistrerAutorisasjon
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVelgerKonsept typeahead ->
            case model.aktivSamtale of
                RegistrerFagbrev _ ->
                    ( typeahead
                        |> forrigetilBeskrivelseInfo
                        |> RegistrerFagbrevBeskrivelse
                        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label typeahead ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                RegistrerMesterbrev _ ->
                    ( typeahead
                        |> forrigetilBeskrivelseInfo
                        |> RegistrerMesterbrevBeskrivelse
                        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label typeahead ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                RegistrerAutorisasjon _ ->
                    ( typeahead
                        |> forrigetilBeskrivelseInfo
                        |> RegistrerAutorisasjonBeskrivelse
                        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label typeahead ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd )
                        |> IkkeFerdig

        BrukerVilRegistrereFagbrevBeskrivelse ->
            case model.aktivSamtale of
                RegistrerFagbrevBeskrivelse info ->
                    let
                        skjema =
                            Skjema.init Fagdokumentasjon.SvennebrevFagbrev (Konsept.label info.forrige) (Konsept.konseptId info.forrige) info.beskrivelse
                    in
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ info.beskrivelse ]) FagdokumentasjonLagret
                        , Cmd.batch
                            [ Api.postFagdokumentasjon FagbrevSendtTilApi skjema
                            , lagtTilSpørsmålCmd
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereAutorisasjonBeskrivelse ->
            case model.aktivSamtale of
                RegistrerAutorisasjonBeskrivelse info ->
                    let
                        skjema =
                            Skjema.init Fagdokumentasjon.Autorisasjon (Konsept.label info.forrige) (Konsept.konseptId info.forrige) info.beskrivelse
                    in
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ info.beskrivelse ]) FagdokumentasjonLagret
                        , Cmd.batch
                            [ Api.postFagdokumentasjon AutorisasjonSendtTilApi skjema
                            , lagtTilSpørsmålCmd
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereMesterbrevBeskrivelse ->
            case model.aktivSamtale of
                RegistrerMesterbrevBeskrivelse info ->
                    let
                        skjema =
                            Skjema.init Fagdokumentasjon.Mesterbrev (Konsept.label info.forrige) (Konsept.konseptId info.forrige) info.beskrivelse
                    in
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ info.beskrivelse ]) FagdokumentasjonLagret
                        , Cmd.batch
                            [ Api.postFagdokumentasjon MesterbrevSendtTilApi skjema
                            , lagtTilSpørsmålCmd
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterFagdokumentasjonBeskrivelse beskrivelse ->
            case model.aktivSamtale of
                RegistrerFagbrevBeskrivelse info ->
                    ( oppdaterSamtaleSteg model (RegistrerFagbrevBeskrivelse { info | beskrivelse = beskrivelse })
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerMesterbrevBeskrivelse info ->
                    ( oppdaterSamtaleSteg model (RegistrerMesterbrevBeskrivelse { info | beskrivelse = beskrivelse })
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerAutorisasjonBeskrivelse info ->
                    ( oppdaterSamtaleSteg model (RegistrerAutorisasjonBeskrivelse { info | beskrivelse = beskrivelse })
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FagbrevSendtTilApi result ->
            case result of
                Ok value ->
                    fullførSeksjonHvisMeldingsloggErFerdig { model | seksjonsMeldingsLogg = model.seksjonsMeldingsLogg |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Da har jeg lagret det!" ] ] } value

                Err error ->
                    ( nesteSamtaleSteg model (Melding.spørsmål [ "Oisann.. Klarte ikke å lagre det! La oss prøve på nytt" ]) RegistrerType
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

        MesterbrevSendtTilApi result ->
            case result of
                Ok value ->
                    fullførSeksjonHvisMeldingsloggErFerdig { model | seksjonsMeldingsLogg = model.seksjonsMeldingsLogg |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Da har jeg lagret det!" ] ] } value

                Err error ->
                    ( nesteSamtaleSteg model (Melding.spørsmål [ "Oisann.. Klarte ikke å lagre det! La oss prøve på nytt" ]) RegistrerType
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

        AutorisasjonSendtTilApi result ->
            case result of
                Ok value ->
                    fullførSeksjonHvisMeldingsloggErFerdig { model | seksjonsMeldingsLogg = model.seksjonsMeldingsLogg |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Da har jeg lagret det!" ] ] } value

                Err error ->
                    ( nesteSamtaleSteg model (Melding.spørsmål [ "Oisann.. Klarte ikke å lagre det! La oss prøve på nytt" ]) RegistrerType
                    , lagtTilSpørsmålCmd
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
                , Process.sleep 1000
                    |> Task.perform (\_ -> FullførMelding)
                ]
            )
                |> IkkeFerdig

        FullførMelding ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.fullførMelding
                |> updateEtterFullførtMelding model

        _ ->
            IkkeFerdig ( Model model, Cmd.none )


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring fagdokumentasjonListe ->
                    Ferdig fagdokumentasjonListe ferdigAnimertSamtale

                _ ->
                    ( Model
                        { model
                            | seksjonsMeldingsLogg =
                                nyMeldingsLogg
                        }
                    , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                    )
                        |> IkkeFerdig

        MeldingerGjenstår ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        nyMeldingsLogg
                }
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig


fullførSeksjonHvisMeldingsloggErFerdig : ModelInfo -> List Fagdokumentasjon -> SamtaleStatus
fullførSeksjonHvisMeldingsloggErFerdig modelInfo fagdokumentasjonListe =
    case MeldingsLogg.ferdigAnimert modelInfo.seksjonsMeldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            Ferdig fagdokumentasjonListe ferdigAnimertMeldingsLogg

        MeldingerGjenstår ->
            ( Model { modelInfo | aktivSamtale = VenterPåAnimasjonFørFullføring fagdokumentasjonListe }, Cmd.none )
                |> IkkeFerdig


lagtTilSpørsmålCmd : Cmd Msg
lagtTilSpørsmålCmd =
    Cmd.batch
        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
        , Process.sleep 200
            |> Task.perform (\_ -> StartÅSkrive)
        ]


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError ErrorLogget)
        |> Maybe.withDefault Cmd.none


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg model melding samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg fagbrevSeksjon =
    case fagbrevSeksjon of
        Intro fagdokumentasjonListe ->
            if List.isEmpty fagdokumentasjonListe then
                [ Melding.spørsmål
                    [ "Har du fagbrev, mesterbrev eller kanskje noen andre autorisasjoner?" ]
                ]

            else
                [ Melding.spørsmål [ "Jeg ser at du allerede har lagt inn et fagbrev, mesterbrev eller en autorisasjon. Har du flere du ønsker å ha med på CVen din?" ] ]

        RegistrerType ->
            [ Melding.spørsmål
                [ "Hvilken av de følgende typene vil du legge til? Du vil få muligheten til å legge til flere etterhvert." ]
            ]

        RegistrerFagbrev _ ->
            [ Melding.spørsmål
                [ "Hvilken type fagbrev var det? Begynn å skriv, så skal jeg komme med noen forslag til forskjellige fagbrev jeg kjenner til. Velg det som passer best :)" ]
            ]

        RegistrerMesterbrev _ ->
            [ Melding.spørsmål
                [ "Hvilken type mesterbrev var det? Begynn å skriv, så skal jeg komme med noen forslag til forskjellige mesterbrev jeg kjenner til. Velg det som passer best :)" ]
            ]

        RegistrerAutorisasjon _ ->
            [ Melding.spørsmål
                [ "Hvilken type autorisasjon var det? Begynn å skriv, så skal jeg komme med noen forslag til forskjellige autorisasjoner jeg kjenner til. Velg det som passer best :)" ]
            ]

        RegistrerFagbrevBeskrivelse _ ->
            [ Melding.spørsmål [ " Beskriv fagbrevet og hva du lærte" ] ]

        RegistrerMesterbrevBeskrivelse _ ->
            [ Melding.spørsmål [ " Beskriv mesterbrevet og hva du lærte" ] ]

        RegistrerAutorisasjonBeskrivelse _ ->
            [ Melding.spørsmål [ " Beskriv autorisasjonen og hva du lærte" ] ]

        _ ->
            []



-- View --


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                Intro _ ->
                    if List.isEmpty model.fagdokumentasjonListe then
                        div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ Knapp.knapp BrukerVilRegistrereFagdokumentasjon "Jeg vil registrere fagbrev eller lignende"
                                    |> Knapp.toHtml
                                , "Jeg har ingen utdanning"
                                    |> Knapp.knapp (FerdigMedFagdokumentasjon "Jeg har ingen fagbrev etc")
                                    |> Knapp.toHtml
                                ]
                            ]

                    else
                        div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ Knapp.knapp BrukerVilRegistrereFagdokumentasjon "Jeg vil legge til flere fagbrev etc"
                                    |> Knapp.toHtml
                                , "Jeg er ferdig med å legge til utdannelser"
                                    |> Knapp.knapp (FerdigMedFagdokumentasjon "Jeg er ferdig med å legge til fagbrev ol.")
                                    |> Knapp.toHtml
                                ]
                            ]

                RegistrerType ->
                    div [ class "inputrad" ]
                        [ div [ class "Utdanningsnivå" ]
                            [ Knapp.knapp BrukerVilRegistrereFagbrev "Fagbrev/Svennebrev" |> Knapp.toHtml
                            , Knapp.knapp BrukerVilRegistrereMesterbrev "Mesterbrev" |> Knapp.toHtml
                            , Knapp.knapp BrukerVilRegistrereAutorisasjon "Autorisasjon" |> Knapp.toHtml
                            ]
                        ]

                RegistrerFagbrev typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ viewTypeaheadRegistrerFagbrev typeaheadState
                        ]

                RegistrerMesterbrev typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ viewTypeaheadRegistrerMesterbrev typeaheadState
                        ]

                RegistrerAutorisasjon typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ viewTypeaheadRegistrerAutorisasjon typeaheadState
                        ]

                RegistrerFagbrevBeskrivelse beskrivelseinfo ->
                    div [ class "Skjema" ]
                        [ beskrivelseinfo.beskrivelse |> Input.input { msg = OppdaterFagdokumentasjonBeskrivelse, label = "" } |> Input.toHtml
                        , Knapp.knapp BrukerVilRegistrereFagbrevBeskrivelse "Lagre"
                            |> Knapp.toHtml
                        ]

                RegistrerMesterbrevBeskrivelse beskrivelseinfo ->
                    div [ class "Skjema" ]
                        [ beskrivelseinfo.beskrivelse |> Input.input { msg = OppdaterFagdokumentasjonBeskrivelse, label = "" } |> Input.toHtml
                        , Knapp.knapp BrukerVilRegistrereMesterbrevBeskrivelse "Lagre"
                            |> Knapp.toHtml
                        ]

                RegistrerAutorisasjonBeskrivelse beskrivelseinfo ->
                    div [ class "Skjema" ]
                        [ beskrivelseinfo.beskrivelse |> Input.input { msg = OppdaterFagdokumentasjonBeskrivelse, label = "" } |> Input.toHtml
                        , Knapp.knapp BrukerVilRegistrereAutorisasjonBeskrivelse "Lagre"
                            |> Knapp.toHtml
                        ]

                LeggTilFlereFagdokumentasjoner string ->
                    div [ class "inputrad" ]
                        [ div [ class "inputrad-innhold" ]
                            [ Knapp.knapp BrukerVilRegistrereFagdokumentasjon "Legg til flere"
                                |> Knapp.toHtml
                            , "Har ingen flere fagbrev å legge til"
                                |> Knapp.knapp (FerdigMedFagdokumentasjon "Jeg er ferdig med å legge til fagbrev etc")
                                |> Knapp.toHtml
                            ]
                        ]

                _ ->
                    text ""

        MeldingerGjenstår ->
            text ""


viewTypeaheadRegistrerFagbrev : TypeaheadState Konsept -> Html Msg
viewTypeaheadRegistrerFagbrev typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Fagbrev/Svennebrev", onInput = BrukerOppdatererFagdokumentasjon, onTypeaheadChange = BrukerTrykkerTypeaheadTast }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestion typeaheadState)
        |> Typeahead.toHtml


viewTypeaheadRegistrerMesterbrev : TypeaheadState Konsept -> Html Msg
viewTypeaheadRegistrerMesterbrev typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Mesterbrev", onInput = BrukerOppdatererFagdokumentasjon, onTypeaheadChange = BrukerTrykkerTypeaheadTast }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestion typeaheadState)
        |> Typeahead.toHtml


viewTypeaheadRegistrerAutorisasjon : TypeaheadState Konsept -> Html Msg
viewTypeaheadRegistrerAutorisasjon typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Autorisasjon", onInput = BrukerOppdatererFagdokumentasjon, onTypeaheadChange = BrukerTrykkerTypeaheadTast }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestion typeaheadState)
        |> Typeahead.toHtml


typeaheadStateSuggestionsTilViewSuggestion : TypeaheadState Konsept -> List (Typeahead.Suggestion Msg)
typeaheadStateSuggestionsTilViewSuggestion typeaheadState =
    typeaheadState
        |> TypeaheadState.map
            (\activeState suggestion ->
                { innhold = Konsept.label suggestion
                , onClick = BrukerVelgerKonsept suggestion
                , onActive = BrukerHovrerOverTypeaheadSuggestion suggestion
                , active =
                    case activeState of
                        TypeaheadState.Active ->
                            True

                        TypeaheadState.NotActive ->
                            False
                }
            )



-- INIT --


init : FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
init gammelMeldingsLogg fagdokumentasjonListe =
    let
        aktivSamtale =
            Intro fagdokumentasjonListe
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , fagdokumentasjonListe = fagdokumentasjonListe
        }
    , lagtTilSpørsmålCmd
    )


initFagbrev : FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initFagbrev gammelMeldingsLogg fagdokumentasjonListe =
    let
        aktivSamtale =
            ""
                |> TypeaheadState.init
                |> RegistrerFagbrev
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , fagdokumentasjonListe = fagdokumentasjonListe
        }
    , lagtTilSpørsmålCmd
    )


initMesterbrev : FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initMesterbrev gammelMeldingsLogg fagdokumentasjonListe =
    let
        aktivSamtale =
            ""
                |> TypeaheadState.init
                |> RegistrerMesterbrev
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , fagdokumentasjonListe = fagdokumentasjonListe
        }
    , lagtTilSpørsmålCmd
    )


initAutorisasjon : FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initAutorisasjon gammelMeldingsLogg fagdokumentasjonListe =
    let
        aktivSamtale =
            ""
                |> TypeaheadState.init
                |> RegistrerAutorisasjon
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , fagdokumentasjonListe = fagdokumentasjonListe
        }
    , lagtTilSpørsmålCmd
    )
