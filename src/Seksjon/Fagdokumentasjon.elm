module Seksjon.Fagdokumentasjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , initAutorisasjon
    , initFagbrev
    , initMesterbrev
    , meldingsLogg
    , update
    , viewBrukerInput
    )

import Api
import Browser.Dom as Dom
import Cv.Fagdokumentasjon as Fagdokumentasjon exposing (Fagdokumentasjon, FagdokumentasjonType(..))
import DebugStatus exposing (DebugStatus)
import Feilmelding
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp exposing (Enabled(..))
import FrontendModuler.Textarea as Textarea
import FrontendModuler.Typeahead as Typeahead
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Konsept exposing (Konsept)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import Skjema.Fagdokumentasjon as Skjema exposing (FagdokumentasjonSkjema, TypeaheadFelt(..), ValidertFagdokumentasjonSkjema)
import Task
import TypeaheadState exposing (TypeaheadState)


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , fagdokumentasjonListe : List Fagdokumentasjon
    , debugStatus : DebugStatus
    }


type Samtale
    = Intro (List Fagdokumentasjon)
    | RegistrerType
    | RegistrerFagbrev FagdokumentasjonType (TypeaheadState Konsept)
    | RegistrerFagbrevBeskrivelse FagdokumentasjonType BeskrivelseInfo
    | RegistrerMesterbrev (TypeaheadState Konsept)
    | RegistrerMesterbrevBeskrivelse BeskrivelseInfo
    | RegistrerAutorisasjon (TypeaheadState Konsept)
    | RegistrerAutorisasjonBeskrivelse BeskrivelseInfo
    | Oppsummering ValidertFagdokumentasjonSkjema
    | OppsummeringEtterEndring ValidertFagdokumentasjonSkjema
    | EndrerOppsummering FagdokumentasjonSkjema
    | Lagrer ValidertFagdokumentasjonSkjema
    | FagdokumentasjonLagret
    | LagringFeilet Http.Error
    | LeggTilFlereFagdokumentasjoner Skjema.ValidertFagdokumentasjonSkjema
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
    { konsept : Konsept
    , beskrivelse : String
    }


forrigetilBeskrivelseInfo : Konsept -> BeskrivelseInfo
forrigetilBeskrivelseInfo konseptTypeahead =
    { konsept = konseptTypeahead, beskrivelse = "" }



--UPDATE--


type Msg
    = BrukerVilRegistrereFagdokumentasjon
    | FerdigMedFagdokumentasjon String
    | BrukerVilRegistrereFagbrev
    | BrukerOppdatererFagdokumentasjon String
    | HentetTypeahead (Result Http.Error (List Konsept))
    | BrukerHovrerOverTypeaheadSuggestion Konsept
    | BrukerVelgerKonseptFraTypeahead Konsept
    | BrukerTrykkerTypeaheadTast Typeahead.Operation
    | BrukerVilRegistrereMesterbrev
    | BrukerVilRegistrereAutorisasjon
    | BrukerVilRegistrereFagbrevBeskrivelse
    | BrukerVilRegistrereMesterbrevBeskrivelse
    | BrukerVilRegistrereAutorisasjonBeskrivelse
    | OppdaterFagdokumentasjonBeskrivelse String
    | BrukerVilLagreIOppsummeringen
    | BrukerVilEndreOppsummeringen
    | BrukerLagrerSkjema ValidertFagdokumentasjonSkjema
    | BrukerVilLagrerUvalidertSkjema
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
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                    else
                        IkkeFerdig
                            ( nesteSamtaleSteg model (Melding.svar [ "Jeg har enda flere fagbrev etc jeg ønsker å legge til " ]) RegistrerType
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                LeggTilFlereFagdokumentasjoner _ ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Legg til flere" ]) RegistrerType
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FerdigMedFagdokumentasjon knappeTekst ->
            case model.aktivSamtale of
                Intro _ ->
                    ( model.fagdokumentasjonListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                LeggTilFlereFagdokumentasjoner _ ->
                    ( model.fagdokumentasjonListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereFagbrev ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerFagbrev SvennebrevFagbrev
                |> nesteSamtaleSteg model (Melding.svar [ "Registrer Fagbrev/Svennebrev" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilRegistrereMesterbrev ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerMesterbrev
                |> nesteSamtaleSteg model (Melding.svar [ "Registrer Mesterbrev/Svennebrev" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilRegistrereAutorisasjon ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerAutorisasjon
                |> nesteSamtaleSteg model (Melding.svar [ "Registrer en Autorisasjon" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerOppdatererFagdokumentasjon query ->
            case model.aktivSamtale of
                RegistrerFagbrev fagdokumentasjonType typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue query
                                    |> RegistrerFagbrev fagdokumentasjonType
                        }
                    , hentTypeaheadSuggestions query fagdokumentasjonType
                    )
                        |> IkkeFerdig

                RegistrerMesterbrev typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue query
                                    |> RegistrerMesterbrev
                        }
                    , Cmd.batch
                        [ Api.getMesterbrevTypeahead HentetTypeahead query
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                RegistrerAutorisasjon typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue query
                                    |> RegistrerAutorisasjon
                        }
                    , Cmd.batch
                        [ Api.getAutorisasjonTypeahead HentetTypeahead query
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                EndrerOppsummering skjema ->
                    ( Model
                        { model
                            | aktivSamtale =
                                query
                                    |> Skjema.oppdaterKonseptFelt skjema
                                    |> EndrerOppsummering
                        }
                    , skjema
                        |> Skjema.fagdokumentasjonType
                        |> hentTypeaheadSuggestions query
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetTypeahead result ->
            case model.aktivSamtale of
                RegistrerFagbrev fagdokumentasjonType typeaheadState ->
                    case result of
                        Ok suggestions ->
                            ( typeaheadState
                                |> TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> RegistrerFagbrev fagdokumentasjonType
                                |> oppdaterSamtaleSteg model
                            , lagtTilSpørsmålCmd model.debugStatus
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
                            , lagtTilSpørsmålCmd model.debugStatus
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
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente AutorisasjonTypeahead" )
                                |> IkkeFerdig

                EndrerOppsummering skjema ->
                    case result of
                        Ok suggestions ->
                            ( TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> Skjema.mapTypeaheadState skjema
                                |> EndrerOppsummering
                                |> oppdaterSamtaleSteg model
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente AutorisasjonTypeahead" )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerHovrerOverTypeaheadSuggestion typeahead ->
            case model.aktivSamtale of
                RegistrerFagbrev fagdokumentasjonType typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateActive typeahead
                                    |> RegistrerFagbrev fagdokumentasjonType
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
                RegistrerFagbrev fagdokumentasjonType typeaheadState ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.arrowUp
                                            |> RegistrerFagbrev fagdokumentasjonType
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
                                            |> RegistrerFagbrev fagdokumentasjonType
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            case TypeaheadState.getActive typeaheadState of
                                Just active ->
                                    ( active
                                        |> forrigetilBeskrivelseInfo
                                        |> RegistrerFagbrevBeskrivelse fagdokumentasjonType
                                        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label active ])
                                    , lagtTilSpørsmålCmd model.debugStatus
                                    )
                                        |> IkkeFerdig

                                Nothing ->
                                    ( Model
                                        { model
                                            | aktivSamtale = RegistrerFagbrev fagdokumentasjonType typeaheadState
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
                                            |> RegistrerFagbrev fagdokumentasjonType
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
                                    , lagtTilSpørsmålCmd model.debugStatus
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
                                    , lagtTilSpørsmålCmd model.debugStatus
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

                EndrerOppsummering skjema ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        TypeaheadState.arrowUp
                                            |> Skjema.mapTypeaheadState skjema
                                            |> EndrerOppsummering
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.ArrowDown ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        TypeaheadState.arrowDown
                                            |> Skjema.mapTypeaheadState skjema
                                            |> EndrerOppsummering
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            case Skjema.konsept skjema of
                                KonseptIkkeValgt typeaheadState ->
                                    ( Model
                                        { model
                                            | aktivSamtale =
                                                skjema
                                                    |> Skjema.velgAktivtKonseptITypeahead
                                                    |> EndrerOppsummering
                                        }
                                    , Cmd.none
                                    )
                                        |> IkkeFerdig

                                KonseptValgt _ ->
                                    IkkeFerdig ( Model model, Cmd.none )

                        Typeahead.MouseLeaveSuggestions ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        TypeaheadState.removeActive
                                            |> Skjema.mapTypeaheadState skjema
                                            |> EndrerOppsummering
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVelgerKonseptFraTypeahead konsept ->
            case model.aktivSamtale of
                RegistrerFagbrev fagdokumentasjonType _ ->
                    ( konsept
                        |> forrigetilBeskrivelseInfo
                        |> RegistrerFagbrevBeskrivelse fagdokumentasjonType
                        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label konsept ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                RegistrerMesterbrev _ ->
                    ( konsept
                        |> forrigetilBeskrivelseInfo
                        |> RegistrerMesterbrevBeskrivelse
                        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label konsept ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                RegistrerAutorisasjon _ ->
                    ( konsept
                        |> forrigetilBeskrivelseInfo
                        |> RegistrerAutorisasjonBeskrivelse
                        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label konsept ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                EndrerOppsummering skjema ->
                    ( Model
                        { model
                            | aktivSamtale =
                                skjema
                                    |> Skjema.velgKonsept konsept
                                    |> EndrerOppsummering
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

        BrukerVilRegistrereFagbrevBeskrivelse ->
            case model.aktivSamtale of
                RegistrerFagbrevBeskrivelse fagdokumentasjonType info ->
                    ( Skjema.initValidertSkjema fagdokumentasjonType info.konsept info.beskrivelse
                        |> Oppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ info.beskrivelse ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilLagreIOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering skjema ->
                    IkkeFerdig
                        ( skjema
                            |> Lagrer
                            |> nesteSamtaleSteg model (Melding.svar [ "Ja, jeg vil informasjonen er riktig" ])
                        , Cmd.batch
                            [ Api.postFagdokumentasjon FagbrevSendtTilApi skjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                OppsummeringEtterEndring skjema ->
                    IkkeFerdig
                        ( skjema
                            |> Lagrer
                            |> nesteSamtaleSteg model (Melding.svar [ "Ja, jeg vil informasjonen er riktig" ])
                        , Cmd.batch
                            [ Api.postFagdokumentasjon FagbrevSendtTilApi skjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilEndreOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering validertSkjema ->
                    ( validertSkjema
                        |> Skjema.tilSkjema
                        |> EndrerOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                OppsummeringEtterEndring validertSkjema ->
                    ( validertSkjema
                        |> Skjema.tilSkjema
                        |> EndrerOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerLagrerSkjema validertSkjema ->
            case model.aktivSamtale of
                EndrerOppsummering _ ->
                    IkkeFerdig
                        ( validertSkjema
                            |> OppsummeringEtterEndring
                            |> nesteSamtaleSteg model
                                (Melding.svar
                                    [ Skjema.konseptFraValidertSkjema validertSkjema
                                    , Skjema.beskrivelseFraValidertSkjema validertSkjema
                                    ]
                                )
                        , Cmd.batch
                            [ lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereAutorisasjonBeskrivelse ->
            case model.aktivSamtale of
                RegistrerAutorisasjonBeskrivelse info ->
                    let
                        skjema =
                            Skjema.initValidertSkjema Fagdokumentasjon.Autorisasjon info.konsept info.beskrivelse
                    in
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ info.beskrivelse ]) FagdokumentasjonLagret
                        , Cmd.batch
                            [ Api.postFagdokumentasjon AutorisasjonSendtTilApi skjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereMesterbrevBeskrivelse ->
            case model.aktivSamtale of
                RegistrerMesterbrevBeskrivelse info ->
                    let
                        skjema =
                            Skjema.initValidertSkjema Fagdokumentasjon.Mesterbrev info.konsept info.beskrivelse
                    in
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ info.beskrivelse ]) FagdokumentasjonLagret
                        , Cmd.batch
                            [ Api.postFagdokumentasjon MesterbrevSendtTilApi skjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterFagdokumentasjonBeskrivelse beskrivelse ->
            case model.aktivSamtale of
                RegistrerFagbrevBeskrivelse fagdokumentasjonType info ->
                    ( oppdaterSamtaleSteg model (RegistrerFagbrevBeskrivelse fagdokumentasjonType { info | beskrivelse = beskrivelse })
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

                EndrerOppsummering skjema ->
                    ( Model
                        { model
                            | aktivSamtale =
                                skjema
                                    |> Skjema.oppdaterBeskrivelse beskrivelse
                                    |> EndrerOppsummering
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FagbrevSendtTilApi result ->
            case result of
                Ok fagdokumentasjoner ->
                    ( Model
                        { model
                            | aktivSamtale = VenterPåAnimasjonFørFullføring fagdokumentasjoner
                            , seksjonsMeldingsLogg =
                                model.seksjonsMeldingsLogg
                                    |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Da har jeg lagret det!" ] ]
                        }
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Err error ->
                    ( nesteSamtaleSteg model (Melding.spørsmål [ "Oisann.. Klarte ikke å lagre det! La oss prøve på nytt" ]) RegistrerType
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd model.debugStatus
                        , logFeilmelding error "Lagre fagbrev"
                        ]
                    )
                        |> IkkeFerdig

        MesterbrevSendtTilApi result ->
            case result of
                Ok fagdokumentasjoner ->
                    ( Model
                        { model
                            | aktivSamtale = VenterPåAnimasjonFørFullføring fagdokumentasjoner
                            , seksjonsMeldingsLogg =
                                model.seksjonsMeldingsLogg
                                    |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Da har jeg lagret det!" ] ]
                        }
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Err error ->
                    ( nesteSamtaleSteg model (Melding.spørsmål [ "Oisann.. Klarte ikke å lagre det! La oss prøve på nytt" ]) RegistrerType
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd model.debugStatus
                        , logFeilmelding error "Lagre mesterbrev"
                        ]
                    )
                        |> IkkeFerdig

        AutorisasjonSendtTilApi result ->
            case result of
                Ok fagdokumentasjoner ->
                    ( Model
                        { model
                            | aktivSamtale = VenterPåAnimasjonFørFullføring fagdokumentasjoner
                            , seksjonsMeldingsLogg =
                                model.seksjonsMeldingsLogg
                                    |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Da har jeg lagret det!" ] ]
                        }
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Err error ->
                    ( nesteSamtaleSteg model (Melding.spørsmål [ "Oisann.. Klarte ikke å lagre det! La oss prøve på nytt" ]) RegistrerType
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd model.debugStatus
                        , logFeilmelding error "Lagre autorisasjon"
                        ]
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

        ErrorLogget result ->
            IkkeFerdig ( Model model, Cmd.none )

        BrukerVilLagrerUvalidertSkjema ->
            IkkeFerdig ( Model model, Cmd.none )


hentTypeaheadSuggestions : String -> FagdokumentasjonType -> Cmd Msg
hentTypeaheadSuggestions query fagdokumentasjonType =
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            Api.getFagbrevTypeahead HentetTypeahead query

        Mesterbrev ->
            Api.getMesterbrevTypeahead HentetTypeahead query

        Autorisasjon ->
            Api.getAutorisasjonTypeahead HentetTypeahead query


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
            , lagtTilSpørsmålCmd model.debugStatus
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

        RegistrerFagbrev fagdokumentasjonType _ ->
            case fagdokumentasjonType of
                SvennebrevFagbrev ->
                    [ Melding.spørsmål [ "Hva er navnet på fagbrevet/svennebrevet ditt?" ]
                    , Melding.spørsmål [ "Begynn å skrive inn fagbrevet/svennebrevet ditt. Velg et av forslagene fra listen som kommer opp." ]
                    ]

                Mesterbrev ->
                    [ Melding.spørsmål [ "Hva er navnet på mesterbrevet ditt?" ]
                    , Melding.spørsmål [ "Begynn å skrive inn mesterbrevet ditt. Velg fra listen med forslag som kommer opp." ]
                    ]

                Autorisasjon ->
                    [ Melding.spørsmål [ "Hva er navnet på autorisasjonen din?" ]
                    , Melding.spørsmål [ "Begynn å skrive inn autorisasjonen din. Velg fra listen med forslag som kommer opp." ]
                    ]

        RegistrerMesterbrev _ ->
            [ Melding.spørsmål [ "Hva er navnet på mesterbrevet ditt?" ]
            , Melding.spørsmål [ "Begynn å skrive inn mesterbrevet ditt. Velg fra listen med forslag som kommer opp." ]
            ]

        RegistrerAutorisasjon _ ->
            [ Melding.spørsmål [ "Hva er navnet på autorisasjonen din?" ]
            , Melding.spørsmål [ "Begynn å skrive inn autorisasjonen din. Velg fra listen med forslag som kommer opp." ]
            ]

        RegistrerFagbrevBeskrivelse fagdokumentasjonType _ ->
            [ Melding.spørsmål [ "Hva lærte du?" ] ]

        RegistrerMesterbrevBeskrivelse _ ->
            [ Melding.spørsmål [ "Hva lærte du?" ] ]

        RegistrerAutorisasjonBeskrivelse _ ->
            [ Melding.spørsmål [ "Hva lærte du?" ] ]

        Oppsummering skjema ->
            [ Melding.spørsmål
                [ "Du har lagt inn dette:"
                , Melding.tomLinje
                , Skjema.konseptFraValidertSkjema skjema
                , Skjema.beskrivelseFraValidertSkjema skjema
                , Melding.tomLinje
                , "Er informasjonen riktig?"
                ]
            ]

        OppsummeringEtterEndring _ ->
            [ Melding.spørsmål [ "Er informasjonen riktig nå?" ] ]

        FagdokumentasjonLagret ->
            []

        LagringFeilet error ->
            []

        LeggTilFlereFagdokumentasjoner fagdokumentasjonSkjema ->
            []

        VenterPåAnimasjonFørFullføring list ->
            []

        Lagrer fagdokumentasjonSkjema ->
            []

        EndrerOppsummering info ->
            []



-- View --


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                Intro _ ->
                    if List.isEmpty model.fagdokumentasjonListe then
                        div [ class "inputkolonne" ]
                            [ div [ class "inputkolonne-innhold" ]
                                [ Knapp.knapp BrukerVilRegistrereFagdokumentasjon "Jeg vil registrere fagbrev eller lignende"
                                    |> Knapp.toHtml
                                , "Jeg har ingen utdanning"
                                    |> Knapp.knapp (FerdigMedFagdokumentasjon "Jeg har ingen fagbrev etc")
                                    |> Knapp.toHtml
                                ]
                            ]

                    else
                        div [ class "inputkolonne" ]
                            [ div [ class "inputkolonne-innhold" ]
                                [ Knapp.knapp BrukerVilRegistrereFagdokumentasjon "Jeg vil legge til flere fagbrev etc"
                                    |> Knapp.toHtml
                                , "Jeg er ferdig med å legge til utdannelser"
                                    |> Knapp.knapp (FerdigMedFagdokumentasjon "Jeg er ferdig med å legge til fagbrev ol.")
                                    |> Knapp.toHtml
                                ]
                            ]

                RegistrerType ->
                    div [ class "inputkolonne" ]
                        [ div [ class "Utdanningsnivå" ]
                            [ Knapp.knapp BrukerVilRegistrereFagbrev "Fagbrev/Svennebrev" |> Knapp.toHtml
                            , Knapp.knapp BrukerVilRegistrereMesterbrev "Mesterbrev" |> Knapp.toHtml
                            , Knapp.knapp BrukerVilRegistrereAutorisasjon "Autorisasjon" |> Knapp.toHtml
                            ]
                        ]

                -- TODO fiks typeahead for alle
                RegistrerFagbrev fagdokumentasjonType typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ viewTypeahead fagdokumentasjonType typeaheadState
                            ]
                        ]

                RegistrerMesterbrev typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ viewTypeaheadRegistrerMesterbrev typeaheadState
                            ]
                        ]

                RegistrerAutorisasjon typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ viewTypeaheadRegistrerAutorisasjon typeaheadState
                            ]
                        ]

                RegistrerFagbrevBeskrivelse fagdokumentasjonType beskrivelseinfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ beskrivelseinfo.beskrivelse
                                |> Textarea.textarea { msg = OppdaterFagdokumentasjonBeskrivelse, label = "Kort beskrivelse" }
                                |> Textarea.withMaybeFeilmelding (feilmeldingBeskrivelsesfelt beskrivelseinfo.beskrivelse)
                                |> Textarea.toHtml
                            , Knapp.knapp BrukerVilRegistrereFagbrevBeskrivelse "Gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

                RegistrerMesterbrevBeskrivelse beskrivelseinfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ beskrivelseinfo.beskrivelse |> Textarea.textarea { msg = OppdaterFagdokumentasjonBeskrivelse, label = "" } |> Textarea.toHtml
                            , Knapp.knapp BrukerVilRegistrereMesterbrevBeskrivelse "Gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

                RegistrerAutorisasjonBeskrivelse beskrivelseinfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ beskrivelseinfo.beskrivelse |> Textarea.textarea { msg = OppdaterFagdokumentasjonBeskrivelse, label = "" } |> Textarea.toHtml
                            , Knapp.knapp BrukerVilRegistrereAutorisasjonBeskrivelse "Gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

                LeggTilFlereFagdokumentasjoner string ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ Knapp.knapp BrukerVilRegistrereFagdokumentasjon "Legg til flere"
                                |> Knapp.toHtml
                            , "Har ingen flere fagbrev å legge til"
                                |> Knapp.knapp (FerdigMedFagdokumentasjon "Jeg er ferdig med å legge til fagbrev etc")
                                |> Knapp.toHtml
                            ]
                        ]

                Oppsummering _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp BrukerVilLagreIOppsummeringen "Ja, informasjonen er riktig"
                                    |> Knapp.toHtml
                                , Knapp.knapp BrukerVilEndreOppsummeringen "Nei, jeg vil endre"
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                OppsummeringEtterEndring _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp BrukerVilLagreIOppsummeringen "Ja, informasjonen er riktig"
                                    |> Knapp.toHtml
                                , Knapp.knapp BrukerVilEndreOppsummeringen "Nei, jeg vil endre"
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                EndrerOppsummering skjema ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ case Skjema.konsept skjema of
                                KonseptValgt konsept ->
                                    konsept
                                        |> Konsept.label
                                        |> Input.input { label = "Fagbrev/svennebrev", msg = BrukerOppdatererFagdokumentasjon }
                                        |> Input.toHtml

                                KonseptIkkeValgt typeaheadState ->
                                    viewTypeaheadRegistrerFagbrev typeaheadState
                            , skjema
                                |> Skjema.beskrivelse
                                |> Textarea.textarea { label = "Beskrivelse", msg = OppdaterFagdokumentasjonBeskrivelse }
                                |> Textarea.toHtml
                            , case Skjema.validertSkjema skjema of
                                Just validertSkjema ->
                                    div [ class "inputkolonne" ]
                                        [ Knapp.knapp (BrukerLagrerSkjema validertSkjema) "Lagre"
                                            |> Knapp.toHtml
                                        ]

                                Nothing ->
                                    div [ class "inputkolonne" ]
                                        [ Knapp.knapp BrukerVilLagrerUvalidertSkjema "Lagre"
                                            |> Knapp.withEnabled Disabled
                                            |> Knapp.toHtml
                                        ]
                            ]
                        ]

                Lagrer fagdokumentasjonSkjema ->
                    text ""

                FagdokumentasjonLagret ->
                    text ""

                LagringFeilet error ->
                    text ""

                VenterPåAnimasjonFørFullføring list ->
                    text ""

        MeldingerGjenstår ->
            text ""


feilmeldingBeskrivelsesfelt : String -> Maybe String
feilmeldingBeskrivelsesfelt innhold =
    if String.length innhold <= 200 then
        Nothing

    else
        let
            tallTekst =
                (String.length innhold - 200)
                    |> String.fromInt
        in
        Just ("Du har " ++ tallTekst ++ " tegn for mye")


viewTypeahead : FagdokumentasjonType -> TypeaheadState Konsept -> Html Msg
viewTypeahead fagdokumentasjonType typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = typeaheadLabel fagdokumentasjonType, onInput = BrukerOppdatererFagdokumentasjon, onTypeaheadChange = BrukerTrykkerTypeaheadTast }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestion typeaheadState)
        |> Typeahead.toHtml


typeaheadLabel : FagdokumentasjonType -> String
typeaheadLabel fagdokumentasjonType =
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            "Fagbrev/svennebrev"

        Mesterbrev ->
            "Mesterbrev"

        Autorisasjon ->
            "Autorisasjon"


viewTypeaheadRegistrerFagbrev : TypeaheadState Konsept -> Html Msg
viewTypeaheadRegistrerFagbrev typeaheadState =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = "Fagbrev/svennebrev", onInput = BrukerOppdatererFagdokumentasjon, onTypeaheadChange = BrukerTrykkerTypeaheadTast }
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
                , onClick = BrukerVelgerKonseptFraTypeahead suggestion
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


initFagbrev : DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initFagbrev debugStatus gammelMeldingsLogg fagdokumentasjonListe =
    let
        aktivSamtale =
            ""
                |> TypeaheadState.init
                |> RegistrerFagbrev SvennebrevFagbrev
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , fagdokumentasjonListe = fagdokumentasjonListe
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )


initMesterbrev : DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initMesterbrev debugStatus gammelMeldingsLogg fagdokumentasjonListe =
    let
        aktivSamtale =
            ""
                |> TypeaheadState.init
                |> RegistrerFagbrev Mesterbrev
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , fagdokumentasjonListe = fagdokumentasjonListe
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )


initAutorisasjon : DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initAutorisasjon debugStatus gammelMeldingsLogg fagdokumentasjonListe =
    let
        aktivSamtale =
            ""
                |> TypeaheadState.init
                |> RegistrerFagbrev Autorisasjon
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , fagdokumentasjonListe = fagdokumentasjonListe
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )
