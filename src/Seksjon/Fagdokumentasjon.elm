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
import Cv.Fagdokumentasjon exposing (Fagdokumentasjon, FagdokumentasjonType(..))
import DebugStatus exposing (DebugStatus)
import Feilmelding
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
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



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , fagdokumentasjonListe : List Fagdokumentasjon
    , debugStatus : DebugStatus
    }


type Samtale
    = RegistrerKonsept FagdokumentasjonType (TypeaheadState Konsept)
    | RegistrerBeskrivelse FagdokumentasjonType BeskrivelseInfo
    | Oppsummering ValidertFagdokumentasjonSkjema
    | EndrerOppsummering FagdokumentasjonSkjema
    | OppsummeringEtterEndring ValidertFagdokumentasjonSkjema
    | Lagrer ValidertFagdokumentasjonSkjema
    | LagringFeilet ValidertFagdokumentasjonSkjema Http.Error
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



---UPDATE---


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
    | OppdaterFagdokumentasjonBeskrivelse String
    | BrukerVilLagreIOppsummeringen
    | BrukerVilEndreOppsummeringen
    | BrukerLagrerSkjema
    | FagbrevSendtTilApi (Result Http.Error (List Fagdokumentasjon))
    | BrukerVilPrøveÅLagrePåNytt
    | BrukerVilIkkePrøveÅLagrePåNytt
    | StartÅSkrive
    | FullførMelding
    | ViewportSatt (Result Dom.Error ())
    | FokusSatt (Result Dom.Error ())
    | ErrorLogget (Result Http.Error ())


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        BrukerVilRegistrereFagdokumentasjon ->
            case model.aktivSamtale of
                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FerdigMedFagdokumentasjon _ ->
            case model.aktivSamtale of
                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereFagbrev ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerKonsept SvennebrevFagbrev
                |> nesteSamtaleSteg model (Melding.svar [ "Registrer Fagbrev/Svennebrev" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilRegistrereMesterbrev ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerKonsept Mesterbrev
                |> nesteSamtaleSteg model (Melding.svar [ "Registrer Mesterbrev/Svennebrev" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilRegistrereAutorisasjon ->
            ( ""
                |> TypeaheadState.init
                |> RegistrerKonsept Autorisasjon
                |> nesteSamtaleSteg model (Melding.svar [ "Registrer en Autorisasjon" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerOppdatererFagdokumentasjon query ->
            case model.aktivSamtale of
                RegistrerKonsept fagdokumentasjonType typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateValue query
                                    |> RegistrerKonsept fagdokumentasjonType
                        }
                    , hentTypeaheadSuggestions query fagdokumentasjonType
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
                RegistrerKonsept fagdokumentasjonType typeaheadState ->
                    case result of
                        Ok suggestions ->
                            ( typeaheadState
                                |> TypeaheadState.updateSuggestions "" (List.take 10 suggestions)
                                |> RegistrerKonsept fagdokumentasjonType
                                |> oppdaterSamtaleSteg model
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente FagbrevTypeahead" )
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
                RegistrerKonsept fagdokumentasjonType typeaheadState ->
                    ( Model
                        { model
                            | aktivSamtale =
                                typeaheadState
                                    |> TypeaheadState.updateActive typeahead
                                    |> RegistrerKonsept fagdokumentasjonType
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerTrykkerTypeaheadTast operation ->
            case model.aktivSamtale of
                RegistrerKonsept fagdokumentasjonType typeaheadState ->
                    case operation of
                        Typeahead.ArrowUp ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        typeaheadState
                                            |> TypeaheadState.arrowUp
                                            |> RegistrerKonsept fagdokumentasjonType
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
                                            |> RegistrerKonsept fagdokumentasjonType
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Typeahead.Enter ->
                            case TypeaheadState.getActive typeaheadState of
                                Just active ->
                                    ( active
                                        |> forrigetilBeskrivelseInfo
                                        |> RegistrerBeskrivelse fagdokumentasjonType
                                        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label active ])
                                    , lagtTilSpørsmålCmd model.debugStatus
                                    )
                                        |> IkkeFerdig

                                Nothing ->
                                    ( Model
                                        { model
                                            | aktivSamtale = RegistrerKonsept fagdokumentasjonType typeaheadState
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
                                            |> RegistrerKonsept fagdokumentasjonType
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
                                KonseptIkkeValgt _ ->
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
                RegistrerKonsept fagdokumentasjonType _ ->
                    ( konsept
                        |> forrigetilBeskrivelseInfo
                        |> RegistrerBeskrivelse fagdokumentasjonType
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
                RegistrerBeskrivelse fagdokumentasjonType info ->
                    case feilmeldingBeskrivelsesfelt info.beskrivelse of
                        Nothing ->
                            ( Skjema.initValidertSkjema fagdokumentasjonType info.konsept info.beskrivelse
                                |> Oppsummering
                                |> nesteSamtaleSteg model (Melding.svar [ info.beskrivelse ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Just _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilLagreIOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                OppsummeringEtterEndring skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

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

        BrukerLagrerSkjema ->
            case model.aktivSamtale of
                EndrerOppsummering skjema ->
                    case Skjema.validertSkjema skjema of
                        Just validertSkjema ->
                            IkkeFerdig
                                ( validertSkjema
                                    |> OppsummeringEtterEndring
                                    |> nesteSamtaleSteg model
                                        (Melding.svar
                                            [ Skjema.konseptFraValidertSkjema validertSkjema
                                            , Skjema.beskrivelseFraValidertSkjema validertSkjema
                                            ]
                                        )
                                , lagtTilSpørsmålCmd model.debugStatus
                                )

                        Nothing ->
                            IkkeFerdig
                                ( skjema
                                    |> EndrerOppsummering
                                    |> oppdaterSamtaleSteg model
                                , Cmd.none
                                )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterFagdokumentasjonBeskrivelse beskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse fagdokumentasjonType info ->
                    ( oppdaterSamtaleSteg model (RegistrerBeskrivelse fagdokumentasjonType { info | beskrivelse = beskrivelse })
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
            case model.aktivSamtale of
                Lagrer skjema ->
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
                            ( error
                                |> LagringFeilet skjema
                                |> nesteSamtaleStegUtenSvar model
                            , Cmd.batch
                                [ lagtTilSpørsmålCmd model.debugStatus
                                , logFeilmelding error "Lagre fagbrev" --TODO: Trekk ut i funksjon
                                ]
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilPrøveÅLagrePåNytt ->
            case model.aktivSamtale of
                LagringFeilet skjema _ ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, prøv på nytt" ])

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilIkkePrøveÅLagrePåNytt ->
            IkkeFerdig
                ( model.fagdokumentasjonListe
                    |> VenterPåAnimasjonFørFullføring
                    |> nesteSamtaleSteg model (Melding.svar [ "Nei, gå videre uten å lagre" ])
                , lagtTilSpørsmålCmd model.debugStatus
                )

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

        ErrorLogget _ ->
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


updateEtterLagreKnappTrykket : ModelInfo -> ValidertFagdokumentasjonSkjema -> Melding -> SamtaleStatus
updateEtterLagreKnappTrykket model skjema svar =
    IkkeFerdig
        ( skjema
            |> Lagrer
            |> nesteSamtaleSteg model svar
        , Cmd.batch
            [ Api.postFagdokumentasjon FagbrevSendtTilApi skjema
            , lagtTilSpørsmålCmd model.debugStatus
            ]
        )


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


nesteSamtaleStegUtenSvar : ModelInfo -> Samtale -> Model
nesteSamtaleStegUtenSvar model samtaleSeksjon =
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
samtaleTilMeldingsLogg fagbrevSeksjon =
    case fagbrevSeksjon of
        RegistrerKonsept fagdokumentasjonType _ ->
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

        RegistrerBeskrivelse _ _ ->
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

        LagringFeilet _ _ ->
            [ Melding.spørsmål [ "Oops... Jeg klarte ikke å lagre. Vil du prøve lagre på nytt?" ] ]

        VenterPåAnimasjonFørFullføring _ ->
            []

        Lagrer _ ->
            []

        EndrerOppsummering _ ->
            []


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        RegistrerKonsept _ _ ->
            settFokusCmd RegistrerKonseptInput

        RegistrerBeskrivelse _ _ ->
            settFokusCmd RegistrerBeskrivelseInput

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    inputId
        |> inputIdTilString
        |> Dom.focus
        |> Task.attempt FokusSatt



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                RegistrerKonsept fagdokumentasjonType typeaheadState ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ viewTypeahead typeaheadState fagdokumentasjonType
                            ]
                        ]

                RegistrerBeskrivelse _ beskrivelseinfo ->
                    Containers.inputMedGåVidereKnapp BrukerVilRegistrereFagbrevBeskrivelse
                        [ beskrivelseinfo.beskrivelse
                            |> Textarea.textarea { msg = OppdaterFagdokumentasjonBeskrivelse, label = "Kort beskrivelse" }
                            |> Textarea.withMaybeFeilmelding (feilmeldingBeskrivelsesfelt beskrivelseinfo.beskrivelse)
                            |> Textarea.withId (inputIdTilString RegistrerBeskrivelseInput)
                            |> Textarea.toHtml
                        ]

                Oppsummering _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerVilLagreIOppsummeringen "Ja, informasjonen er riktig"
                            |> Knapp.toHtml
                        , Knapp.knapp BrukerVilEndreOppsummeringen "Nei, jeg vil endre"
                            |> Knapp.toHtml
                        ]

                EndrerOppsummering skjema ->
                    Containers.skjema { lagreMsg = BrukerLagrerSkjema, lagreKnappTekst = "Lagre" }
                        [ case Skjema.konsept skjema of
                            KonseptValgt konsept ->
                                konsept
                                    |> Konsept.label
                                    |> Input.input
                                        { msg = BrukerOppdatererFagdokumentasjon
                                        , label =
                                            skjema
                                                |> Skjema.fagdokumentasjonType
                                                |> typeaheadLabel
                                        }
                                    |> Input.toHtml

                            KonseptIkkeValgt typeaheadState ->
                                skjema
                                    |> Skjema.fagdokumentasjonType
                                    |> viewTypeahead typeaheadState
                        , skjema
                            |> Skjema.beskrivelse
                            |> Textarea.textarea { label = "Beskrivelse", msg = OppdaterFagdokumentasjonBeskrivelse }
                            |> Textarea.withMaybeFeilmelding (Skjema.beskrivelse skjema |> feilmeldingBeskrivelsesfelt)
                            |> Textarea.toHtml
                        ]

                OppsummeringEtterEndring _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerVilLagreIOppsummeringen "Ja, informasjonen er riktig"
                            |> Knapp.toHtml
                        , Knapp.knapp BrukerVilEndreOppsummeringen "Nei, jeg vil endre"
                            |> Knapp.toHtml
                        ]

                Lagrer _ ->
                    text ""

                LagringFeilet _ _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerVilPrøveÅLagrePåNytt "Ja, prøv på nytt"
                            |> Knapp.toHtml
                        , Knapp.knapp BrukerVilIkkePrøveÅLagrePåNytt "Nei, gå videre uten å lagre"
                            |> Knapp.toHtml
                        ]

                VenterPåAnimasjonFørFullføring _ ->
                    text ""

        MeldingerGjenstår ->
            text ""


type InputId
    = RegistrerKonseptInput
    | RegistrerBeskrivelseInput


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        RegistrerKonseptInput ->
            "fagdokumentasjon-registrer-konsept"

        RegistrerBeskrivelseInput ->
            "fagdokumentasjon-registrer-beskrivelse"


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


viewTypeahead : TypeaheadState Konsept -> FagdokumentasjonType -> Html Msg
viewTypeahead typeaheadState fagdokumentasjonType =
    typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = typeaheadLabel fagdokumentasjonType, onInput = BrukerOppdatererFagdokumentasjon, onTypeaheadChange = BrukerTrykkerTypeaheadTast, inputId = inputIdTilString RegistrerKonseptInput }
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



--- INIT ---


init : FagdokumentasjonType -> DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
init fagdokumentasjonType debugStatus gammelMeldingsLogg fagdokumentasjonListe =
    let
        aktivSamtale =
            ""
                |> TypeaheadState.init
                |> RegistrerKonsept fagdokumentasjonType
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


initFagbrev : DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initFagbrev =
    init SvennebrevFagbrev


initMesterbrev : DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initMesterbrev =
    init Mesterbrev


initAutorisasjon : DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initAutorisasjon =
    init Autorisasjon
