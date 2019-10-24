module Fagdokumentasjon.Seksjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , initAutorisasjon
    , initFagbrev
    , initMesterbrev
    , meldingsLogg
    , subscriptions
    , update
    , viewBrukerInput
    )

import Api
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import Cv.Fagdokumentasjon exposing (Fagdokumentasjon, FagdokumentasjonType(..))
import DebugStatus exposing (DebugStatus)
import ErrorMelding exposing (OperasjonEtterError(..))
import Fagdokumentasjon.Skjema as Skjema exposing (FagdokumentasjonSkjema, ValidertFagdokumentasjonSkjema)
import Feilmelding
import FrontendModuler.Common as Common
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Textarea as Textarea
import Html exposing (Html, text)
import Http exposing (Error(..))
import Konsept exposing (Konsept)
import Melding exposing (Melding)
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import Task
import Typeahead.Typeahead as Typeahead exposing (GetSuggestionStatus(..), InputStatus(..))



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
    = RegistrerKonsept FagdokumentasjonType Bool (Typeahead.Model Konsept)
    | RegistrerBeskrivelse FagdokumentasjonType BeskrivelseInfo
    | Oppsummering ValidertFagdokumentasjonSkjema
    | EndrerOppsummering (Typeahead.Model Konsept) FagdokumentasjonSkjema
    | OppsummeringEtterEndring ValidertFagdokumentasjonSkjema
    | Lagrer LagreStatus ValidertFagdokumentasjonSkjema
    | LagringFeilet ValidertFagdokumentasjonSkjema Http.Error
    | VenterPåAnimasjonFørFullføring (List Fagdokumentasjon)


type LagreStatus
    = LagrerFørsteGang
    | LagrerPåNyttEtterError Http.Error
    | ForsøkÅLagrePåNyttEtterDetteForsøket


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
    = TypeaheadMsg (Typeahead.Msg Konsept)
    | HentetTypeahead (Result Http.Error (List Konsept))
    | BrukerVilRegistrereKonsept
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
    | WindowEndrerVisibility Visibility
    | FokusSatt (Result Dom.Error ())
    | ErrorLogget (Result Http.Error ())


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        TypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                RegistrerKonsept fagdokumentasjonType visFeilmelding typeaheadModel ->
                    updateSamtaleTypeahead model fagdokumentasjonType visFeilmelding typeaheadMsg typeaheadModel

                EndrerOppsummering typeaheadModel skjema ->
                    let
                        ( nyTypeaheadModel, status ) =
                            Typeahead.update Konsept.label typeaheadMsg typeaheadModel
                    in
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> Typeahead.selected
                            |> Skjema.oppdaterKonsept skjema
                            |> Skjema.gjørFeilmeldingKonseptSynlig (Typeahead.inputStatus status == InputBlurred)
                            |> EndrerOppsummering nyTypeaheadModel
                            |> oppdaterSamtaleSteg model
                        , case Typeahead.getSuggestionsStatus status of
                            GetSuggestionsForInput string ->
                                skjema
                                    |> Skjema.fagdokumentasjonType
                                    |> hentTypeaheadSuggestions string

                            DoNothing ->
                                Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetTypeahead result ->
            case model.aktivSamtale of
                RegistrerKonsept fagdokumentasjonType visFeilmelding typeaheadModel ->
                    case result of
                        Ok suggestions ->
                            ( let
                                nyTypeaheadModel =
                                    Typeahead.updateSuggestions Konsept.label typeaheadModel suggestions
                              in
                              nyTypeaheadModel
                                |> RegistrerKonsept fagdokumentasjonType visFeilmelding
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente FagbrevTypeahead" )
                                |> IkkeFerdig

                EndrerOppsummering typeaheadModel skjema ->
                    case result of
                        Ok suggestions ->
                            ( EndrerOppsummering (Typeahead.updateSuggestions Konsept.label typeaheadModel suggestions) skjema
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hente AutorisasjonTypeahead" )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereKonsept ->
            case model.aktivSamtale of
                RegistrerKonsept fagdokumentasjonType _ typeaheadModel ->
                    case Typeahead.selected typeaheadModel of
                        Just konsept ->
                            brukerVilRegistrereKonsept model fagdokumentasjonType konsept

                        Nothing ->
                            visFeilmeldingRegistrerKonsept model fagdokumentasjonType typeaheadModel

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

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
                    LagrerFørsteGang
                        |> updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                OppsummeringEtterEndring skjema ->
                    LagrerFørsteGang
                        |> updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilEndreOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering validertSkjema ->
                    initRedigeringAvValidertSkjema model validertSkjema

                OppsummeringEtterEndring validertSkjema ->
                    initRedigeringAvValidertSkjema model validertSkjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerLagrerSkjema ->
            case model.aktivSamtale of
                EndrerOppsummering typeaheadModel skjema ->
                    case Skjema.validertSkjema skjema of
                        Just validertSkjema ->
                            IkkeFerdig
                                ( validertSkjema
                                    |> OppsummeringEtterEndring
                                    |> nesteSamtaleSteg model
                                        (Melding.svar
                                            [ Skjema.konseptStringFraValidertSkjema validertSkjema
                                            , Skjema.beskrivelseFraValidertSkjema validertSkjema
                                            ]
                                        )
                                , lagtTilSpørsmålCmd model.debugStatus
                                )

                        Nothing ->
                            IkkeFerdig
                                ( skjema
                                    |> Skjema.gjørFeilmeldingKonseptSynlig True
                                    |> EndrerOppsummering typeaheadModel
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

                EndrerOppsummering typeaheadModel skjema ->
                    ( Model
                        { model
                            | aktivSamtale =
                                skjema
                                    |> Skjema.oppdaterBeskrivelse beskrivelse
                                    |> EndrerOppsummering typeaheadModel
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FagbrevSendtTilApi result ->
            case model.aktivSamtale of
                Lagrer lagreStatus skjema ->
                    case result of
                        Ok fagdokumentasjoner ->
                            if lagringFeiletTidligerePåGrunnAvInnlogging lagreStatus then
                                ( Model
                                    { model
                                        | aktivSamtale = VenterPåAnimasjonFørFullføring fagdokumentasjoner
                                        , seksjonsMeldingsLogg =
                                            model.seksjonsMeldingsLogg
                                                |> MeldingsLogg.leggTilSvar (Melding.svar [ "Ja, jeg vil logge inn" ])
                                                |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Da har jeg lagret det!" ] ]
                                    }
                                , lagtTilSpørsmålCmd model.debugStatus
                                )
                                    |> IkkeFerdig

                            else
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
                                , logFeilmelding error "Lagre fagbrev"
                                ]
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case model.aktivSamtale of
                        LagringFeilet skjema ((BadStatus 401) as error) ->
                            ( skjema
                                |> Lagrer (LagrerPåNyttEtterError error)
                                |> nesteSamtaleStegUtenSvar model
                            , Api.postFagdokumentasjon FagbrevSendtTilApi skjema
                            )
                                |> IkkeFerdig

                        Lagrer (LagrerPåNyttEtterError (BadStatus 401)) skjema ->
                            ( skjema
                                |> Lagrer ForsøkÅLagrePåNyttEtterDetteForsøket
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Hidden ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilPrøveÅLagrePåNytt ->
            case model.aktivSamtale of
                LagringFeilet skjema error ->
                    error
                        |> LagrerPåNyttEtterError
                        |> updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, prøv på nytt" ])

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


initRedigeringAvValidertSkjema : ModelInfo -> ValidertFagdokumentasjonSkjema -> SamtaleStatus
initRedigeringAvValidertSkjema model validertSkjema =
    ( validertSkjema
        |> Skjema.tilSkjema
        |> EndrerOppsummering (initSkjemaTypeaheadFraValidertSkjema validertSkjema)
        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
    , Cmd.batch
        [ lagtTilSpørsmålCmd model.debugStatus
        , validertSkjema
            |> Skjema.tilSkjema
            |> Skjema.fagdokumentasjonType
            |> hentTypeaheadSuggestions
                (validertSkjema
                    |> Skjema.konseptFraValidertSkjema
                    |> Konsept.label
                )
        ]
    )
        |> IkkeFerdig


initSamtaleTypeahead : FagdokumentasjonType -> Typeahead.Model Konsept
initSamtaleTypeahead fagdokumentasjonType =
    Typeahead.init
        { value = ""
        , label = typeaheadLabel fagdokumentasjonType
        , id = inputIdTilString RegistrerKonseptInput
        , toString = Konsept.label
        }


initSkjemaTypeaheadFraValidertSkjema : ValidertFagdokumentasjonSkjema -> Typeahead.Model Konsept
initSkjemaTypeaheadFraValidertSkjema skjema =
    skjema
        |> Skjema.konseptFraValidertSkjema
        |> initSkjemaTypeaheadFraKonsept (skjema |> Skjema.tilSkjema |> Skjema.fagdokumentasjonType)


initSkjemaTypeaheadFraKonsept : FagdokumentasjonType -> Konsept -> Typeahead.Model Konsept
initSkjemaTypeaheadFraKonsept fagdokumentasjonType konsept =
    Typeahead.initWithSelected
        { selected = konsept
        , label = typeaheadLabel fagdokumentasjonType
        , id = inputIdTilString RegistrerKonseptInput
        , toString = Konsept.label
        }


updateSamtaleTypeahead : ModelInfo -> FagdokumentasjonType -> Bool -> Typeahead.Msg Konsept -> Typeahead.Model Konsept -> SamtaleStatus
updateSamtaleTypeahead model fagdokumentasjonType visFeilmelding msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Konsept.label msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just konsept ->
                    brukerVilRegistrereKonsept model fagdokumentasjonType konsept

                Nothing ->
                    visFeilmeldingRegistrerKonsept model fagdokumentasjonType nyTypeaheadModel

        Typeahead.InputBlurred ->
            visFeilmeldingRegistrerKonsept model fagdokumentasjonType nyTypeaheadModel

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerKonsept fagdokumentasjonType visFeilmelding
                    |> oppdaterSamtaleSteg model
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput string ->
                        hentTypeaheadSuggestions string fagdokumentasjonType

                    DoNothing ->
                        Cmd.none
                )


visFeilmeldingRegistrerKonsept : ModelInfo -> FagdokumentasjonType -> Typeahead.Model Konsept -> SamtaleStatus
visFeilmeldingRegistrerKonsept model fagdokumentasjonType typeaheadModel =
    ( RegistrerKonsept fagdokumentasjonType True typeaheadModel
        |> oppdaterSamtaleSteg model
    , Cmd.none
    )
        |> IkkeFerdig


brukerVilRegistrereKonsept : ModelInfo -> FagdokumentasjonType -> Konsept -> SamtaleStatus
brukerVilRegistrereKonsept model fagdokumentasjonType konsept =
    ( konsept
        |> forrigetilBeskrivelseInfo
        |> RegistrerBeskrivelse fagdokumentasjonType
        |> nesteSamtaleSteg model (Melding.svar [ Konsept.label konsept ])
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


feilmeldingstekstIkkeValgtKonsept : FagdokumentasjonType -> String
feilmeldingstekstIkkeValgtKonsept fagdokumentasjonType =
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            "Velg et svennebrev/fagbrev fra listen med forslag som kommer opp"

        Mesterbrev ->
            "Velg et mesterbrev fra listen med forslag som kommer opp"

        Autorisasjon ->
            "Velg en autorisasjon fra listen med forslag som kommer opp"


hentTypeaheadSuggestions : String -> FagdokumentasjonType -> Cmd Msg
hentTypeaheadSuggestions query fagdokumentasjonType =
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            Api.getFagbrevTypeahead HentetTypeahead query

        Mesterbrev ->
            Api.getMesterbrevTypeahead HentetTypeahead query

        Autorisasjon ->
            Api.getAutorisasjonTypeahead HentetTypeahead query


updateEtterLagreKnappTrykket : ModelInfo -> ValidertFagdokumentasjonSkjema -> Melding -> LagreStatus -> SamtaleStatus
updateEtterLagreKnappTrykket model skjema svar lagreStatus =
    IkkeFerdig
        ( skjema
            |> Lagrer lagreStatus
            |> nesteSamtaleSteg model svar
        , Cmd.batch
            [ Api.postFagdokumentasjon FagbrevSendtTilApi skjema
            , lagtTilSpørsmålCmd model.debugStatus
            ]
        )


lagringFeiletTidligerePåGrunnAvInnlogging : LagreStatus -> Bool
lagringFeiletTidligerePåGrunnAvInnlogging lagreStatus =
    case lagreStatus of
        LagrerFørsteGang ->
            False

        LagrerPåNyttEtterError error ->
            ErrorMelding.errorOperasjon error == LoggInn

        ForsøkÅLagrePåNyttEtterDetteForsøket ->
            True


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
        RegistrerKonsept fagdokumentasjonType _ _ ->
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
                , Skjema.konseptStringFraValidertSkjema skjema
                , Skjema.beskrivelseFraValidertSkjema skjema
                , Melding.tomLinje
                , "Er informasjonen riktig?"
                ]
            ]

        OppsummeringEtterEndring _ ->
            [ Melding.spørsmål [ "Er informasjonen riktig nå?" ] ]

        LagringFeilet validertSkjema error ->
            [ ErrorMelding.errorMelding { operasjon = lagreOperasjonStringFraSkjema validertSkjema, error = error } ]

        VenterPåAnimasjonFørFullføring _ ->
            []

        Lagrer _ _ ->
            []

        EndrerOppsummering _ _ ->
            []


lagreOperasjonStringFraSkjema : ValidertFagdokumentasjonSkjema -> String
lagreOperasjonStringFraSkjema skjema =
    skjema
        |> Skjema.tilSkjema
        |> Skjema.fagdokumentasjonType
        |> lagreOperasjonStringFraFagdokumentasjonType


lagreOperasjonStringFraFagdokumentasjonType : FagdokumentasjonType -> String
lagreOperasjonStringFraFagdokumentasjonType fagdokumentasjonType =
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            "lagre fagbrevet/svennebrevet"

        Mesterbrev ->
            "lagre mesterbrevet"

        Autorisasjon ->
            "lagre autorisasjonen"


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        RegistrerKonsept _ _ _ ->
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
                RegistrerKonsept fagdokumentasjonType visFeilmelding typeaheadModel ->
                    Containers.typeaheadMedGåVidereKnapp BrukerVilRegistrereKonsept
                        [ typeaheadModel
                            |> feilmeldingTypeahead fagdokumentasjonType
                            |> maybeHvisTrue visFeilmelding
                            |> Typeahead.view Konsept.label typeaheadModel
                            |> Html.map TypeaheadMsg
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

                EndrerOppsummering typeaheadModel skjema ->
                    Containers.skjema { lagreMsg = BrukerLagrerSkjema, lagreKnappTekst = "Lagre" }
                        [ skjema
                            |> Skjema.feilmeldingTypeahead
                            |> Typeahead.view Konsept.label typeaheadModel
                            |> Html.map TypeaheadMsg
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

                Lagrer _ _ ->
                    text ""

                LagringFeilet _ error ->
                    case ErrorMelding.errorOperasjon error of
                        GiOpp ->
                            Containers.knapper Flytende
                                [ Knapp.knapp BrukerVilIkkePrøveÅLagrePåNytt "Gå videre"
                                    |> Knapp.toHtml
                                ]

                        PrøvPåNytt ->
                            Containers.knapper Flytende
                                [ Knapp.knapp BrukerVilPrøveÅLagrePåNytt "Prøv på nytt"
                                    |> Knapp.toHtml
                                , Knapp.knapp BrukerVilIkkePrøveÅLagrePåNytt "Gå videre"
                                    |> Knapp.toHtml
                                ]

                        LoggInn ->
                            Common.viewLoggInnLenke

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


feilmeldingTypeahead : FagdokumentasjonType -> Typeahead.Model Konsept -> Maybe String
feilmeldingTypeahead fagdokumentasjonType typeaheadModel =
    case Typeahead.selected typeaheadModel of
        Just _ ->
            Nothing

        Nothing ->
            Just (feilmeldingstekstIkkeValgtKonsept fagdokumentasjonType)


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


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


typeaheadLabel : FagdokumentasjonType -> String
typeaheadLabel fagdokumentasjonType =
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            "Fagbrev/svennebrev"

        Mesterbrev ->
            "Mesterbrev"

        Autorisasjon ->
            "Autorisasjon"



--- INIT ---


init : FagdokumentasjonType -> DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
init fagdokumentasjonType debugStatus gammelMeldingsLogg fagdokumentasjonListe =
    let
        aktivSamtale =
            initSamtaleTypeahead fagdokumentasjonType
                |> RegistrerKonsept fagdokumentasjonType False
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onVisibilityChange WindowEndrerVisibility
