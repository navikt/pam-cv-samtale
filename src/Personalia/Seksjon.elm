module Personalia.Seksjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , subscriptions
    , update
    , viewBrukerInput
    )

import Api
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Feilmelding
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.BrukerInputMedGaVidereKnapp as BrukerInputMedGåVidereKnapp
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onInput)
import Http exposing (Error(..))
import Json.Encode
import LagreStatus exposing (LagreStatus)
import Meldinger.Melding as Melding exposing (Melding(..))
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Personalia.Personalia as Personalia exposing (Personalia)
import Personalia.PersonaliaId as PersonaliaId
import Personalia.Poststed exposing (Poststed)
import Personalia.Skjema as Skjema exposing (PersonaliaSkjema, ValidertPersonaliaSkjema)
import Process
import Task



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , personalia : Personalia
    , debugStatus : DebugStatus
    }


type BekreftPersonaliaState
    = OpprinneligPersonalia Personalia
    | EndretPersonalia ValidertPersonaliaSkjema
    | LagtTilMerKontaktInfo ValidertPersonaliaSkjema


type EndreStatus
    = EndrerFrivillig
    | EndrerPgaValideringsfeil


type Samtale
    = BekreftPersonalia BekreftPersonaliaState
    | EndrerPersonalia EndreStatus PersonaliaSkjema
    | LeggerTilEpost PersonaliaSkjema
    | LeggerTilTelefonnummer { harLagtTilEpost : Bool } PersonaliaSkjema
    | LagrerPersonalia ValidertPersonaliaSkjema LagreStatus
    | LagringFeilet Http.Error ValidertPersonaliaSkjema
    | VenterPåAnimasjonFørFullføring Personalia FullføringStatus


type FullføringStatus
    = LagringLyktesFørsteGang
    | LagringLyktesEtterFlereForsøk
    | BrukerGikkVidere


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Personalia FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



--- UPDATE ---


type Msg
    = BrukerVilEndrePersonalia
    | PersonaliaSkjemaEndret Skjema.Felt String
    | PersonaliaSkjemaFeltMistetFokus Skjema.Felt
    | PoststedHentet String (Result Http.Error Poststed)
    | PoststedfeltEndretSelvOmDetErDisabled String
    | PersonaliaskjemaLagreknappTrykket
    | EpostOppdatert String
    | EpostFeltMistetFokus
    | VilGåVidereMedEpost
    | TelefonnummerOppdatert String
    | TelefonnummerFeltMistetFokus
    | VilGåVidereMedTelefonnummer
    | PersonaliaOppdatert (Result Http.Error Personalia)
    | VilLagreBekreftetPersonalia
    | BrukerVilGåVidereUtenÅLagre
    | BrukerVilPrøveÅLagrePåNytt
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())
    | ErrorLogget


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        BrukerVilEndrePersonalia ->
            case model.aktivSamtale of
                BekreftPersonalia bekreftPersonaliaState ->
                    case bekreftPersonaliaState of
                        OpprinneligPersonalia personalia ->
                            ( personalia
                                |> Skjema.init
                                |> EndrerPersonalia EndrerFrivillig
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        EndretPersonalia validertSkjema ->
                            ( validertSkjema
                                |> Skjema.tilUvalidertSkjema
                                |> EndrerPersonalia EndrerFrivillig
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        LagtTilMerKontaktInfo validertSkjema ->
                            ( validertSkjema
                                |> Skjema.tilUvalidertSkjema
                                |> EndrerPersonalia EndrerFrivillig
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        PersonaliaSkjemaEndret felt string ->
            case model.aktivSamtale of
                EndrerPersonalia endreGrunn skjema ->
                    ( Skjema.oppdaterFelt felt skjema string
                        |> EndrerPersonalia endreGrunn
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , case felt of
                        Skjema.Postnummer ->
                            if String.length string == 4 && String.toInt string /= Nothing then
                                Api.hentPoststed (PoststedHentet string) string

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        PersonaliaSkjemaFeltMistetFokus felt ->
            case model.aktivSamtale of
                EndrerPersonalia endreGrunn skjema ->
                    ( skjema
                        |> Skjema.gjørFeilmeldingSynligForFelt felt
                        |> EndrerPersonalia endreGrunn
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        PoststedHentet postnummer result ->
            case result of
                Ok poststed ->
                    case model.aktivSamtale of
                        EndrerPersonalia endreGrunn skjema ->
                            ( skjema
                                |> Skjema.oppdaterPoststed poststed
                                |> EndrerPersonalia endreGrunn
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Err error ->
                    case error of
                        Http.BadStatus 404 ->
                            IkkeFerdig ( Model model, Cmd.none )

                        _ ->
                            IkkeFerdig
                                ( Model model
                                , error
                                    |> Feilmelding.feilmelding ("Hent poststed for postnummer: \"" ++ postnummer ++ "\"")
                                    |> Maybe.map (Api.logError (always ErrorLogget))
                                    |> Maybe.withDefault Cmd.none
                                )

        PoststedfeltEndretSelvOmDetErDisabled _ ->
            IkkeFerdig ( Model model, Cmd.none )

        PersonaliaskjemaLagreknappTrykket ->
            case model.aktivSamtale of
                EndrerPersonalia endreGrunn skjema ->
                    case Skjema.validerSkjema skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> EndretPersonalia
                                |> BekreftPersonalia
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar (personaliaSkjemaOppsummering skjema)))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> Skjema.gjørAlleFeilmeldingerSynlig
                                |> EndrerPersonalia endreGrunn
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        EpostOppdatert string ->
            case model.aktivSamtale of
                LeggerTilEpost skjema ->
                    ( string
                        |> Skjema.oppdaterFelt Skjema.Epost skjema
                        |> LeggerTilEpost
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        EpostFeltMistetFokus ->
            case model.aktivSamtale of
                LeggerTilEpost skjema ->
                    ( skjema
                        |> Skjema.gjørFeilmeldingSynligForFelt Skjema.Epost
                        |> LeggerTilEpost
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilGåVidereMedEpost ->
            case model.aktivSamtale of
                LeggerTilEpost skjema ->
                    case (Skjema.gjørFeilmeldingSynligForFelt Skjema.Epost >> Skjema.epostFeilmelding) skjema of
                        Just feilmelding ->
                            ( skjema
                                |> Skjema.gjørFeilmeldingSynligForFelt Skjema.Epost
                                |> LeggerTilEpost
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Nothing ->
                            if (Personalia.telefon >> Maybe.withDefault "" >> String.trim >> String.isEmpty) model.personalia then
                                ( skjema
                                    |> LeggerTilTelefonnummer { harLagtTilEpost = True }
                                    |> oppdaterSamtale model (SvarFraMsg msg)
                                , lagtTilSpørsmålCmd model.debugStatus
                                )
                                    |> IkkeFerdig

                            else
                                case Skjema.validerSkjema skjema of
                                    Just validertSkjema ->
                                        ( validertSkjema
                                            |> LagtTilMerKontaktInfo
                                            |> BekreftPersonalia
                                            |> oppdaterSamtale model (SvarFraMsg msg)
                                        , lagtTilSpørsmålCmd model.debugStatus
                                        )
                                            |> IkkeFerdig

                                    Nothing ->
                                        gåTilEndreSkjemaEtterValideringsFeil model msg skjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        TelefonnummerOppdatert string ->
            case model.aktivSamtale of
                LeggerTilTelefonnummer harLagtTilEpost skjema ->
                    ( string
                        |> Skjema.oppdaterFelt Skjema.Telefon skjema
                        |> LeggerTilTelefonnummer harLagtTilEpost
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        TelefonnummerFeltMistetFokus ->
            case model.aktivSamtale of
                LeggerTilTelefonnummer harLagtTilEpost skjema ->
                    ( skjema
                        |> Skjema.gjørFeilmeldingSynligForFelt Skjema.Telefon
                        |> LeggerTilTelefonnummer harLagtTilEpost
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilGåVidereMedTelefonnummer ->
            case model.aktivSamtale of
                LeggerTilTelefonnummer harLagtTilEpost skjema ->
                    case (Skjema.gjørFeilmeldingSynligForFelt Skjema.Telefon >> Skjema.telefonFeilmelding) skjema of
                        Just feilmelding ->
                            ( skjema
                                |> Skjema.gjørFeilmeldingSynligForFelt Skjema.Telefon
                                |> LeggerTilTelefonnummer harLagtTilEpost
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Nothing ->
                            case Skjema.validerSkjema skjema of
                                Just validertSkjema ->
                                    ( validertSkjema
                                        |> LagtTilMerKontaktInfo
                                        |> BekreftPersonalia
                                        |> oppdaterSamtale model (SvarFraMsg msg)
                                    , lagtTilSpørsmålCmd model.debugStatus
                                    )
                                        |> IkkeFerdig

                                Nothing ->
                                    gåTilEndreSkjemaEtterValideringsFeil model msg skjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        PersonaliaOppdatert result ->
            case model.aktivSamtale of
                LagrerPersonalia skjema lagreStatus ->
                    case result of
                        Ok personalia ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                lagreStatus
                                    |> fullførtStatusEtterOkLagring
                                    |> VenterPåAnimasjonFørFullføring personalia
                                    |> oppdaterSamtale model (SvarFraMsg msg)
                                    |> fullførSeksjonHvisMeldingsloggErFerdig personalia

                            else
                                lagreStatus
                                    |> fullførtStatusEtterOkLagring
                                    |> VenterPåAnimasjonFørFullføring personalia
                                    |> oppdaterSamtale model UtenSvar
                                    |> fullførSeksjonHvisMeldingsloggErFerdig personalia

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerPersonalia skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , Api.endrePersonalia PersonaliaOppdatert skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , Cmd.none
                                    )
                                        |> IkkeFerdig

                            else
                                ( skjema
                                    |> LagringFeilet error
                                    |> oppdaterSamtale model UtenSvar
                                , Cmd.batch
                                    [ lagtTilSpørsmålCmd model.debugStatus
                                    , model.personalia
                                        |> Personalia.id
                                        |> PersonaliaId.encode
                                        |> Tuple.pair "id"
                                        |> List.singleton
                                        |> Json.Encode.object
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre personalia" error
                                    ]
                                )
                                    |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreBekreftetPersonalia ->
            case model.aktivSamtale of
                LagringFeilet error feiletPersonalia ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerPersonalia feiletPersonalia
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , Cmd.batch
                        [ Api.endrePersonalia PersonaliaOppdatert feiletPersonalia
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                BekreftPersonalia bekreftPersonaliaState ->
                    case bekreftPersonaliaState of
                        OpprinneligPersonalia personalia ->
                            if (Personalia.epost >> Maybe.withDefault "" >> String.trim >> String.isEmpty) personalia then
                                ( personalia
                                    |> Skjema.init
                                    |> LeggerTilEpost
                                    |> oppdaterSamtale model (SvarFraMsg msg)
                                , lagtTilSpørsmålCmd model.debugStatus
                                )
                                    |> IkkeFerdig

                            else if (Personalia.telefon >> Maybe.withDefault "" >> String.trim >> String.isEmpty) model.personalia then
                                ( personalia
                                    |> Skjema.init
                                    |> LeggerTilTelefonnummer { harLagtTilEpost = False }
                                    |> oppdaterSamtale model (SvarFraMsg msg)
                                , lagtTilSpørsmålCmd model.debugStatus
                                )
                                    |> IkkeFerdig

                            else
                                LagringLyktesFørsteGang
                                    |> VenterPåAnimasjonFørFullføring model.personalia
                                    |> oppdaterSamtale model (SvarFraMsg msg)
                                    |> fullførSeksjonHvisMeldingsloggErFerdig model.personalia

                        EndretPersonalia validertSkjema ->
                            lagreSkjema model msg validertSkjema

                        LagtTilMerKontaktInfo validertSkjema ->
                            lagreSkjema model msg validertSkjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilPrøveÅLagrePåNytt ->
            case model.aktivSamtale of
                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerPersonalia skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , Cmd.batch
                        [ Api.endrePersonalia PersonaliaOppdatert skjema
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilGåVidereUtenÅLagre ->
            BrukerGikkVidere
                |> VenterPåAnimasjonFørFullføring model.personalia
                |> oppdaterSamtale model (SvarFraMsg msg)
                |> fullførSeksjonHvisMeldingsloggErFerdig model.personalia

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case model.aktivSamtale of
                        LagringFeilet error skjema ->
                            ( error
                                |> LagreStatus.fraError
                                |> LagrerPersonalia skjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Api.endrePersonalia PersonaliaOppdatert skjema
                            )
                                |> IkkeFerdig

                        LagrerPersonalia skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerPersonalia skjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Hidden ->
                    IkkeFerdig ( Model model, Cmd.none )

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            SamtaleAnimasjon.update model.debugStatus samtaleAnimasjonMsg model.seksjonsMeldingsLogg
                |> updateEtterFullførtMelding model

        FokusSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )


fullførtStatusEtterOkLagring : LagreStatus -> FullføringStatus
fullførtStatusEtterOkLagring lagreStatus =
    if LagreStatus.lagrerPåFørsteForsøk lagreStatus then
        LagringLyktesFørsteGang

    else
        LagringLyktesEtterFlereForsøk


lagreSkjema : ModelInfo -> Msg -> ValidertPersonaliaSkjema -> SamtaleStatus
lagreSkjema model msg validertSkjema =
    ( LagreStatus.init
        |> LagrerPersonalia validertSkjema
        |> oppdaterSamtale model (SvarFraMsg msg)
    , Cmd.batch
        [ Api.endrePersonalia PersonaliaOppdatert validertSkjema
        , lagtTilSpørsmålCmd model.debugStatus
        ]
    )
        |> IkkeFerdig


gåTilEndreSkjemaEtterValideringsFeil : ModelInfo -> Msg -> PersonaliaSkjema -> SamtaleStatus
gåTilEndreSkjemaEtterValideringsFeil model msg skjema =
    ( skjema
        |> Skjema.gjørAlleFeilmeldingerSynlig
        |> EndrerPersonalia EndrerPgaValideringsfeil
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding model ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring personalia _ ->
                    Ferdig personalia ferdigAnimertSamtale

                _ ->
                    ( Model { model | seksjonsMeldingsLogg = nyMeldingsLogg }
                    , Cmd.batch
                        [ Cmd.map SamtaleAnimasjonMsg cmd
                        , settFokus model.aktivSamtale
                        ]
                    )
                        |> IkkeFerdig

        MeldingerGjenstår ->
            ( Model { model | seksjonsMeldingsLogg = nyMeldingsLogg }
            , Cmd.map SamtaleAnimasjonMsg cmd
            )
                |> IkkeFerdig


fullførSeksjonHvisMeldingsloggErFerdig : Personalia -> Model -> SamtaleStatus
fullførSeksjonHvisMeldingsloggErFerdig personalia (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            Ferdig personalia ferdigAnimertMeldingsLogg

        MeldingerGjenstår ->
            ( Model model, lagtTilSpørsmålCmd model.debugStatus )
                |> IkkeFerdig


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        LeggerTilEpost _ ->
            settFokusCmd EpostId

        LeggerTilTelefonnummer _ _ ->
            settFokusCmd TelefonnummerId

        BekreftPersonalia _ ->
            settFokusCmd BekreftPersonaliaId

        EndrerPersonalia _ _ ->
            settFokusCmd FornavnId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
        |> Task.attempt FokusSatt


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


svarFraBrukerInput : ModelInfo -> Msg -> Melding
svarFraBrukerInput modelInfo msg =
    modelInfo
        |> modelTilBrukerInput
        |> BrukerInput.tilSvarMelding msg


oppdaterSamtale : ModelInfo -> SamtaleOppdatering Msg -> Samtale -> Model
oppdaterSamtale model meldingsoppdatering samtale =
    Model
        { model
            | aktivSamtale = samtale
            , seksjonsMeldingsLogg =
                case meldingsoppdatering of
                    IngenNyeMeldinger ->
                        model.seksjonsMeldingsLogg

                    SvarFraMsg msg ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (svarFraBrukerInput model msg)
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)

                    ManueltSvar melding ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar melding
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)

                    UtenSvar ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)
        }


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg personaliaSeksjon =
    case personaliaSeksjon of
        BekreftPersonalia bekreftState ->
            case bekreftState of
                OpprinneligPersonalia personalia ->
                    [ Melding.spørsmål [ "Da setter vi i gang 😊" ]
                    , Melding.spørsmål [ "Jeg har hentet inn kontaktinformasjonen din. Den vises på CV-en. Sjekk at den er riktig, slik at arbeidsgivere kan kontakte deg." ]
                    , Melding.spørsmål
                        (List.concat
                            [ personalia
                                |> Skjema.init
                                |> personaliaSkjemaOppsummering
                            , [ Melding.tomLinje
                              , "Er kontaktinformasjonen riktig?"
                              ]
                            ]
                        )
                    ]

                EndretPersonalia _ ->
                    [ Melding.spørsmål [ "Da har du endret 👍 Er det riktig nå?" ] ]

                LagtTilMerKontaktInfo validertSkjema ->
                    [ Melding.spørsmål
                        (List.concat
                            [ validertSkjema
                                |> Skjema.tilUvalidertSkjema
                                |> personaliaSkjemaOppsummering
                            , [ Melding.tomLinje
                              , "Er kontaktinformasjonen riktig nå?"
                              ]
                            ]
                        )
                    ]

        EndrerPersonalia endreGrunn _ ->
            case endreGrunn of
                EndrerFrivillig ->
                    [ Melding.spørsmål [ "Ok! Skriv inn riktig informasjon i feltene under." ] ]

                EndrerPgaValideringsfeil ->
                    [ Melding.spørsmål [ "Det skjedde en feil 😕 Du må rette feilen og lagre på nytt." ] ]

        LeggerTilEpost _ ->
            [ Melding.spørsmål [ "Jeg ser at vi mangler e-postadressen din. Kan du fylle inn den?" ] ]

        LeggerTilTelefonnummer { harLagtTilEpost } _ ->
            if harLagtTilEpost then
                [ Melding.spørsmål [ "Supert! Jeg trenger også telefonnummeret ditt. Da kan arbeidsgivere ringe deg." ] ]

            else
                [ Melding.spørsmål [ "Jeg ser at vi mangler telefonnummeret ditt. Kan du fylle inn det?" ] ]

        LagrerPersonalia _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { operasjon = "lagre kontaktinformasjonen", error = error } ]

        VenterPåAnimasjonFørFullføring _ fullføringStatus ->
            case fullføringStatus of
                LagringLyktesFørsteGang ->
                    [ Melding.spørsmål [ "Så bra! 😊 Nå kan arbeidsgivere kontakte deg." ]
                    , Melding.spørsmål [ "Da går vi videre til utdanning." ]
                    ]

                LagringLyktesEtterFlereForsøk ->
                    [ Melding.spørsmål [ "Supert! Nå fikk jeg det til. Kontaktinformasjonen er lagret. La oss fortsette 😊" ] ]

                BrukerGikkVidere ->
                    [ Melding.spørsmål [ "Da går vi videre." ] ]


personaliaSkjemaOppsummering : PersonaliaSkjema -> List String
personaliaSkjemaOppsummering skjema =
    [ "Navn: " ++ Skjema.fornavn skjema ++ " " ++ Skjema.etternavn skjema
    , "Fødselsdato: " ++ (skjema |> Skjema.fodselsdato |> viewDatoString)
    , "E-post: " ++ Skjema.epost skjema
    , "Telefonnummer: " ++ Skjema.telefon skjema
    , "Adresse: "
        ++ Skjema.gateadresse skjema
        ++ " "
        ++ Skjema.postnummer skjema
        ++ " "
        ++ Skjema.poststed skjema
    ]


viewDatoString : String -> String
viewDatoString datoString =
    case String.split "-" datoString of
        år :: måned :: dag :: [] ->
            dag ++ "." ++ måned ++ "." ++ år

        _ ->
            datoString



--- VIEW ---


type InputId
    = EpostId
    | TelefonnummerId
    | BekreftPersonaliaId
    | FornavnId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        EpostId ->
            "personalia-epost"

        TelefonnummerId ->
            "personalia-telefonnummer"

        BekreftPersonaliaId ->
            "personalia-bekreft-id"

        FornavnId ->
            "personalia-fornavn-id"


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput { aktivSamtale, seksjonsMeldingsLogg } =
    if MeldingsLogg.visBrukerInput seksjonsMeldingsLogg then
        case aktivSamtale of
            BekreftPersonalia _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilLagreBekreftetPersonalia "Ja, det er riktig"
                        |> Knapp.withId (inputIdTilString BekreftPersonaliaId)
                    , Knapp.knapp BrukerVilEndrePersonalia "Nei, jeg vil endre"
                    ]

            EndrerPersonalia _ personaliaSkjema ->
                BrukerInput.skjema { lagreMsg = PersonaliaskjemaLagreknappTrykket, lagreKnappTekst = "Lagre endringer" }
                    [ personaliaSkjema
                        |> Skjema.fornavn
                        |> Input.input { label = "Fornavn", msg = PersonaliaSkjemaEndret Skjema.Fornavn }
                        |> Input.withFeilmelding (Skjema.fornavnFeilmelding personaliaSkjema)
                        |> Input.withOnBlur (PersonaliaSkjemaFeltMistetFokus Skjema.Fornavn)
                        |> Input.withErObligatorisk
                        |> Input.withId (inputIdTilString FornavnId)
                        |> Input.toHtml
                    , personaliaSkjema
                        |> Skjema.etternavn
                        |> Input.input { label = "Etternavn", msg = PersonaliaSkjemaEndret Skjema.Etternavn }
                        |> Input.withFeilmelding (Skjema.etternavnFeilmelding personaliaSkjema)
                        |> Input.withOnBlur (PersonaliaSkjemaFeltMistetFokus Skjema.Etternavn)
                        |> Input.withErObligatorisk
                        |> Input.toHtml
                    , personaliaSkjema
                        |> Skjema.epost
                        |> Input.input { label = "E-post", msg = PersonaliaSkjemaEndret Skjema.Epost }
                        |> Input.withFeilmelding (Skjema.epostFeilmelding personaliaSkjema)
                        |> Input.withOnBlur (PersonaliaSkjemaFeltMistetFokus Skjema.Epost)
                        |> Input.withErObligatorisk
                        |> Input.toHtml
                    , viewTelefonISkjema personaliaSkjema
                    , personaliaSkjema
                        |> Skjema.gateadresse
                        |> Input.input { label = "Gateadresse", msg = PersonaliaSkjemaEndret Skjema.Gateadresse }
                        |> Input.toHtml
                    , div [ class "PersonaliaSeksjon-poststed" ]
                        [ personaliaSkjema
                            |> Skjema.postnummer
                            |> Input.input { label = "Postnummer", msg = PersonaliaSkjemaEndret Skjema.Postnummer }
                            |> Input.withFeilmelding (Skjema.postnummerFeilmelding personaliaSkjema)
                            |> Input.withOnBlur (PersonaliaSkjemaFeltMistetFokus Skjema.Postnummer)
                            |> Input.toHtml
                        , personaliaSkjema
                            |> Skjema.poststed
                            |> Input.input { label = "Poststed", msg = PoststedfeltEndretSelvOmDetErDisabled }
                            |> Input.withEnabled Input.Disabled
                            |> Input.toHtml
                        ]
                    ]

            LeggerTilEpost skjema ->
                skjema
                    |> Skjema.epost
                    |> Input.input { label = "E-postadresse", msg = EpostOppdatert }
                    |> Input.withErObligatorisk
                    |> Input.withFeilmelding (Skjema.epostFeilmelding skjema)
                    |> Input.withId (inputIdTilString EpostId)
                    |> Input.withOnBlur EpostFeltMistetFokus
                    |> Input.withOnEnter VilGåVidereMedEpost
                    |> BrukerInputMedGåVidereKnapp.input VilGåVidereMedEpost
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            LeggerTilTelefonnummer _ skjema ->
                skjema
                    |> Skjema.telefon
                    |> Input.input { label = "Telefonnummer", msg = TelefonnummerOppdatert }
                    |> Input.withErObligatorisk
                    |> Input.withFeilmelding (Skjema.telefonFeilmelding skjema)
                    |> Input.withId (inputIdTilString TelefonnummerId)
                    |> Input.withOnBlur TelefonnummerFeltMistetFokus
                    |> Input.withOnEnter VilGåVidereMedTelefonnummer
                    |> BrukerInputMedGåVidereKnapp.input VilGåVidereMedTelefonnummer
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            -- Lenken for å logge seg inn skal alltid være synlig hvis man har blitt utlogget, selv under lagring
            LagrerPersonalia _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    BrukerInput.utenInnhold

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilGåVidereUtenÅLagre "Gå videre"
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilPrøveÅLagrePåNytt "Prøv på nytt"
                            , Knapp.knapp BrukerVilGåVidereUtenÅLagre "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            VenterPåAnimasjonFørFullføring _ _ ->
                BrukerInput.utenInnhold

    else
        BrukerInput.utenInnhold


viewTelefonISkjema : PersonaliaSkjema -> Html Msg
viewTelefonISkjema personaliaSkjema =
    div [ class "skjemaelement" ]
        [ label []
            [ span [ class "skjemaelement__label" ]
                [ text "Telefon"
                , span [ class "skjemaelement__måFyllesUt" ] [ text " - må fylles ut" ]
                ]
            , div [ class "PersonaliaSeksjon--telefonnummer" ]
                [ p [ class "PersonaliaSeksjon--telefonnummer--country-code" ] [ text "+47" ]
                , input
                    [ value (Skjema.telefon personaliaSkjema)
                    , onInput (PersonaliaSkjemaEndret Skjema.Telefon)
                    , onBlur (PersonaliaSkjemaFeltMistetFokus Skjema.Telefon)
                    , classList
                        [ ( "skjemaelement__input", True )
                        , ( "PersonaliaSeksjon--telefonnummer--input", True )
                        , ( "skjemaelement__input--harFeil", Skjema.telefonFeilmelding personaliaSkjema /= Nothing )
                        ]
                    ]
                    []
                ]
            , case Skjema.telefonFeilmelding personaliaSkjema of
                Just feilmelding ->
                    div [ class "skjemaelement__feilmelding" ] [ text feilmelding ]

                Nothing ->
                    text ""
            ]
        ]



--- INIT ---


init : DebugStatus -> Personalia -> MeldingsLogg -> ( Model, Cmd Msg )
init debugStatus personalia gammelMeldingsLogg =
    let
        aktivSamtale =
            BekreftPersonalia (OpprinneligPersonalia personalia)
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål
                (samtaleTilMeldingsLogg aktivSamtale)
                gammelMeldingsLogg
        , aktivSamtale = aktivSamtale
        , personalia = personalia
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Browser.Events.onVisibilityChange WindowEndrerVisibility
        , model.seksjonsMeldingsLogg
            |> SamtaleAnimasjon.subscriptions
            |> Sub.map SamtaleAnimasjonMsg
        ]
