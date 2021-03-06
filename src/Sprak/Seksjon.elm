module Sprak.Seksjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , sistLagret
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
import FrontendModuler.Knapp as Knapp exposing (Knapp, Type(..))
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Select as Select
import Html exposing (..)
import Http
import LagreStatus exposing (LagreStatus)
import List.Extra as List
import Meldinger.Melding as Melding exposing (Melding)
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Process
import Sprak.Skjema as Skjema exposing (Ferdighet(..), SpråkSkjema)
import Sprak.Sprak as Språk exposing (Språk)
import Sprak.SprakKode as SpråkKode exposing (SpråkKode)
import String.Extra as String
import Task
import Tid exposing (nyesteSistLagretVerdi)
import Time exposing (Posix)



-- MODEL --


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , språk : List Språk
    , språkKoder : RemoteDataSpråkKoder
    , debugStatus : DebugStatus
    , sistLagretFraForrigeSeksjon : Posix
    }


sistLagret : Model -> Posix
sistLagret (Model model) =
    model.språk
        |> List.map Språk.sistEndretDato
        |> nyesteSistLagretVerdi model.sistLagretFraForrigeSeksjon


type Samtale
    = IntroLeggTilNorsk (List Språk)
    | LeggTilNorskMuntlig
    | LeggTilNorskSkriftlig Ferdighet
    | LagrerNorsk SpråkSkjema LagreStatus
    | LagreNorskFeilet Http.Error SpråkSkjema
    | LeggTilEngelsk
    | VelgNyttSpråk { valgtSpråk : Maybe SpråkKode, feilmelding : Maybe String }
    | LeggTilMuntlig SpråkKode
    | LeggTilSkriftlig SpråkMedMuntlig
    | LagrerSpråk SpråkSkjema LagreStatus
    | LagringFeilet Http.Error SpråkSkjema
    | LeggTilFlereSpråk AvsluttetGrunn
    | SpråkKodeneFeilet (Maybe Http.Error)
    | VenterPåAnimasjonFørFullføring


type AvsluttetGrunn
    = LagtInnSpråk
    | IkkeLagtInnSpråk
    | AvbruttPåbegynt


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Posix FerdigAnimertMeldingsLogg


type RemoteDataSpråkKoder
    = Loading
    | Success (List SpråkKode)
    | Failure Http.Error


type alias SpråkMedMuntlig =
    { språk : SpråkKode
    , muntlig : Ferdighet
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



--- UPDATE ---


type Msg
    = NorskErFørstespråk
    | NorskErIkkeFørstespråk
    | BrukerVelgerMuntligNivå Ferdighet
    | BrukerVelgerSkriftligNivå Ferdighet
    | BrukerKanEngelsk
    | BrukerKanIkkeEngelsk
    | BrukerKanFlereSpråk
    | BrukerHarValgtSpråkFraDropdown String
    | BrukerVilGåVidereMedValgtSpråk
    | BackendSvarerPåLagreRequest (Result Http.Error (List Språk))
    | SendSkjemaPåNytt
    | SpråkKoderHentet (Result Http.Error (List SpråkKode))
    | BrukerVilHenteSpråkKoderPåNytt
    | BrukerVilAvslutteSeksjonen
    | BrukerVilAvbryteRegistreringen
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | ErrorLogget
    | FokusSatt (Result Dom.Error ())


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        NorskErFørstespråk ->
            ( LagreStatus.init
                |> LagrerNorsk Skjema.norskFørstespråk
                |> oppdaterSamtale model (SvarFraMsg msg)
            , Cmd.batch
                [ leggTilSpråkAPI Skjema.norskFørstespråk
                , lagtTilSpørsmålCmd model.debugStatus
                ]
            )
                |> IkkeFerdig

        NorskErIkkeFørstespråk ->
            ( LeggTilNorskMuntlig
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVelgerMuntligNivå muntligNivå ->
            case model.aktivSamtale of
                LeggTilNorskMuntlig ->
                    ( muntligNivå
                        |> LeggTilNorskSkriftlig
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                LeggTilMuntlig språkKode ->
                    ( { språk = språkKode, muntlig = muntligNivå }
                        |> LeggTilSkriftlig
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVelgerSkriftligNivå skriftligNivå ->
            case model.aktivSamtale of
                LeggTilSkriftlig språkMedMuntlig ->
                    let
                        skjema =
                            Skjema.init
                                { språk = språkMedMuntlig.språk
                                , muntlig = språkMedMuntlig.muntlig
                                , skriftlig = skriftligNivå
                                }
                    in
                    ( LagreStatus.init
                        |> LagrerSpråk skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , Cmd.batch
                        [ leggTilSpråkAPI skjema
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                LeggTilNorskSkriftlig muntligNivå ->
                    let
                        skjema =
                            Skjema.init
                                { språk = SpråkKode.norsk
                                , muntlig = muntligNivå
                                , skriftlig = skriftligNivå
                                }
                    in
                    ( LagreStatus.init
                        |> LagrerNorsk skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , Cmd.batch
                        [ leggTilSpråkAPI skjema
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerKanEngelsk ->
            ( LeggTilMuntlig SpråkKode.engelsk
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerKanIkkeEngelsk ->
            ( IkkeLagtInnSpråk
                |> LeggTilFlereSpråk
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerKanFlereSpråk ->
            case model.språkKoder of
                Success _ ->
                    ( { valgtSpråk = Nothing, feilmelding = Nothing }
                        |> VelgNyttSpråk
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( SpråkKodeneFeilet Nothing
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BrukerHarValgtSpråkFraDropdown valgtSpråk ->
            IkkeFerdig
                ( valgtSpråk
                    |> updateEtterAtBrukerHarValgtSpråkFraDropdown model.aktivSamtale model.språkKoder
                    |> oppdaterSamtale model IngenNyeMeldinger
                , Cmd.none
                )

        BrukerVilGåVidereMedValgtSpråk ->
            case model.aktivSamtale of
                VelgNyttSpråk velgNyttSpråkInfo ->
                    case velgNyttSpråkInfo.valgtSpråk of
                        Just språkKode ->
                            ( språkKode
                                |> LeggTilMuntlig
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar [ SpråkKode.kode språkKode ]))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { velgNyttSpråkInfo | feilmelding = Just "Velg et språk" }
                                |> VelgNyttSpråk
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        BackendSvarerPåLagreRequest result ->
            case result of
                Ok språk ->
                    case model.aktivSamtale of
                        LagrerNorsk _ lagreStatus ->
                            lagringLykkes model språk lagreStatus LeggTilEngelsk

                        LagrerSpråk _ lagreStatus ->
                            lagringLykkes model språk lagreStatus (LeggTilFlereSpråk LagtInnSpråk)

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Err error ->
                    case model.aktivSamtale of
                        LagrerNorsk skjema lagreStatus ->
                            lagringMislyktes model error skjema lagreStatus LagrerNorsk LagreNorskFeilet

                        LagrerSpråk skjema lagreStatus ->
                            lagringMislyktes model error skjema lagreStatus LagrerSpråk LagringFeilet

                        _ ->
                            IkkeFerdig ( Model model, logFeilmelding error "Lagre språk" )

        SendSkjemaPåNytt ->
            case model.aktivSamtale of
                LagreNorskFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerNorsk skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , leggTilSpråkAPI skjema
                    )
                        |> IkkeFerdig

                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerSpråk skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , leggTilSpråkAPI skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SpråkKoderHentet result ->
            case result of
                Ok koder ->
                    ( Model { model | språkKoder = Success koder }
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Err error ->
                    IkkeFerdig ( Model model, logFeilmelding error "Hente språkkoder" )

        BrukerVilHenteSpråkKoderPåNytt ->
            case model.språkKoder of
                Success _ ->
                    ( { valgtSpråk = Nothing, feilmelding = Nothing }
                        |> VelgNyttSpråk
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( SpråkKodeneFeilet Nothing
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd model.debugStatus
                        , hentSpråkkoder
                        ]
                    )
                        |> IkkeFerdig

        BrukerVilAvslutteSeksjonen ->
            ( VenterPåAnimasjonFørFullføring
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilAvbryteRegistreringen ->
            ( AvbruttPåbegynt
                |> LeggTilFlereSpråk
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case model.aktivSamtale of
                        LagrerNorsk skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerNorsk skjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagreNorskFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerNorsk skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , leggTilSpråkAPI skjema
                                    )

                            else
                                IkkeFerdig ( Model model, Cmd.none )

                        LagrerSpråk skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerSpråk skjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagringFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSpråk skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , leggTilSpråkAPI skjema
                                    )

                            else
                                IkkeFerdig ( Model model, Cmd.none )

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Hidden ->
                    IkkeFerdig ( Model model, Cmd.none )

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            SamtaleAnimasjon.update model.debugStatus samtaleAnimasjonMsg model.seksjonsMeldingsLogg
                |> updateEtterFullførtMelding model

        ErrorLogget ->
            ( Model model, Cmd.none ) |> IkkeFerdig

        FokusSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )


updateEtterAtBrukerHarValgtSpråkFraDropdown : Samtale -> RemoteDataSpråkKoder -> String -> Samtale
updateEtterAtBrukerHarValgtSpråkFraDropdown aktivSamtale remoteSpråkKoder valgtSpråk =
    case ( aktivSamtale, remoteSpråkKoder ) of
        ( VelgNyttSpråk velgNyttSpråkInfo, Success språkKoder ) ->
            case List.find (\sprakKode -> valgtSpråk == SpråkKode.kode sprakKode) språkKoder of
                Just språkKode ->
                    VelgNyttSpråk
                        { valgtSpråk = Just språkKode
                        , feilmelding = Nothing
                        }

                Nothing ->
                    VelgNyttSpråk
                        { velgNyttSpråkInfo
                            | valgtSpråk = Nothing
                        }

        _ ->
            aktivSamtale


muntligNivåTilKnappeTekst : SpråkKode -> Ferdighet -> String
muntligNivåTilKnappeTekst språkKode muntligNivå =
    case muntligNivå of
        Nybegynner ->
            "Jeg er nybegynner"

        Godt ->
            "Jeg snakker godt " ++ String.toLower (SpråkKode.term språkKode)

        VeldigGodt ->
            "Jeg snakker veldig godt " ++ String.toLower (SpråkKode.term språkKode)

        Førstespråk ->
            SpråkKode.term språkKode ++ " er førstespråket (morsmålet) mitt"


skriftligNivåTilKnappeTekst : SpråkKode -> Ferdighet -> String
skriftligNivåTilKnappeTekst språkKode skriftligNivå =
    case skriftligNivå of
        Nybegynner ->
            "Jeg er nybegynner"

        Godt ->
            "Jeg skriver godt " ++ String.toLower (SpråkKode.term språkKode)

        VeldigGodt ->
            "Jeg skriver veldig godt " ++ String.toLower (SpråkKode.term språkKode)

        Førstespråk ->
            SpråkKode.term språkKode ++ " er førstespråket (morsmålet) mitt"


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding model ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring ->
                    Ferdig (sistLagret (Model model)) ferdigAnimertSamtale

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


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError (always ErrorLogget))
        |> Maybe.withDefault Cmd.none


leggTilSpråkAPI : SpråkSkjema -> Cmd Msg
leggTilSpråkAPI skjema =
    Api.opprettSpråk BackendSvarerPåLagreRequest skjema


hentSpråkkoder : Cmd Msg
hentSpråkkoder =
    Api.getSpråkkoder SpråkKoderHentet


samtaleTilMeldingsLogg : ModelInfo -> Samtale -> List Melding
samtaleTilMeldingsLogg model språkSeksjon =
    case språkSeksjon of
        IntroLeggTilNorsk språkListe ->
            if List.isEmpty språkListe then
                [ Melding.spørsmål [ "Nå skal du legge inn språk." ]
                , Melding.spørsmål [ "La oss begynne med norsk. Er norsk førstespråket (morsmålet) ditt?" ]
                ]

            else
                [ Melding.spørsmål [ "Nå skal du legge inn språk." ]
                , Melding.spørsmål
                    [ "Jeg ser at du har lagt inn disse språkene allerede:"
                    , språkListe
                        |> List.filterMap Språk.sprak
                        |> List.map String.toLower
                        |> listeTilSetning
                        |> String.toSentenceCase
                        |> (\setning -> setning ++ ".")
                    ]
                , Melding.spørsmål [ "Vil du legge til flere?" ]
                ]

        LeggTilNorskMuntlig ->
            [ Melding.spørsmål [ "Hvor godt snakker du norsk?" ] ]

        LeggTilNorskSkriftlig _ ->
            [ Melding.spørsmål [ "Hvor godt skriver du norsk?" ] ]

        LagrerNorsk _ _ ->
            []

        LagreNorskFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre norsk" } ]

        LeggTilEngelsk ->
            [ Melding.spørsmål [ "Da går videre til engelsk. Kan du det?" ] ]

        VelgNyttSpråk _ ->
            [ Melding.spørsmål [ "Hvilket språk vil du legge til?" ] ]

        LeggTilMuntlig språkKode ->
            [ Melding.spørsmål [ "Hvor godt snakker du " ++ String.toLower (SpråkKode.term språkKode) ++ "?" ] ]

        LeggTilSkriftlig språkMedMuntlig ->
            [ Melding.spørsmål [ "Hvor godt skriver du " ++ String.toLower (SpråkKode.term språkMedMuntlig.språk) ++ "?" ] ]

        LagrerSpråk _ _ ->
            []

        LagringFeilet error skjema ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre " ++ String.toLower (Skjema.språkNavn skjema) } ]

        LeggTilFlereSpråk avsluttetGrunn ->
            [ case avsluttetGrunn of
                LagtInnSpråk ->
                    [ Melding.spørsmål
                        [ "Bra! Nå har du lagt til "
                            ++ (model.språk
                                    |> List.filterMap Språk.sprak
                                    |> List.map String.toLower
                                    |> listeTilSetning
                               )
                            ++ "."
                        ]
                    ]

                IkkeLagtInnSpråk ->
                    []

                AvbruttPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg avbrutt." ] ]
            , [ Melding.spørsmål [ "Kan du flere språk?" ] ]
            ]
                |> List.concat

        SpråkKodeneFeilet _ ->
            [ Melding.spørsmål [ "Oops! Jeg klarte ikke hente listen over språk man kan velge", "Vil du at jeg skal prøve på nytt eller avslutte og gå videre?", "Prøv gjerne igjen senere." ] ]

        VenterPåAnimasjonFørFullføring ->
            []


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
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model samtale)

                    ManueltSvar melding ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar melding
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model samtale)

                    UtenSvar ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model samtale)
        }


lagringLykkes : ModelInfo -> List Språk -> LagreStatus -> Samtale -> SamtaleStatus
lagringLykkes model språk lagreStatus nyAktivSamtale =
    ( if LagreStatus.lagrerEtterUtlogging lagreStatus then
        nyAktivSamtale
            |> oppdaterSamtale { model | språk = språk } (ManueltSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ]))

      else
        nyAktivSamtale
            |> oppdaterSamtale { model | språk = språk } UtenSvar
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


lagringMislyktes : ModelInfo -> Http.Error -> SpråkSkjema -> LagreStatus -> (SpråkSkjema -> LagreStatus -> Samtale) -> (Http.Error -> SpråkSkjema -> Samtale) -> SamtaleStatus
lagringMislyktes model error skjema lagreStatus tilLagring tilFeilet =
    if LagreStatus.lagrerEtterUtlogging lagreStatus then
        if LagreStatus.forsøkPåNytt lagreStatus then
            ( LagreStatus.fraError error
                |> tilLagring skjema
                |> oppdaterSamtale model IngenNyeMeldinger
            , leggTilSpråkAPI skjema
            )
                |> IkkeFerdig

        else
            ( skjema
                |> tilFeilet error
                |> oppdaterSamtale model IngenNyeMeldinger
            , skjema
                |> Skjema.encode
                |> Api.logErrorWithRequestBody ErrorLogget "Lagre språk" error
            )
                |> IkkeFerdig

    else
        ( skjema
            |> tilFeilet error
            |> oppdaterSamtale model UtenSvar
        , Cmd.batch
            [ lagtTilSpørsmålCmd model.debugStatus
            , skjema
                |> Skjema.encode
                |> Api.logErrorWithRequestBody ErrorLogget "Lagre språk" error
            ]
        )
            |> IkkeFerdig


listeTilSetning : List String -> String
listeTilSetning list =
    case List.reverse list of
        [] ->
            ""

        siste :: [] ->
            siste

        siste :: resten ->
            (List.reverse resten |> String.join ", ") ++ " og " ++ siste


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        IntroLeggTilNorsk _ ->
            settFokusCmd LeggTilSpråkIntroId

        LeggTilNorskMuntlig ->
            settFokusCmd VelgNorskMuntligId

        LeggTilNorskSkriftlig _ ->
            settFokusCmd VelgNorskSkriftligId

        LeggTilEngelsk ->
            settFokusCmd LeggTilEngelskId

        LeggTilFlereSpråk _ ->
            settFokusCmd LeggTilSpråkId

        VelgNyttSpråk _ ->
            settFokusCmd VelgSpråkId

        LeggTilSkriftlig _ ->
            settFokusCmd VelgNivåSkriftligId

        LeggTilMuntlig _ ->
            settFokusCmd VelgNivåMuntligId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
        |> Task.attempt FokusSatt



--- VIEW ---


type InputId
    = LeggTilSpråkIntroId
    | VelgNorskMuntligId
    | VelgNorskSkriftligId
    | LeggTilEngelskId
    | LeggTilSpråkId
    | VelgSpråkId
    | VelgNivåSkriftligId
    | VelgNivåMuntligId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        LeggTilSpråkIntroId ->
            "språk-legg-til-intro-id"

        VelgNorskSkriftligId ->
            "språk-velg-norsk-skriftlig-id"

        VelgNorskMuntligId ->
            "språk-velg-norsk-muntlig-id"

        LeggTilEngelskId ->
            "språk-legg-til-engelsk-id"

        LeggTilSpråkId ->
            "språk-legg-til-id"

        VelgSpråkId ->
            "språk-velg-id"

        VelgNivåSkriftligId ->
            "språk-velg-skriftlig-id"

        VelgNivåMuntligId ->
            "språk-velg-muntlig-id"


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput model =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            IntroLeggTilNorsk språkListe ->
                if List.isEmpty språkListe then
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp NorskErFørstespråk "Ja"
                            |> Knapp.withId (inputIdTilString LeggTilSpråkIntroId)
                        , Knapp.knapp NorskErIkkeFørstespråk "Nei"
                        ]

                else
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp BrukerKanFlereSpråk "Ja, legg til språk"
                            |> Knapp.withId (inputIdTilString LeggTilSpråkIntroId)
                        , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, jeg har lagt inn språkene jeg kan"
                        ]

            LeggTilNorskMuntlig ->
                BrukerInput.knapper Kolonne
                    [ muntligKnapp SpråkKode.norsk Nybegynner
                        |> Knapp.withId (inputIdTilString VelgNorskMuntligId)
                    , muntligKnapp SpråkKode.norsk Godt
                    , muntligKnapp SpråkKode.norsk VeldigGodt
                    ]

            LeggTilNorskSkriftlig _ ->
                BrukerInput.knapper Kolonne
                    [ skriftligKnapp SpråkKode.norsk Nybegynner
                        |> Knapp.withId (inputIdTilString VelgNorskSkriftligId)
                    , skriftligKnapp SpråkKode.norsk Godt
                    , skriftligKnapp SpråkKode.norsk VeldigGodt
                    ]

            LagrerNorsk _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    BrukerInput.utenInnhold

            LagreNorskFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilAvslutteSeksjonen "Gå videre"
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp SendSkjemaPåNytt "Prøv på nytt"
                            , Knapp.knapp BrukerVilAvslutteSeksjonen "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            LeggTilEngelsk ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerKanEngelsk "Ja"
                        |> Knapp.withId (inputIdTilString LeggTilEngelskId)
                    , Knapp.knapp BrukerKanIkkeEngelsk "Nei"
                    ]

            LeggTilMuntlig språkKode ->
                BrukerInput.knapper Kolonne
                    [ muntligKnapp språkKode Nybegynner
                        |> Knapp.withId (inputIdTilString VelgNivåMuntligId)
                    , muntligKnapp språkKode Godt
                    , muntligKnapp språkKode VeldigGodt
                    , muntligKnapp språkKode Førstespråk
                    , Knapp.knapp BrukerVilAvbryteRegistreringen "Avbryt"
                        |> Knapp.withType Flat
                    ]

            LeggTilSkriftlig språkKode ->
                BrukerInput.knapper Kolonne
                    [ skriftligKnapp språkKode.språk Nybegynner
                        |> Knapp.withId (inputIdTilString VelgNivåSkriftligId)
                    , skriftligKnapp språkKode.språk Godt
                    , skriftligKnapp språkKode.språk VeldigGodt
                    , skriftligKnapp språkKode.språk Førstespråk
                    , Knapp.knapp BrukerVilAvbryteRegistreringen "Avbryt"
                        |> Knapp.withType Flat
                    ]

            VelgNyttSpråk velgNyttSpråkInfo ->
                case model.språkKoder of
                    Success list ->
                        BrukerInput.selectMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilGåVidereMedValgtSpråk }
                            (Select.select "Språk"
                                BrukerHarValgtSpråkFraDropdown
                                (( "Velg språk", "Velg språk" )
                                    :: List.map
                                        (\el ->
                                            ( SpråkKode.kode el, SpråkKode.term el )
                                        )
                                        list
                                )
                                |> Select.withMaybeSelected (Maybe.map SpråkKode.kode velgNyttSpråkInfo.valgtSpråk)
                                |> Select.withFeilmelding velgNyttSpråkInfo.feilmelding
                                |> Select.withErObligatorisk
                                |> Select.withId (inputIdTilString VelgSpråkId)
                            )

                    Loading ->
                        BrukerInput.utenInnhold

                    Failure _ ->
                        -- TODO: Fiks feilhåndtering her
                        BrukerInput.utenInnhold

            LagrerSpråk _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    BrukerInput.utenInnhold

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp SendSkjemaPåNytt "Ja, prøv på nytt"
                            , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            LeggTilFlereSpråk _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerKanFlereSpråk "Ja, legg til språk"
                        |> Knapp.withId (inputIdTilString LeggTilSpråkId)
                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, jeg har lagt inn språkene jeg kan"
                    ]

            SpråkKodeneFeilet _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerVilHenteSpråkKoderPåNytt "Ja, prøv på nytt"
                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, avslutt og gå videre"
                    ]

            VenterPåAnimasjonFørFullføring ->
                BrukerInput.utenInnhold

    else
        BrukerInput.utenInnhold


muntligKnapp : SpråkKode -> Ferdighet -> Knapp Msg
muntligKnapp språkKode ferdighet =
    muntligNivåTilKnappeTekst språkKode ferdighet
        |> Knapp.knapp (BrukerVelgerMuntligNivå ferdighet)


skriftligKnapp : SpråkKode -> Ferdighet -> Knapp Msg
skriftligKnapp språkKode ferdighet =
    skriftligNivåTilKnappeTekst språkKode ferdighet
        |> Knapp.knapp (BrukerVelgerSkriftligNivå ferdighet)



--- INIT ---


init : DebugStatus -> Posix -> FerdigAnimertMeldingsLogg -> List Språk -> ( Model, Cmd Msg )
init debugStatus sistLagretFraForrigeSeksjon gammelMeldingsLogg språkFerdighet =
    let
        aktivSamtale =
            IntroLeggTilNorsk språkFerdighet

        modelInfo =
            { seksjonsMeldingsLogg = MeldingsLogg.tilMeldingsLogg gammelMeldingsLogg
            , aktivSamtale = aktivSamtale
            , språk = språkFerdighet
            , språkKoder = Loading
            , debugStatus = debugStatus
            , sistLagretFraForrigeSeksjon = sistLagretFraForrigeSeksjon
            }
    in
    ( Model
        { modelInfo
            | seksjonsMeldingsLogg =
                MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg modelInfo aktivSamtale) modelInfo.seksjonsMeldingsLogg
        }
    , Cmd.batch
        [ lagtTilSpørsmålCmd debugStatus
        , hentSpråkkoder
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Browser.Events.onVisibilityChange WindowEndrerVisibility
        , model.seksjonsMeldingsLogg
            |> SamtaleAnimasjon.subscriptions
            |> Sub.map SamtaleAnimasjonMsg
        ]
