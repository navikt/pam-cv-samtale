module Sprak.Seksjon exposing
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
import Browser.Events exposing (Visibility(..))
import Cv.Spraakferdighet as Spraakferdighet exposing (Spraakferdighet)
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Feilmelding
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Encode
import LagreStatus exposing (LagreStatus)
import List.Extra as List
import Melding exposing (Melding)
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import SamtaleAnimasjon
import Sprak.Skjema as Skjema exposing (Ferdighet(..), SpråkSkjema)
import SpråkKode exposing (SpråkKode)
import String.Extra as String



-- MODEL --


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , språk : List Spraakferdighet
    , språkKoder : RemoteDataSpråkKoder
    , debugStatus : DebugStatus
    }


type Samtale
    = IntroLeggTilNorsk (List Spraakferdighet)
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
    | LeggTilFlereSpråk
    | SpråkKodeneFeilet (Maybe Http.Error)
    | VenterPåAnimasjonFørFullføring


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig FerdigAnimertMeldingsLogg


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
    | BackendSvarerPåLagreRequest (Result Http.Error (List Spraakferdighet))
    | SendSkjemaPåNytt
    | SpråkKoderHentet (Result Http.Error (List SpråkKode))
    | BrukerVilHenteSpråkKoderPåNytt
    | BrukerVilAvslutteSeksjonen
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | ErrorLogget


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        NorskErFørstespråk ->
            ( LagreStatus.init
                |> LagrerNorsk Skjema.norskFørstespråk
                |> nesteSamtaleSteg model (Melding.svar [ "Ja" ])
            , Cmd.batch
                [ leggTilSpråkAPI Skjema.norskFørstespråk
                , lagtTilSpørsmålCmd model.debugStatus
                ]
            )
                |> IkkeFerdig

        NorskErIkkeFørstespråk ->
            ( LeggTilNorskMuntlig
                |> nesteSamtaleSteg model (Melding.svar [ "Nei" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVelgerMuntligNivå muntligNivå ->
            case model.aktivSamtale of
                LeggTilNorskMuntlig ->
                    ( LeggTilNorskSkriftlig muntligNivå
                        |> nesteSamtaleSteg model
                            (Melding.svar [ muntligNivåTilKnappeTekst SpråkKode.norsk muntligNivå ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                LeggTilMuntlig språkKode ->
                    ( LeggTilSkriftlig { språk = språkKode, muntlig = muntligNivå }
                        |> nesteSamtaleSteg model
                            (Melding.svar [ muntligNivåTilKnappeTekst språkKode muntligNivå ])
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
                        |> nesteSamtaleSteg model
                            (Melding.svar [ skriftligNivåTilKnappeTekst språkMedMuntlig.språk skriftligNivå ])
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
                        |> nesteSamtaleSteg model
                            (Melding.svar [ skriftligNivåTilKnappeTekst SpråkKode.norsk skriftligNivå ])
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
                |> nesteSamtaleSteg model (Melding.svar [ "Ja" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerKanIkkeEngelsk ->
            ( nesteSamtaleSteg model (Melding.svar [ "Nei" ]) (IntroLeggTilNorsk model.språk)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerKanFlereSpråk ->
            case model.språkKoder of
                Success _ ->
                    ( { valgtSpråk = Nothing, feilmelding = Nothing }
                        |> VelgNyttSpråk
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ]) (SpråkKodeneFeilet Nothing)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BrukerHarValgtSpråkFraDropdown valgtSpråk ->
            IkkeFerdig
                ( Model { model | aktivSamtale = updateEtterAtBrukerHarValgtSpråkFraDropdown model.aktivSamtale model.språkKoder valgtSpråk }
                , Cmd.none
                )

        BrukerVilGåVidereMedValgtSpråk ->
            case model.aktivSamtale of
                VelgNyttSpråk velgNyttSpråkInfo ->
                    case velgNyttSpråkInfo.valgtSpråk of
                        Just språkKode ->
                            ( LeggTilMuntlig språkKode
                                |> nesteSamtaleSteg model (Melding.svar [ SpråkKode.kode språkKode ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        VelgNyttSpråk
                                            { velgNyttSpråkInfo
                                                | feilmelding = Just "Velg et språk"
                                            }
                                }
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
                            lagringLykkes model språk lagreStatus LeggTilFlereSpråk

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
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, prøv på nytt" ])
                    , leggTilSpråkAPI skjema
                    )
                        |> IkkeFerdig

                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerSpråk skjema
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, prøv på nytt" ])
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
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, prøve på nytt" ]) (SpråkKodeneFeilet Nothing)
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd model.debugStatus
                        , hentSpråkkoder
                        ]
                    )
                        |> IkkeFerdig

        BrukerVilAvslutteSeksjonen ->
            ( nesteSamtaleSteg model (Melding.svar [ "Nei, gå videre" ]) VenterPåAnimasjonFørFullføring
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
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagreNorskFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerNorsk skjema
                                        |> oppdaterSamtaleSteg model
                                    , leggTilSpråkAPI skjema
                                    )

                            else
                                IkkeFerdig ( Model model, Cmd.none )

                        LagrerSpråk skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerSpråk skjema
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagringFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSpråk skjema
                                        |> oppdaterSamtaleSteg model
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
                    Ferdig ferdigAnimertSamtale

                _ ->
                    ( Model { model | seksjonsMeldingsLogg = nyMeldingsLogg }
                    , Cmd.map SamtaleAnimasjonMsg cmd
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


logFeilmeldingMedRequestBody : Http.Error -> String -> Json.Encode.Value -> Cmd Msg
logFeilmeldingMedRequestBody error operasjon requestBody =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Feilmelding.withRequestBody requestBody)
        |> Maybe.map (Api.logError (always ErrorLogget))
        |> Maybe.withDefault Cmd.none


leggTilSpråkAPI : SpråkSkjema -> Cmd Msg
leggTilSpråkAPI skjema =
    Api.postSpråk BackendSvarerPåLagreRequest skjema


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
                        |> List.filterMap Spraakferdighet.sprak
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
            [ Melding.spørsmål [ "Hva med engelsk? Kan du det?" ] ]

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

        LeggTilFlereSpråk ->
            [ Melding.spørsmål
                [ "Supert! Da har du lagt inn "
                    ++ (model.språk
                            |> List.filterMap Spraakferdighet.sprak
                            |> List.map String.toLower
                            |> listeTilSetning
                       )
                    ++ "."
                ]
            , Melding.spørsmål
                [ "Kan du flere språk?"
                ]
            ]

        SpråkKodeneFeilet _ ->
            [ Melding.spørsmål [ "Oops! Jeg klarte ikke hente listen over språk man kan velge", "Vil du at jeg skal prøve på nytt eller avslutte og gå videre?", "Prøv gjerne igjen senere." ] ]

        VenterPåAnimasjonFørFullføring ->
            []


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg model melding samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model samtaleSeksjon)
        }


nesteSamtaleStegUtenMelding : ModelInfo -> Samtale -> Model
nesteSamtaleStegUtenMelding model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model samtaleSeksjon)
        }


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg modelInfo samtaleSeksjon =
    Model
        { modelInfo
            | aktivSamtale = samtaleSeksjon
        }


lagringLykkes : ModelInfo -> List Spraakferdighet -> LagreStatus -> Samtale -> SamtaleStatus
lagringLykkes model språk lagreStatus nyAktivSamtale =
    ( if LagreStatus.lagrerEtterUtlogging lagreStatus then
        nyAktivSamtale
            |> nesteSamtaleSteg { model | språk = språk } (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])

      else
        nyAktivSamtale
            |> nesteSamtaleStegUtenMelding { model | språk = språk }
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


lagringMislyktes : ModelInfo -> Http.Error -> SpråkSkjema -> LagreStatus -> (SpråkSkjema -> LagreStatus -> Samtale) -> (Http.Error -> SpråkSkjema -> Samtale) -> SamtaleStatus
lagringMislyktes model error skjema lagreStatus tilLagring tilFeilet =
    if LagreStatus.lagrerEtterUtlogging lagreStatus then
        if LagreStatus.forsøkPåNytt lagreStatus then
            ( LagreStatus.fraError error
                |> tilLagring skjema
                |> oppdaterSamtaleSteg model
            , leggTilSpråkAPI skjema
            )
                |> IkkeFerdig

        else
            ( skjema
                |> tilFeilet error
                |> oppdaterSamtaleSteg model
            , skjema
                |> Skjema.encode
                |> Api.logErrorWithRequestBody ErrorLogget "Lagre språk" error
            )
                |> IkkeFerdig

    else
        ( skjema
            |> tilFeilet error
            |> nesteSamtaleStegUtenMelding model
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



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            IntroLeggTilNorsk språkListe ->
                if List.isEmpty språkListe then
                    Containers.knapper Flytende
                        [ Knapp.knapp NorskErFørstespråk "Ja"
                            |> Knapp.toHtml
                        , Knapp.knapp NorskErIkkeFørstespråk "Nei"
                            |> Knapp.toHtml
                        ]

                else
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerKanFlereSpråk "Ja, legg til språk"
                            |> Knapp.toHtml
                        , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                            |> Knapp.toHtml
                        ]

            LeggTilNorskMuntlig ->
                Containers.knapper Kolonne
                    [ muntligKnapp SpråkKode.norsk Nybegynner
                    , muntligKnapp SpråkKode.norsk Godt
                    , muntligKnapp SpråkKode.norsk VeldigGodt
                    ]

            LeggTilNorskSkriftlig _ ->
                Containers.knapper Kolonne
                    [ skriftligKnapp SpråkKode.norsk Nybegynner
                    , skriftligKnapp SpråkKode.norsk Godt
                    , skriftligKnapp SpråkKode.norsk VeldigGodt
                    ]

            LagrerNorsk _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    text ""

            LagreNorskFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        Containers.knapper Flytende
                            [ Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                |> Knapp.toHtml
                            ]

                    PrøvPåNytt ->
                        Containers.knapper Flytende
                            [ Knapp.knapp SendSkjemaPåNytt "Ja, prøv på nytt"
                                |> Knapp.toHtml
                            , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                |> Knapp.toHtml
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            LeggTilEngelsk ->
                Containers.knapper Flytende
                    [ Knapp.knapp BrukerKanEngelsk "Ja"
                        |> Knapp.toHtml
                    , Knapp.knapp BrukerKanIkkeEngelsk "Nei"
                        |> Knapp.toHtml
                    ]

            LeggTilMuntlig språkKode ->
                Containers.knapper Kolonne
                    [ muntligKnapp språkKode Nybegynner
                    , muntligKnapp språkKode Godt
                    , muntligKnapp språkKode VeldigGodt
                    , muntligKnapp språkKode Førstespråk
                    ]

            LeggTilSkriftlig språkKode ->
                Containers.knapper Kolonne
                    [ skriftligKnapp språkKode.språk Nybegynner
                    , skriftligKnapp språkKode.språk Godt
                    , skriftligKnapp språkKode.språk VeldigGodt
                    , skriftligKnapp språkKode.språk Førstespråk
                    ]

            VelgNyttSpråk velgNyttSpråkInfo ->
                case model.språkKoder of
                    Success list ->
                        Containers.inputMedGåVidereKnapp BrukerVilGåVidereMedValgtSpråk
                            [ div [ class "select-i-samtaleflyt-wrapper" ]
                                [ Select.select "Språk"
                                    BrukerHarValgtSpråkFraDropdown
                                    (( "Velg språk", "Velg språk" )
                                        :: List.map
                                            (\el ->
                                                ( SpråkKode.kode el, SpråkKode.term el )
                                            )
                                            list
                                    )
                                    |> Select.withMaybeSelected (Maybe.map SpråkKode.kode velgNyttSpråkInfo.valgtSpråk)
                                    |> Select.withMaybeFeilmelding velgNyttSpråkInfo.feilmelding
                                    |> Select.toHtml
                                ]
                            ]

                    Loading ->
                        text ""

                    Failure _ ->
                        -- TODO: Fiks feilhåndtering her
                        Containers.knapper Flytende
                            [ text "Noe gikk galt..." ]

            LagrerSpråk _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    text ""

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        Containers.knapper Flytende
                            [ Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                |> Knapp.toHtml
                            ]

                    PrøvPåNytt ->
                        Containers.knapper Flytende
                            [ Knapp.knapp SendSkjemaPåNytt "Ja, prøv på nytt"
                                |> Knapp.toHtml
                            , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                |> Knapp.toHtml
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            LeggTilFlereSpråk ->
                Containers.knapper Flytende
                    [ Knapp.knapp BrukerKanFlereSpråk "Ja, legg til språk"
                        |> Knapp.toHtml
                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                        |> Knapp.toHtml
                    ]

            SpråkKodeneFeilet _ ->
                Containers.knapper Flytende
                    [ Knapp.knapp BrukerVilHenteSpråkKoderPåNytt "Ja, prøv på nytt"
                        |> Knapp.toHtml
                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, avslutt og gå videre"
                        |> Knapp.toHtml
                    ]

            VenterPåAnimasjonFørFullføring ->
                text ""

    else
        text ""


muntligKnapp : SpråkKode -> Ferdighet -> Html Msg
muntligKnapp språkKode ferdighet =
    Knapp.knapp (BrukerVelgerMuntligNivå ferdighet) (muntligNivåTilKnappeTekst språkKode ferdighet)
        |> Knapp.toHtml


skriftligKnapp : SpråkKode -> Ferdighet -> Html Msg
skriftligKnapp språkKode ferdighet =
    Knapp.knapp (BrukerVelgerSkriftligNivå ferdighet) (skriftligNivåTilKnappeTekst språkKode ferdighet)
        |> Knapp.toHtml



--- INIT ---


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List Spraakferdighet -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg språkFerdighet =
    let
        aktivSamtale =
            IntroLeggTilNorsk språkFerdighet

        modelInfo =
            { seksjonsMeldingsLogg = MeldingsLogg.tilMeldingsLogg gammelMeldingsLogg
            , aktivSamtale = aktivSamtale
            , språk = språkFerdighet
            , språkKoder = Loading
            , debugStatus = debugStatus
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
