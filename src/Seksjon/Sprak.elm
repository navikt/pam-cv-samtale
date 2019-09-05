module Seksjon.Sprak exposing
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
import Cv.Spraakferdighet as Spraakferdighet exposing (Spraakferdighet)
import DebugStatus exposing (DebugStatus)
import Feilmelding
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Encode
import List.Extra as List
import Melding exposing (Melding)
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Process
import SamtaleAnimasjon
import Skjema.Sprak as SpråkSkjema exposing (Ferdighet(..), SpråkSkjema)
import SpråkKode exposing (SpråkKode)
import String.Extra as String
import Task



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
    | LagrerNorsk SpråkSkjema
    | LagreNorskFeilet SpråkSkjema Http.Error
    | LeggTilEngelsk
    | VelgNyttSpråk (Maybe SpråkKode)
    | LeggTilMuntlig SpråkKode
    | LeggTilSkriftlig SpråkMedMuntlig
    | LagrerSpråk SpråkSkjema
    | LagringFeilet SpråkSkjema Http.Error
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
    { språkNavn : SpråkKode
    , muntlig : Ferdighet
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



--- UPDATE ---


type Msg
    = ValgtSpråk String
    | BrukerHarValgtSpråk
    | NorskErMorsmål
    | NorskErIkkeMorsmål
    | BrukerKanEngelsk
    | BrukerKanIkkeEngelsk
    | BrukerKanFlereSpråk
    | BrukerVelgerMuntligNivå Ferdighet
    | BrukerVelgerSkriftligNivå Ferdighet
    | BrukerVilLeggeTilSpråk String
    | BrukerVilAvslutteSeksjonen
    | ViewportSatt (Result Dom.Error ())
    | BackendSvarerPåLagringsRequest (Result Http.Error (List Spraakferdighet))
    | ErrorLogget (Result Http.Error ())
    | SpråkkoderHentet (Result Http.Error (List SpråkKode))
    | SendSkjemaPåNytt SpråkSkjema
    | BrukerVilHenteSpråkPåNytt
    | StartÅSkrive
    | FullførMelding


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        NorskErMorsmål ->
            ( nesteSamtaleSteg model (Melding.svar [ "Ja" ]) (LagrerNorsk SpråkSkjema.norskMorsmål)
            , Cmd.batch
                [ leggTilSpråkAPI SpråkSkjema.norskMorsmål
                , lagtTilSpørsmålCmd model.debugStatus
                ]
            )
                |> IkkeFerdig

        NorskErIkkeMorsmål ->
            ( LeggTilNorskMuntlig
                |> nesteSamtaleSteg model (Melding.svar [ "Nei" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVelgerSkriftligNivå skriftligNivå ->
            case model.aktivSamtale of
                LeggTilSkriftlig språkMedMuntlig ->
                    let
                        skjema =
                            SpråkSkjema.init
                                { språk = språkMedMuntlig.språkNavn
                                , muntlig = språkMedMuntlig.muntlig
                                , skriftlig = skriftligNivå
                                }
                    in
                    ( LagrerSpråk skjema
                        |> nesteSamtaleSteg model
                            (Melding.svar [ skriftligNivåTilKnappeTekst språkMedMuntlig.språkNavn skriftligNivå ])
                    , Cmd.batch
                        [ leggTilSpråkAPI skjema
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                LeggTilNorskSkriftlig muntlig ->
                    let
                        skjema =
                            SpråkSkjema.init
                                { språk = SpråkKode.norsk
                                , muntlig = muntlig
                                , skriftlig = skriftligNivå
                                }
                    in
                    ( LagrerNorsk skjema
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

        BrukerVelgerMuntligNivå muntligNivå ->
            case model.aktivSamtale of
                LeggTilNorskMuntlig ->
                    ( LeggTilNorskSkriftlig muntligNivå
                        |> nesteSamtaleSteg model
                            (Melding.svar [ muntligNivåTilKnappeTekst SpråkKode.norsk muntligNivå ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                LeggTilMuntlig enkeltSpråk ->
                    ( LeggTilSkriftlig { språkNavn = enkeltSpråk, muntlig = muntligNivå }
                        |> nesteSamtaleSteg model
                            (Melding.svar [ muntligNivåTilKnappeTekst enkeltSpråk muntligNivå ])
                    , lagtTilSpørsmålCmd model.debugStatus
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

        BrukerVilLeggeTilSpråk språkNavn ->
            case model.språkKoder of
                Success språkKoder ->
                    case List.find (\element -> SpråkKode.term element == språkNavn) språkKoder of
                        Just språkKode ->
                            ( LeggTilMuntlig språkKode
                                |> nesteSamtaleSteg model (Melding.svar [ språkNavn ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerKanFlereSpråk ->
            case model.språkKoder of
                Success list ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ]) (VelgNyttSpråk Nothing)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ]) (SpråkKodeneFeilet Nothing)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BrukerVilAvslutteSeksjonen ->
            ( nesteSamtaleSteg model (Melding.svar [ "Nei, gå videre" ]) VenterPåAnimasjonFørFullføring
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        ViewportSatt result ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        BackendSvarerPåLagringsRequest result ->
            case result of
                Ok value ->
                    case model.aktivSamtale of
                        LagrerNorsk _ ->
                            ( nesteSamtaleStegUtenMelding { model | språk = value } LeggTilEngelsk
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        LagrerSpråk _ ->
                            ( nesteSamtaleStegUtenMelding { model | språk = value } LeggTilFlereSpråk
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        LagreNorskFeilet _ _ ->
                            ( nesteSamtaleStegUtenMelding { model | språk = value } LeggTilEngelsk
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        LagringFeilet _ _ ->
                            ( nesteSamtaleSteg { model | språk = value } (Melding.svar [ "Ja, prøv på nytt" ]) LeggTilFlereSpråk
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Err error ->
                    case model.aktivSamtale of
                        LagrerNorsk enkeltSpråk ->
                            ( nesteSamtaleStegUtenMelding model (LagreNorskFeilet enkeltSpråk error)
                            , Cmd.batch
                                [ lagtTilSpørsmålCmd model.debugStatus
                                , logFeilmeldingMedRequestBody error "Lagre språk" (SpråkSkjema.encode enkeltSpråk)
                                ]
                            )
                                |> IkkeFerdig

                        LagrerSpråk enkeltSpråk ->
                            ( nesteSamtaleStegUtenMelding model (LagringFeilet enkeltSpråk error)
                            , Cmd.batch
                                [ lagtTilSpørsmålCmd model.debugStatus
                                , logFeilmeldingMedRequestBody error "Lagre språk" (SpråkSkjema.encode enkeltSpråk)
                                ]
                            )
                                |> IkkeFerdig

                        _ ->
                            ( Model model
                            , logFeilmelding error "Lagre språk"
                            )
                                |> IkkeFerdig

        ErrorLogget result ->
            ( Model model, Cmd.none ) |> IkkeFerdig

        SpråkkoderHentet result ->
            case result of
                Ok koder ->
                    ( Model { model | språkKoder = Success koder }
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Err error ->
                    ( Model model
                    , logFeilmelding error "Språkkoder hentet"
                    )
                        |> IkkeFerdig

        ValgtSpråk valgtSpråk ->
            case model.språkKoder of
                Loading ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

                Success list ->
                    ( Model { model | aktivSamtale = VelgNyttSpråk (List.find (\sprakkode -> valgtSpråk == SpråkKode.kode sprakkode) list) }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Failure error ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        BrukerHarValgtSpråk ->
            case model.aktivSamtale of
                VelgNyttSpråk språkkode ->
                    case språkkode of
                        Just kode ->
                            ( LeggTilMuntlig kode
                                |> nesteSamtaleSteg model (Melding.svar [ SpråkKode.kode kode ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( Model model, Cmd.none ) |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        SendSkjemaPåNytt enkeltSpråk ->
            case model.aktivSamtale of
                LagreNorskFeilet skjema _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, prøv på nytt" ]) (LagrerNorsk skjema)
                    , leggTilSpråkAPI enkeltSpråk
                    )
                        |> IkkeFerdig

                LagringFeilet skjema _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, prøv på nytt" ]) (LagrerSpråk skjema)
                    , leggTilSpråkAPI enkeltSpråk
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilHenteSpråkPåNytt ->
            case model.språkKoder of
                Success list ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ]) (VelgNyttSpråk Nothing)
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


muntligNivåTilKnappeTekst : SpråkKode -> Ferdighet -> String
muntligNivåTilKnappeTekst språkKode muntligNivå =
    case muntligNivå of
        Nybegynner ->
            "Jeg er nybegynner"

        Godt ->
            "Jeg snakker godt " ++ String.toLower (SpråkKode.term språkKode)

        VeldigGodt ->
            "Jeg snakker veldig godt " ++ String.toLower (SpråkKode.term språkKode)

        Morsmål ->
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

        Morsmål ->
            SpråkKode.term språkKode ++ " er førstespråket (morsmålet) mitt"


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring ->
                    Ferdig ferdigAnimertSamtale

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


fullførSeksjonHvisMeldingsloggErFerdig : ModelInfo -> SamtaleStatus
fullførSeksjonHvisMeldingsloggErFerdig modelInfo =
    case MeldingsLogg.ferdigAnimert modelInfo.seksjonsMeldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            Ferdig ferdigAnimertMeldingsLogg

        MeldingerGjenstår ->
            ( Model { modelInfo | aktivSamtale = VenterPåAnimasjonFørFullføring }, Cmd.none )
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


logFeilmeldingMedRequestBody : Http.Error -> String -> Json.Encode.Value -> Cmd Msg
logFeilmeldingMedRequestBody error operasjon requestBody =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Feilmelding.withRequestBody requestBody)
        |> Maybe.map (Api.logError ErrorLogget)
        |> Maybe.withDefault Cmd.none


leggTilSpråkAPI : SpråkSkjema -> Cmd Msg
leggTilSpråkAPI skjema =
    Api.postSpråk BackendSvarerPåLagringsRequest skjema


hentSpråkkoder : Cmd Msg
hentSpråkkoder =
    Api.getSpråkkoder SpråkkoderHentet


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

        LeggTilNorskSkriftlig ferdighet ->
            [ Melding.spørsmål [ "Hvor godt skriver du norsk?" ] ]

        LagrerNorsk enkeltSpråk ->
            []

        LagreNorskFeilet språkSkjema error ->
            [ Melding.spørsmål [ "Oops... Jeg klarte ikke å lagre norsk.", "Vil du prøve på nytt?" ] ]

        LeggTilEngelsk ->
            [ Melding.spørsmål [ "Hva med engelsk? Kan du det?" ] ]

        VelgNyttSpråk _ ->
            [ Melding.spørsmål [ "Hvilket språk vil du legge til?" ] ]

        LeggTilMuntlig enkeltSpråk ->
            [ Melding.spørsmål [ "Hvor godt snakker du " ++ String.toLower (SpråkKode.term enkeltSpråk) ++ "?" ] ]

        LeggTilSkriftlig enkeltSpråk ->
            [ Melding.spørsmål [ "Hvor godt skriver du " ++ String.toLower (SpråkKode.term enkeltSpråk.språkNavn) ++ "?" ] ]

        LagrerSpråk enkeltSpråk ->
            []

        LagringFeilet enkeltSpråk error ->
            [ Melding.spørsmål [ "Oops... Jeg klarte ikke å lagre " ++ String.toLower (SpråkSkjema.språkNavn enkeltSpråk) ++ ".", "Vil du prøve på nytt?" ] ]

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

        SpråkKodeneFeilet error ->
            [ Melding.spørsmål [ "Oops! Noe gikk galt...", "Vil du prøve på nytt eller avslutte og gå videre?", "Prøv gjerne igjen senere." ] ]

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
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                IntroLeggTilNorsk språkListe ->
                    if List.isEmpty språkListe then
                        div [ class "skjema-wrapper" ]
                            [ div [ class "skjema" ]
                                [ div [ class "inputrad" ]
                                    [ Knapp.knapp NorskErMorsmål "Ja"
                                        |> Knapp.toHtml
                                    , Knapp.knapp NorskErIkkeMorsmål "Nei"
                                        |> Knapp.toHtml
                                    ]
                                ]
                            ]

                    else
                        div [ class "skjema-wrapper" ]
                            [ div [ class "skjema" ]
                                [ div [ class "inputkolonne" ]
                                    [ Knapp.knapp BrukerKanFlereSpråk "Ja, legg til språk"
                                        |> Knapp.toHtml
                                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                        |> Knapp.toHtml
                                    ]
                                ]
                            ]

                LeggTilNorskMuntlig ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            [ div [ class "inputkolonne" ] [ muntligKnapp SpråkKode.norsk Nybegynner ]
                            , div [ class "inputkolonne" ] [ muntligKnapp SpråkKode.norsk Godt ]
                            , div [ class "inputkolonne" ] [ muntligKnapp SpråkKode.norsk VeldigGodt ]
                            ]
                        ]

                LeggTilNorskSkriftlig _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            [ div [ class "inputkolonne" ] [ skriftligKnapp SpråkKode.norsk Nybegynner ]
                            , div [ class "inputkolonne" ] [ skriftligKnapp SpråkKode.norsk Godt ]
                            , div [ class "inputkolonne" ] [ skriftligKnapp SpråkKode.norsk VeldigGodt ]
                            ]
                        ]

                LagrerNorsk enkeltSpråk ->
                    text ""

                LagreNorskFeilet språkSkjema error ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ Knapp.knapp (SendSkjemaPåNytt språkSkjema) "Ja, prøv på nytt"
                                |> Knapp.toHtml
                            , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

                LeggTilEngelsk ->
                    div [ class "inputrad" ]
                        [ div [ class "inputrad-innhold" ]
                            [ Knapp.knapp BrukerKanEngelsk "Ja"
                                |> Knapp.toHtml
                            , Knapp.knapp BrukerKanIkkeEngelsk "Nei"
                                |> Knapp.toHtml
                            ]
                        ]

                VelgNyttSpråk valgtSpråk ->
                    case model.språkKoder of
                        Success list ->
                            div [ class "skjema-wrapper" ]
                                [ div [ class "skjema" ]
                                    [ div [ class "inputkolonne" ]
                                        [ div []
                                            [ Select.select "Språk"
                                                ValgtSpråk
                                                (( "Velg språk", "Velg språk" )
                                                    :: List.map
                                                        (\el ->
                                                            ( SpråkKode.kode el, SpråkKode.term el )
                                                        )
                                                        list
                                                )
                                                |> Select.toHtml
                                            , Knapp.knapp BrukerHarValgtSpråk "Legg til"
                                                |> Knapp.withEnabled
                                                    (if valgtSpråk /= Nothing then
                                                        Knapp.Enabled

                                                     else
                                                        Knapp.Disabled
                                                    )
                                                |> Knapp.withClass Knapp.SpråknivåKnapp
                                                |> Knapp.toHtml
                                            ]
                                        ]
                                    ]
                                ]

                        Loading ->
                            div [ class "inputkolonne" ]
                                [ div [ class "inputkolonne-innhold" ]
                                    [ text "Loading..." ]
                                ]

                        Failure error ->
                            div [ class "inputkolonne" ]
                                [ div [ class "inputkolonne-innhold" ]
                                    [ text "Noe gikk galt..." ]
                                ]

                LeggTilMuntlig enkeltSpråk ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            [ div [ class "inputkolonne" ] [ muntligKnapp enkeltSpråk Nybegynner ]
                            , div [ class "inputkolonne" ] [ muntligKnapp enkeltSpråk Godt ]
                            , div [ class "inputkolonne" ] [ muntligKnapp enkeltSpråk VeldigGodt ]
                            ]
                        ]

                LeggTilSkriftlig enkeltSpråk ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            [ div [ class "inputkolonne" ] [ skriftligKnapp enkeltSpråk.språkNavn Nybegynner ]
                            , div [ class "inputkolonne" ] [ skriftligKnapp enkeltSpråk.språkNavn Godt ]
                            , div [ class "inputkolonne" ] [ skriftligKnapp enkeltSpråk.språkNavn VeldigGodt ]
                            ]
                        ]

                LagrerSpråk enkeltSpråk ->
                    text ""

                LagringFeilet enkeltSpråk error ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ Knapp.knapp (SendSkjemaPåNytt enkeltSpråk) "Ja, prøv på nytt"
                                |> Knapp.toHtml
                            , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

                LeggTilFlereSpråk ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp BrukerKanFlereSpråk "Ja, legg til språk"
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                SpråkKodeneFeilet error ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ Knapp.knapp BrukerVilHenteSpråkPåNytt "Ja, prøv på nytt"
                                |> Knapp.toHtml
                            , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, avslutt og gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

                VenterPåAnimasjonFørFullføring ->
                    text ""

        MeldingerGjenstår ->
            text ""


muntligKnapp : SpråkKode -> Ferdighet -> Html Msg
muntligKnapp språkKode ferdighet =
    Knapp.knapp (BrukerVelgerMuntligNivå ferdighet) (muntligNivåTilKnappeTekst språkKode ferdighet)
        |> Knapp.withClass Knapp.SpråknivåKnapp
        |> Knapp.toHtml


skriftligKnapp : SpråkKode -> Ferdighet -> Html Msg
skriftligKnapp språkKode ferdighet =
    Knapp.knapp (BrukerVelgerSkriftligNivå ferdighet) (skriftligNivåTilKnappeTekst språkKode ferdighet)
        |> Knapp.withClass Knapp.SpråknivåKnapp
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
