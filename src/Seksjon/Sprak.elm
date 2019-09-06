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
    = NorskErMorsmål
    | NorskErIkkeMorsmål
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
    | StartÅSkrive
    | FullførMelding
    | ViewportSatt
    | ErrorLogget


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
                    ( LeggTilSkriftlig { språkNavn = språkKode, muntlig = muntligNivå }
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

                LeggTilNorskSkriftlig muntligNivå ->
                    let
                        skjema =
                            SpråkSkjema.init
                                { språk = SpråkKode.norsk
                                , muntlig = muntligNivå
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
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ]) (VelgNyttSpråk Nothing)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ]) (SpråkKodeneFeilet Nothing)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BrukerHarValgtSpråkFraDropdown valgtSpråk ->
            case model.språkKoder of
                Success språkKoder ->
                    ( Model
                        { model
                            | aktivSamtale =
                                språkKoder
                                    |> List.find (\sprakKode -> valgtSpråk == SpråkKode.kode sprakKode)
                                    |> VelgNyttSpråk
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        BrukerVilGåVidereMedValgtSpråk ->
            case model.aktivSamtale of
                VelgNyttSpråk maybeSpråkKode ->
                    case maybeSpråkKode of
                        Just språkKode ->
                            ( LeggTilMuntlig språkKode
                                |> nesteSamtaleSteg model (Melding.svar [ SpråkKode.kode språkKode ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( Model model, Cmd.none ) |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        BackendSvarerPåLagreRequest result ->
            case result of
                Ok språk ->
                    case model.aktivSamtale of
                        LagrerNorsk _ ->
                            ( nesteSamtaleStegUtenMelding { model | språk = språk } LeggTilEngelsk
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        LagrerSpråk _ ->
                            ( nesteSamtaleStegUtenMelding { model | språk = språk } LeggTilFlereSpråk
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Err error ->
                    case model.aktivSamtale of
                        LagrerNorsk skjema ->
                            ( nesteSamtaleStegUtenMelding model (LagreNorskFeilet skjema error)
                            , Cmd.batch
                                [ lagtTilSpørsmålCmd model.debugStatus
                                , logFeilmeldingMedRequestBody error "Lagre språk" (SpråkSkjema.encode skjema)
                                ]
                            )
                                |> IkkeFerdig

                        LagrerSpråk skjema ->
                            ( nesteSamtaleStegUtenMelding model (LagringFeilet skjema error)
                            , Cmd.batch
                                [ lagtTilSpørsmålCmd model.debugStatus
                                , logFeilmeldingMedRequestBody error "Lagre språk" (SpråkSkjema.encode skjema)
                                ]
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, logFeilmelding error "Lagre språk" )

        SendSkjemaPåNytt ->
            case model.aktivSamtale of
                LagreNorskFeilet skjema _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, prøv på nytt" ]) (LagrerNorsk skjema)
                    , leggTilSpråkAPI skjema
                    )
                        |> IkkeFerdig

                LagringFeilet skjema _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, prøv på nytt" ]) (LagrerSpråk skjema)
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
                    IkkeFerdig ( Model model, logFeilmelding error "Språkkoder hentet" )

        BrukerVilHenteSpråkKoderPåNytt ->
            case model.språkKoder of
                Success _ ->
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

        BrukerVilAvslutteSeksjonen ->
            ( nesteSamtaleSteg model (Melding.svar [ "Nei, gå videre" ]) VenterPåAnimasjonFørFullføring
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        StartÅSkrive ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        MeldingsLogg.startÅSkrive model.seksjonsMeldingsLogg
                }
            , Cmd.batch
                [ SamtaleAnimasjon.scrollTilBunn (always ViewportSatt)
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

        ViewportSatt ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        ErrorLogget ->
            ( Model model, Cmd.none ) |> IkkeFerdig


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
                    , SamtaleAnimasjon.scrollTilBunn (always ViewportSatt)
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
        [ SamtaleAnimasjon.scrollTilBunn (always ViewportSatt)
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

        LagrerNorsk _ ->
            []

        LagreNorskFeilet _ _ ->
            [ Melding.spørsmål [ "Oops... Jeg klarte ikke å lagre norsk.", "Vil du prøve på nytt?" ] ]

        LeggTilEngelsk ->
            [ Melding.spørsmål [ "Hva med engelsk? Kan du det?" ] ]

        VelgNyttSpråk _ ->
            [ Melding.spørsmål [ "Hvilket språk vil du legge til?" ] ]

        LeggTilMuntlig språkKode ->
            [ Melding.spørsmål [ "Hvor godt snakker du " ++ String.toLower (SpråkKode.term språkKode) ++ "?" ] ]

        LeggTilSkriftlig språkMedMuntlig ->
            [ Melding.spørsmål [ "Hvor godt skriver du " ++ String.toLower (SpråkKode.term språkMedMuntlig.språkNavn) ++ "?" ] ]

        LagrerSpråk _ ->
            []

        LagringFeilet skjema _ ->
            [ Melding.spørsmål [ "Oops... Jeg klarte ikke å lagre " ++ String.toLower (SpråkSkjema.språkNavn skjema) ++ ".", "Vil du prøve på nytt?" ] ]

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

                LagrerNorsk _ ->
                    text ""

                LagreNorskFeilet _ _ ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ Knapp.knapp SendSkjemaPåNytt "Ja, prøv på nytt"
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
                                                BrukerHarValgtSpråkFraDropdown
                                                (( "Velg språk", "Velg språk" )
                                                    :: List.map
                                                        (\el ->
                                                            ( SpråkKode.kode el, SpråkKode.term el )
                                                        )
                                                        list
                                                )
                                                |> Select.toHtml
                                            , Knapp.knapp BrukerVilGåVidereMedValgtSpråk "Legg til"
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

                        Failure _ ->
                            div [ class "inputkolonne" ]
                                [ div [ class "inputkolonne-innhold" ]
                                    [ text "Noe gikk galt..." ]
                                ]

                LeggTilMuntlig språkKode ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            [ div [ class "inputkolonne" ] [ muntligKnapp språkKode Morsmål ]
                            , div [ class "inputkolonne" ] [ muntligKnapp språkKode Nybegynner ]
                            , div [ class "inputkolonne" ] [ muntligKnapp språkKode Godt ]
                            , div [ class "inputkolonne" ] [ muntligKnapp språkKode VeldigGodt ]
                            ]
                        ]

                LeggTilSkriftlig språkKode ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            [ div [ class "inputkolonne" ] [ skriftligKnapp språkKode.språkNavn Morsmål ]
                            , div [ class "inputkolonne" ] [ skriftligKnapp språkKode.språkNavn Nybegynner ]
                            , div [ class "inputkolonne" ] [ skriftligKnapp språkKode.språkNavn Godt ]
                            , div [ class "inputkolonne" ] [ skriftligKnapp språkKode.språkNavn VeldigGodt ]
                            ]
                        ]

                LagrerSpråk _ ->
                    text ""

                LagringFeilet _ _ ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ Knapp.knapp SendSkjemaPåNytt "Ja, prøv på nytt"
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

                SpråkKodeneFeilet _ ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ Knapp.knapp BrukerVilHenteSpråkKoderPåNytt "Ja, prøv på nytt"
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
