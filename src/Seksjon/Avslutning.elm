module Seksjon.Avslutning exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , update
    , viewBrukerInput
    )

-- MODEL --

import Api
import Browser.Dom as Dom
import Cv.Cv exposing (Cv)
import DebugStatus exposing (DebugStatus)
import FrontendModuler.Knapp as Knapp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Melding exposing (Melding)
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Person exposing (Person)
import Personalia exposing (Personalia)
import Process
import SamtaleAnimasjon
import Task


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , cv : Cv
    , personalia : Personalia
    , debugStatus : DebugStatus
    }


type Samtale
    = Intro
    | UnderOppfølging
    | DelMedArbeidsgiver Bool
    | AvsluttendeOrd
    | LagringSynlighetFeilet
    | HentPersonFeilet
    | VenterPåAnimasjonFørFullføring


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Model FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



-- UPDATE --


type Msg
    = BrukerGodkjennerSynligCV String
    | BrukerGodkjennerIkkeSynligCV String
    | BrukerVilHentePersonPåNytt String
    | BrukerVilAvslutte String
    | PersonHentet (Result Http.Error Person)
    | SynlighetPostet (Result Http.Error Bool)
    | ViewportSatt (Result Dom.Error ())
    | StartÅSkrive
    | FullførMelding


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        ViewportSatt result ->
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

        PersonHentet result ->
            case result of
                Ok value ->
                    if Person.underOppfolging value then
                        ( nesteSamtaleStegUtenMelding model UnderOppfølging
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else if Person.cvSynligForArbeidsgiver value then
                        ( nesteSamtaleStegUtenMelding model (DelMedArbeidsgiver True)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                    else
                        ( nesteSamtaleStegUtenMelding model (DelMedArbeidsgiver False)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )
                            |> IkkeFerdig

                Err error ->
                    ( nesteSamtaleStegUtenMelding model HentPersonFeilet
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BrukerGodkjennerSynligCV knappeTekst ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (Melding.svar [ knappeTekst ])
                }
            , Cmd.batch
                [ lagtTilSpørsmålCmd model.debugStatus
                , Api.postSynlighet SynlighetPostet True
                ]
            )
                |> IkkeFerdig

        BrukerGodkjennerIkkeSynligCV knappeTekst ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (Melding.svar [ knappeTekst ])
                }
            , Cmd.batch
                [ lagtTilSpørsmålCmd model.debugStatus
                , Api.postSynlighet SynlighetPostet False
                ]
            )
                |> IkkeFerdig

        SynlighetPostet result ->
            case result of
                Ok value ->
                    ( nesteSamtaleStegUtenMelding model AvsluttendeOrd, lagtTilSpørsmålCmd model.debugStatus ) |> IkkeFerdig

                Err error ->
                    ( nesteSamtaleStegUtenMelding model LagringSynlighetFeilet
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BrukerVilHentePersonPåNytt knappeTekst ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (Melding.svar [ knappeTekst ])
                }
            , Cmd.batch
                [ lagtTilSpørsmålCmd model.debugStatus
                , hentSynlighet
                ]
            )
                |> IkkeFerdig

        BrukerVilAvslutte knappeTekst ->
            ( nesteSamtaleSteg model (Melding.svar [ knappeTekst ]) AvsluttendeOrd
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig


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


nesteSamtaleStegUtenMelding : ModelInfo -> Samtale -> Model
nesteSamtaleStegUtenMelding model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring ->
                    Ferdig (Model model) ferdigAnimertSamtale

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


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg avslutningsSeksjon =
    case avslutningsSeksjon of
        VenterPåAnimasjonFørFullføring ->
            []

        Intro ->
            [ Melding.spørsmål
                [ "I denne CV-tjenesten kan arbeidsgivere søke opp CV-en din. Hvis de har en ledig jobb du kan passe til, kan de ta kontakt." ]
            ]

        DelMedArbeidsgiver synlig ->
            if synlig then
                [ Melding.spørsmål
                    [ "CV-en din er allerede synlig for arbeidsgivere!"
                    , "Ønsker du fremdeles at arbeidsgivere skal kunne se CV-en din?"
                    ]
                ]

            else
                [ Melding.spørsmål
                    [ "Ønsker du at arbeidsgivere skal kunne se CV-en din?" ]
                ]

        UnderOppfølging ->
            [ Melding.spørsmål [ "Siden du er under oppfølging fra NAV, så vil CV-en din være synlig for arbeidsgivere og NAV-veiledere." ]
            , Melding.spørsmål [ "Bra innsats! 👍👍 Alt du har lagt inn er nå lagret i CV-en din." ]
            , Melding.spørsmål [ "Da er vi ferdige med CV-en. Husk at du når som helst kan endre og forbedre den." ]
            , Melding.spørsmål [ "Lykke til med jobbjakten! 😊" ]
            ]

        AvsluttendeOrd ->
            [ Melding.spørsmål [ "Bra innsats! 👍👍 Alt du har lagt inn er nå lagret i CV-en din." ]
            , Melding.spørsmål [ "Da er vi ferdige med CV-en. Husk at du når som helst kan endre og forbedre den." ]
            , Melding.spørsmål [ "Lykke til med jobbjakten! 😊" ]
            ]

        HentPersonFeilet ->
            [ Melding.spørsmål
                [ "Oisann. Jeg klarte ikke å hente informasjonen for å fullføre registreringen."
                , "Vil du prøve på nytt?"
                ]
            ]

        LagringSynlighetFeilet ->
            [ Melding.spørsmål
                [ "Oops. Jeg klarte ikke å lagre informasjonen."
                , "Vil du prøve på nytt?"
                ]
            ]



-- VIEW --


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                AvsluttendeOrd ->
                    div []
                        [ div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ a [ href "/cv/forhandsvis", class "avslutt-knapp" ]
                                    [ div [ class "Knapp" ]
                                        [ text "Avslutt og vis CV-en min" ]
                                    ]
                                ]
                            ]
                        ]

                VenterPåAnimasjonFørFullføring ->
                    text ""

                DelMedArbeidsgiver _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            [ div [ class "inputkolonne" ]
                                [ let
                                    synligCV =
                                        "Ja, CV-en skal være synlig for arbeidsgivere"
                                  in
                                  Knapp.knapp (BrukerGodkjennerSynligCV synligCV) synligCV
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                ]
                            , div [ class "inputkolonne" ]
                                [ let
                                    ikkeSynligCV =
                                        "Nei, CV-en skal bare være synlig for meg"
                                  in
                                  Knapp.knapp (BrukerGodkjennerIkkeSynligCV ikkeSynligCV) ikkeSynligCV
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                Intro ->
                    text ""

                UnderOppfølging ->
                    div []
                        [ div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ a [ href "/cv/forhandsvis", class "avslutt-knapp" ]
                                    [ div [ class "Knapp" ]
                                        [ text "Avslutt og vis CV-en min" ]
                                    ]
                                ]
                            ]
                        ]

                HentPersonFeilet ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ let
                                hentBruker =
                                    "Ja, prøv på nytt"
                              in
                              Knapp.knapp (BrukerVilHentePersonPåNytt hentBruker) hentBruker
                                |> Knapp.withClass Knapp.SpråknivåKnapp
                                |> Knapp.toHtml
                            , let
                                avslutt =
                                    "Nei, jeg gjør det senere"
                              in
                              Knapp.knapp (BrukerVilAvslutte avslutt) avslutt
                                |> Knapp.withClass Knapp.SpråknivåKnapp
                                |> Knapp.toHtml
                            ]
                        ]

                LagringSynlighetFeilet ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            [ div [ class "inputkolonne" ]
                                [ let
                                    synligCV =
                                        "Ja, CV-en skal være synlig for arbeidsgivere"
                                  in
                                  Knapp.knapp (BrukerGodkjennerSynligCV synligCV) synligCV
                                    |> Knapp.toHtml
                                ]
                            , div [ class "inputkolonne" ]
                                [ let
                                    ikkeSynligCV =
                                        "Nei, CV-en skal bare være synlig for meg"
                                  in
                                  Knapp.knapp (BrukerGodkjennerIkkeSynligCV ikkeSynligCV) ikkeSynligCV
                                    |> Knapp.toHtml
                                ]
                            , div [ class "inputkolonne" ]
                                [ let
                                    avslutt =
                                        "Avslutt, jeg gjør det senere"
                                  in
                                  Knapp.knapp (BrukerVilAvslutte avslutt) avslutt
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

        MeldingerGjenstår ->
            text ""



-- INIT --


hentSynlighet : Cmd Msg
hentSynlighet =
    Api.getPerson PersonHentet


init : DebugStatus -> Personalia -> Cv -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
init debugStatus personalia cv gammelMeldingsLogg =
    let
        aktivSamtale =
            Intro
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , personalia = personalia
        , cv = cv
        , debugStatus = debugStatus
        }
    , Cmd.batch
        [ lagtTilSpørsmålCmd debugStatus
        , hentSynlighet
        ]
    )
