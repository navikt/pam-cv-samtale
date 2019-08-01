module Seksjon.Avslutning exposing
    ( Model(..)
    , Msg(..)
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
                , Process.sleep 1000
                    |> Task.perform (\_ -> FullførMelding)
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
                        , lagtTilSpørsmålCmd
                        )
                            |> IkkeFerdig

                    else if Person.cvSynligForArbeidsgiver value then
                        ( nesteSamtaleStegUtenMelding model (DelMedArbeidsgiver True)
                        , lagtTilSpørsmålCmd
                        )
                            |> IkkeFerdig

                    else
                        ( nesteSamtaleStegUtenMelding model (DelMedArbeidsgiver False)
                        , lagtTilSpørsmålCmd
                        )
                            |> IkkeFerdig

                Err error ->
                    ( nesteSamtaleStegUtenMelding model HentPersonFeilet
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

        BrukerGodkjennerSynligCV knappeTekst ->
            ( nesteSamtaleSteg model (Melding.svar [ knappeTekst ]) AvsluttendeOrd
            , Cmd.batch
                [ lagtTilSpørsmålCmd
                , Api.postSynlighet SynlighetPostet True
                ]
            )
                |> IkkeFerdig

        BrukerGodkjennerIkkeSynligCV knappeTekst ->
            ( nesteSamtaleSteg model (Melding.svar [ knappeTekst ]) AvsluttendeOrd
            , Cmd.batch
                [ lagtTilSpørsmålCmd
                , Api.postSynlighet SynlighetPostet False
                ]
            )
                |> IkkeFerdig

        SynlighetPostet result ->
            case result of
                Ok value ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

                Err error ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        BrukerVilHentePersonPåNytt knappeTekst ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (Melding.svar [ knappeTekst ])
                }
            , Cmd.batch
                [ lagtTilSpørsmålCmd
                , hentSynlighet
                ]
            )
                |> IkkeFerdig

        BrukerVilAvslutte knappeTekst ->
            ( nesteSamtaleSteg model (Melding.svar [ knappeTekst ]) AvsluttendeOrd
            , Cmd.batch
                [ lagtTilSpørsmålCmd
                ]
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
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig


lagtTilSpørsmålCmd : Cmd Msg
lagtTilSpørsmålCmd =
    Cmd.batch
        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
        , Process.sleep 200
            |> Task.perform (\_ -> StartÅSkrive)
        ]


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg avslutningsSeksjon =
    case avslutningsSeksjon of
        VenterPåAnimasjonFørFullføring ->
            []

        Intro ->
            [ Melding.spørsmål
                [ "I vår løsning kan arbeidsgivere søke opp CV-en din. De kan ta kontakt med deg hvis de har en ledig jobb du kan passe til." ]
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
            [ Melding.spørsmål
                [ "Siden du er under oppfølging fra NAV, så vil CV-en din være synlig for arbeidsgivere og NAV-veiledere." ]
            ]

        AvsluttendeOrd ->
            [ Melding.spørsmål [ "Bra innsats! Alt du har skrevet her er lagret i CV-en din." ]
            , Melding.spørsmål [ "Da er vi ferdige med CV-en. Husk at du når som helst kan endre og forbedre den." ]
            , Melding.spørsmål [ "Lykke til med jobbjakten! :)" ]
            ]

        HentPersonFeilet ->
            [ Melding.spørsmål
                [ "Oisann. Jeg klarte ikke å hente informasjonen for å fullføre registreringen."
                , "Vil du prøve på nytt?"
                ]
            ]

        LagringSynlighetFeilet ->
            [ Melding.spørsmål
                [ "Oops. Jeg klarte ikke å lagre iformasjonen."
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
                    text ""

                VenterPåAnimasjonFørFullføring ->
                    text ""

                DelMedArbeidsgiver _ ->
                    div [ class "inputrad" ]
                        [ div [ class "inputrad-innhold" ]
                            [ let
                                synligCV =
                                    "Ja, Cv-en skal være synlig for arbeidsgivere"
                              in
                              Knapp.knapp (BrukerGodkjennerSynligCV synligCV) synligCV
                                |> Knapp.toHtml
                            , let
                                ikkeSynligCV =
                                    "Nei, CV-en skal bare være synlig for meg"
                              in
                              Knapp.knapp (BrukerGodkjennerIkkeSynligCV ikkeSynligCV) ikkeSynligCV
                                |> Knapp.toHtml
                            ]
                        ]

                Intro ->
                    text ""

                UnderOppfølging ->
                    text ""

                HentPersonFeilet ->
                    div [ class "inputrad" ]
                        [ div [ class "inputrad-innhold" ]
                            [ let
                                hentBruker =
                                    "Ja, prøv på nytt"
                              in
                              Knapp.knapp (BrukerVilHentePersonPåNytt hentBruker) hentBruker
                                |> Knapp.toHtml
                            , let
                                avslutt =
                                    "Nei, jeg gjør det senere"
                              in
                              Knapp.knapp (BrukerVilAvslutte avslutt) avslutt
                                |> Knapp.toHtml
                            ]
                        ]

                LagringSynlighetFeilet ->
                    div [ class "inputrad" ]
                        [ div [ class "inputrad-innhold" ]
                            [ let
                                synligCV =
                                    "Ja, Cv-en skal være synlig for arbeidsgivere"
                              in
                              Knapp.knapp (BrukerGodkjennerSynligCV synligCV) synligCV
                                |> Knapp.toHtml
                            , let
                                ikkeSynligCV =
                                    "Nei, CV-en skal bare være synlig for meg"
                              in
                              Knapp.knapp (BrukerGodkjennerIkkeSynligCV ikkeSynligCV) ikkeSynligCV
                                |> Knapp.toHtml
                            ]
                        ]

        MeldingerGjenstår ->
            text ""



-- INIT --


hentSynlighet : Cmd Msg
hentSynlighet =
    Api.getPerson PersonHentet


init : Personalia -> Cv -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
init personalia cv gammelMeldingsLogg =
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
        }
    , Cmd.batch
        [ lagtTilSpørsmålCmd
        , hentSynlighet
        ]
    )
