module Jobbprofil.Seksjon exposing
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
import DebugStatus exposing (DebugStatus)
import Feilmelding
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.Knapp as Knapp
import Html exposing (Html)
import Http
import Jobbprofil.Jobbprofil exposing (Jobbprofil)
import Jobbprofil.Skjema as Skjema exposing (JobbprofilSkjema, ValidertJobbprofilSkjema, geografiListeFraSkjema, geografiSammendragFraSkjema, omfangsListeFraSkjema, stillingListeFraSkjema, stillingSammendragFraSkjema)
import Meldinger.Melding as Melding exposing (Melding)
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Person exposing (BrukerInfo(..))
import Time exposing (Posix)



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , debugStatus : DebugStatus
    , sistLagretFraCV : Posix
    , brukerInfo : BrukerInfo
    }


type Samtale
    = LasterJobbprofil
    | HentingAvJobbprofilFeilet Http.Error
    | HarJobbprofilJobbsøker Jobbprofil
    | HarJobbprofilUnderOppfølging Jobbprofil
    | HarIkkeJobbprofilJobbsøker
    | LeggTilYrke Bool


type FullføringStatus
    = LagringLyktesFørsteGang
    | LagringLyktesEtterFlereForsøk
    | BrukerGikkVidere


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Jobbprofil FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model info) =
    info.seksjonsMeldingsLogg



--- UPDATE ---


type Msg
    = JobbprofilHentet (Result Http.Error Jobbprofil)
    | VilEndreJobbprofil
    | VilLagreJobbprofil
    | VilBegynnePåJobbprofil
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | WindowEndrerVisibility Visibility
    | ErrorLogget


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        JobbprofilHentet result ->
            let
                underOppfølging =
                    case model.brukerInfo of
                        UnderOppfølging _ ->
                            True

                        JobbSkifter _ ->
                            False
            in
            case result of
                Ok jobbprofil ->
                    let
                        nesteSamtaleSteg =
                            if underOppfølging then
                                HarJobbprofilUnderOppfølging jobbprofil

                            else
                                HarJobbprofilJobbsøker jobbprofil
                    in
                    ( nesteSamtaleSteg
                        |> oppdaterSamtale model UtenSvar
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Err error ->
                    case error of
                        Http.BadStatus 404 ->
                            let
                                nesteSamtaleSteg =
                                    if underOppfølging then
                                        LeggTilYrke True

                                    else
                                        HarIkkeJobbprofilJobbsøker
                            in
                            ( nesteSamtaleSteg
                                |> oppdaterSamtale model UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, logFeilmelding error "Hente jobbprofil" )

        VilBegynnePåJobbprofil ->
            ( LeggTilYrke False
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            SamtaleAnimasjon.update model.debugStatus samtaleAnimasjonMsg model.seksjonsMeldingsLogg
                |> updateEtterFullførtMelding model

        VilEndreJobbprofil ->
            IkkeFerdig ( Model model, Cmd.none )

        VilLagreJobbprofil ->
            IkkeFerdig ( Model model, Cmd.none )

        WindowEndrerVisibility visibility ->
            IkkeFerdig ( Model model, Cmd.none )

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding model ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                --VenterPåAnimasjonFørFullføring  _ ->
                --  Ferdig (sistLagret (Model model)) ferdigAnimertSamtale
                _ ->
                    ( Model { model | seksjonsMeldingsLogg = nyMeldingsLogg }
                    , Cmd.batch
                        [ Cmd.map SamtaleAnimasjonMsg cmd

                        -- , settFokus model.aktivSamtale
                        ]
                    )
                        |> IkkeFerdig

        MeldingerGjenstår ->
            ( Model { model | seksjonsMeldingsLogg = nyMeldingsLogg }
            , Cmd.map SamtaleAnimasjonMsg cmd
            )
                |> IkkeFerdig


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg jobbprofilSamtale =
    case jobbprofilSamtale of
        LasterJobbprofil ->
            []

        HarIkkeJobbprofilJobbsøker ->
            [ Melding.spørsmål
                [ "Vi må vite litt mer om jøbbønskene dine for at CV-en skal bli søkbar. Er du klar til å begynne?"
                ]
            ]

        HarJobbprofilJobbsøker jobbprofil ->
            [ Melding.spørsmål
                (List.concat
                    [ [ "Jeg ser du har en jobbprofil fra før av. Du har lagt inn dette:"
                      , Melding.tomLinje
                      ]
                    , jobbprofil
                        |> Skjema.fraJobbprofil
                        |> skjemaOppsummering
                    , [ Melding.tomLinje
                      , "Er informasjonen riktig?"
                      ]
                    ]
                )
            ]

        HarJobbprofilUnderOppfølging jobbprofil ->
            [ Melding.spørsmål
                (List.concat
                    [ [ "Bra! Nå gjenstår bare jobbprofilen. Jeg ser du har lagt inn dette tidligere:"
                      , Melding.tomLinje
                      ]
                    , jobbprofil
                        |> Skjema.fraJobbprofil
                        |> skjemaOppsummering
                    , [ Melding.tomLinje
                      , "Er informasjonen riktig?"
                      ]
                    ]
                )
            ]

        HentingAvJobbprofilFeilet error ->
            --todo: håndter feil
            []

        LeggTilYrke underOppfølging ->
            if underOppfølging then
                [ Melding.spørsmål [ "Bra! Nå gjenstår bare jobbprofilen." ]
                , Melding.spørsmål [ "Hva slags stillinger eller yrker ser du etter? For eksempel møbelsnekker eller butikkmedarbeider." ]
                ]

            else
                [ Melding.spørsmål [ "Flott! Da begynner vi." ]
                , Melding.spørsmål
                    [ "Hva slags stillinger eller yrker ser du etter? For eksempel møbelsnekker eller butikkmedarbeider."
                    ]
                ]


skjemaOppsummering : JobbprofilSkjema -> List String
skjemaOppsummering skjema =
    [ "Stilling/yrke: " ++ String.join ", " (stillingSammendragFraSkjema (stillingListeFraSkjema skjema))
    , "Område: " ++ String.join ", " (geografiSammendragFraSkjema (geografiListeFraSkjema skjema))
    , "Heltid/deltid: " ++ String.join ", " (omfangsListeFraSkjema skjema)
    ]


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError (always ErrorLogget))
        |> Maybe.withDefault Cmd.none


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



--- VIEW ---


type InputId
    = BekreftJobbprofilId
    | BegynnPåJobbprofilId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        BekreftJobbprofilId ->
            "jobbprofil-bekreft-id"

        BegynnPåJobbprofilId ->
            "jobbprofil-begynn-id"


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput model =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            LasterJobbprofil ->
                BrukerInput.utenInnhold

            HarIkkeJobbprofilJobbsøker ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilBegynnePåJobbprofil "Ja!"
                        |> Knapp.withId (inputIdTilString BegynnPåJobbprofilId)
                    ]

            HarJobbprofilJobbsøker _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilLagreJobbprofil "Ja, det er riktig"
                        |> Knapp.withId (inputIdTilString BekreftJobbprofilId)
                    , Knapp.knapp VilEndreJobbprofil "Nei, jeg vil endre"
                    ]

            HentingAvJobbprofilFeilet _ ->
                BrukerInput.utenInnhold

            HarJobbprofilUnderOppfølging jobbprofil ->
                BrukerInput.utenInnhold

            LeggTilYrke _ ->
                BrukerInput.utenInnhold

    else
        BrukerInput.utenInnhold



--- INIT ---


init : DebugStatus -> Posix -> BrukerInfo -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
init debugStatus sistLagretFraCV brukerInfo gammelMeldingsLogg =
    let
        aktivSamtale =
            LasterJobbprofil
    in
    ( Model
        { seksjonsMeldingsLogg =
            gammelMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale)
        , aktivSamtale = aktivSamtale
        , brukerInfo = brukerInfo
        , debugStatus = debugStatus
        , sistLagretFraCV = sistLagretFraCV
        }
    , Cmd.batch
        [ lagtTilSpørsmålCmd debugStatus
        , Api.getJobbprofil JobbprofilHentet
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
