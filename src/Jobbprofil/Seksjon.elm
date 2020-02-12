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
import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Browser.Events exposing (Visibility(..))
import DebugStatus exposing (DebugStatus)
import Feilmelding
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.BrukerInputMedGaVidereKnapp as BrukerInputMedGåVidereKnapp
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Typeahead
import Html exposing (Html)
import Http
import Jobbprofil.Jobbprofil as Jobbprofil exposing (Jobbprofil)
import Jobbprofil.Skjema as Skjema exposing (JobbprofilSkjema, SeksjonValg(..), ValidertJobbprofilSkjema, ansettelsesformListeFraSkjema, ansettelsesformSammendragFraSkjema, arbeidsdagerListeFraSkjema, arbeidstidListeFraSkjema, arbeidstidordningListeFraSkjema, fraJobbprofil, geografiListeFraSkjema, geografiSammendragFraSkjema, hentValg, kompetanseListeFraSkjema, kompetanseSammendragFraSkjema, listeSammendragFraSkjema, nårKanDuJobbeSammendragFraSkjema, omfangsSammendragFraSkjema, oppstartSammendragFraSkjema, stillingListeFraSkjema, stillingSammendragFraSkjema)
import Meldinger.Melding as Melding exposing (Melding)
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Person exposing (BrukerInfo(..))
import Process
import Result.Extra as Result
import Task
import Time exposing (Posix)
import Typeahead.Typeahead as Typeahead exposing (GetSuggestionStatus(..), InputStatus(..))



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
    | LeggTilYrke { underOppfølging : Bool, visFeilmelding : Bool } (Typeahead.Model Yrke)
    | EndreOppsummering (Typeahead.Model Yrke) JobbprofilSkjema
    | VelgOmfang
    | VelgArbeidstid


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
    | VilLagreOmfang
    | VilBegynnePåJobbprofil
    | YrkeTypeaheadMsg (Typeahead.Msg Yrke)
    | HentetYrkeTypeahead Typeahead.Query (Result Http.Error (List Yrke))
    | VilGåVidereFraYrke
    | JobbprofilEndret SkjemaEndring
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | WindowEndrerVisibility Visibility
    | ErrorLogget
    | FeltMisterFokus
    | TimeoutEtterAtFeltMistetFokus


type SkjemaEndring
    = Omfang


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
                                        initSamtaleTypeahead
                                            |> Tuple.first
                                            |> LeggTilYrke { underOppfølging = True, visFeilmelding = False }

                                    else
                                        HarIkkeJobbprofilJobbsøker
                            in
                            ( nesteSamtaleSteg
                                |> oppdaterSamtale model UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, logFeilmelding "Hente jobbprofil" error )

        VilBegynnePåJobbprofil ->
            ( initSamtaleTypeahead
                |> Tuple.first
                |> LeggTilYrke { underOppfølging = False, visFeilmelding = False }
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        YrkeTypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                LeggTilYrke info typeaheadModel ->
                    updateSamtaleYrkeTypeahead model info typeaheadMsg typeaheadModel

                EndreOppsummering gammelTypeaheadModel skjema ->
                    --Todo: implementer
                    IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetYrkeTypeahead query result ->
            case model.aktivSamtale of
                LeggTilYrke info typeaheadModel ->
                    ( result
                        |> Typeahead.updateSuggestions Yrke.label typeaheadModel query
                        |> LeggTilYrke info
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente Yrketypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                EndreOppsummering typeaheadModel skjema ->
                    ( EndreOppsummering (Typeahead.updateSuggestions Yrke.label typeaheadModel query result) skjema
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente Yrketypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilGåVidereFraYrke ->
            IkkeFerdig ( Model model, Cmd.none )

        VilEndreJobbprofil ->
            IkkeFerdig ( Model model, Cmd.none )

        VilLagreJobbprofil ->
            IkkeFerdig ( Model model, Cmd.none )

        JobbprofilEndret skjemaEndring ->
            IkkeFerdig ( Model model, Cmd.none )

        VilLagreOmfang ->
            IkkeFerdig ( Model model, Cmd.none )

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            SamtaleAnimasjon.update model.debugStatus samtaleAnimasjonMsg model.seksjonsMeldingsLogg
                |> updateEtterFullførtMelding model

        WindowEndrerVisibility visibility ->
            IkkeFerdig ( Model model, Cmd.none )

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )

        FeltMisterFokus ->
            IkkeFerdig ( Model model, mistetFokusCmd )

        TimeoutEtterAtFeltMistetFokus ->
            case model.aktivSamtale of
                LeggTilYrke info typeaheadModel ->
                    visFeilmeldingForYrke model info typeaheadModel

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )


updateSamtaleYrkeTypeahead : ModelInfo -> { underOppfølging : Bool, visFeilmelding : Bool } -> Typeahead.Msg Yrke -> Typeahead.Model Yrke -> SamtaleStatus
updateSamtaleYrkeTypeahead model info msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Yrke.label msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected typeaheadModel of
                Just yrke ->
                    -- brukerVelgerYrke model (YrkeTypeaheadMsg msg) yrke
                    ( typeaheadModel
                        |> LeggTilYrke { underOppfølging = info.underOppfølging, visFeilmelding = False }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForYrke model info nyTypeaheadModel

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilYrke info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilYrke info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getYrkeJobbprofilTypeahead HentetYrkeTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilYrke info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView Yrke.label Nothing
                    |> Cmd.map YrkeTypeaheadMsg
                )


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
                    [ [ "Nå gjenstår bare jobbprofilen. Jeg ser du har lagt inn dette tidligere:"
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

        VelgOmfang ->
            [ Melding.spørsmål [ "Vil du jobbe heltid eller deltid?" ] ]

        VelgArbeidstid ->
            [ Melding.spørsmål [ "Når kan du jobbe?" ] ]

        LeggTilYrke info _ ->
            if info.underOppfølging then
                [ Melding.spørsmål [ "Nå gjenstår bare jobbprofilen." ]
                , Melding.spørsmål [ "Hva slags stillinger eller yrker ser du etter? For eksempel møbelsnekker eller butikkmedarbeider." ]
                ]

            else
                [ Melding.spørsmål [ "Flott! Da begynner vi." ]
                , Melding.spørsmål
                    [ "Hva slags stillinger eller yrker ser du etter? For eksempel møbelsnekker eller butikkmedarbeider."
                    ]
                ]

        EndreOppsummering model jobbprofilSkjema ->
            []


skjemaOppsummering : JobbprofilSkjema -> List String
skjemaOppsummering skjema =
    [ "Stilling/yrke: " ++ stillingSammendragFraSkjema skjema
    , "Område: " ++ geografiSammendragFraSkjema skjema
    , "Heltid/deltid: " ++ omfangsSammendragFraSkjema skjema
    , "Når kan du jobbe? " ++ nårKanDuJobbeSammendragFraSkjema skjema
    , "Hva slags ansettelse ønsker du? " ++ ansettelsesformSammendragFraSkjema skjema
    , "Når kan du begynne? " ++ oppstartSammendragFraSkjema skjema
    , "Kompetanser: " ++ kompetanseSammendragFraSkjema skjema
    ]


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


logFeilmelding : String -> Http.Error -> Cmd Msg
logFeilmelding operasjon error =
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


mistetFokusCmd : Cmd Msg
mistetFokusCmd =
    Process.sleep 100
        |> Task.perform (\_ -> TimeoutEtterAtFeltMistetFokus)



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


type InputId
    = BekreftJobbprofilId
    | BegynnPåJobbprofilId
    | StillingYrkeTypeaheadId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        BekreftJobbprofilId ->
            "jobbprofil-bekreft-id"

        BegynnPåJobbprofilId ->
            "jobbprofil-begynn-id"

        StillingYrkeTypeaheadId ->
            "jobbprofil-yrke-typeahaed-id"


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


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

            LeggTilYrke info typeaheadModel ->
                (typeaheadModel
                    |> feilmeldingTypeahead
                    |> maybeHvisTrue info.visFeilmelding
                    |> Typeahead.toViewElement Yrke.label typeaheadModel
                    |> FrontendModuler.Typeahead.map YrkeTypeaheadMsg
                )
                    |> BrukerInputMedGåVidereKnapp.typeahead VilGåVidereFraYrke
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            VelgOmfang ->
                BrukerInput.skjema { lagreMsg = VilLagreOmfang, lagreKnappTekst = "Gå videre" }
                    [ Checkbox.checkbox "Heltid" (JobbprofilEndret Omfang) False
                        |> Checkbox.toHtml
                    ]

            VelgArbeidstid ->
                BrukerInput.utenInnhold

            EndreOppsummering typeaheadMOdel jobbprofilSkjema ->
                BrukerInput.utenInnhold

    else
        BrukerInput.utenInnhold


feilmeldingTypeahead : Typeahead.Model Yrke -> Maybe String
feilmeldingTypeahead typeaheadModel =
    case Typeahead.selected typeaheadModel of
        Just _ ->
            Nothing

        Nothing ->
            Just "Velg et yrke fra listen med forslag som kommer opp"


visFeilmeldingForYrke : ModelInfo -> { underOppfølging : Bool, visFeilmelding : Bool } -> Typeahead.Model Yrke -> SamtaleStatus
visFeilmeldingForYrke model info typeaheadModel =
    ( typeaheadModel
        |> LeggTilYrke { underOppfølging = info.underOppfølging, visFeilmelding = True }
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig



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


initSamtaleTypeahead : ( Typeahead.Model Yrke, Typeahead.Query )
initSamtaleTypeahead =
    Typeahead.init
        { value = ""
        , label = "Hvilken stilling/yrke har du?"
        , id = inputIdTilString StillingYrkeTypeaheadId
        , toString = Yrke.label
        }


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Browser.Events.onVisibilityChange WindowEndrerVisibility
        , model.seksjonsMeldingsLogg
            |> SamtaleAnimasjon.subscriptions
            |> Sub.map SamtaleAnimasjonMsg
        ]
