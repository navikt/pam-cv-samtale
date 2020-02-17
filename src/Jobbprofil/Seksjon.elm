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
import FrontendModuler.Merkelapp as Merkelapp exposing (Merkelapp)
import FrontendModuler.Typeahead
import Html exposing (Html, text)
import Http
import Jobbprofil.Jobbprofil as Jobbprofil exposing (Jobbprofil)
import Jobbprofil.Omrade as Omrade exposing (Omrade)
import Jobbprofil.Skjema as Skjema exposing (JobbprofilSkjema, SeksjonValg(..), ValidertJobbprofilSkjema, ansettelsesformListeFraSkjema, ansettelsesformSammendragFraSkjema, arbeidsdagerListeFraSkjema, arbeidstidListeFraSkjema, arbeidstidordningListeFraSkjema, fraJobbprofil, geografiListeFraSkjema, geografiSammendragFraSkjema, hentValg, kompetanseListeFraSkjema, kompetanseSammendragFraSkjema, listeSammendragFraSkjema, nårKanDuJobbeSammendragFraSkjema, omfangsSammendragFraSkjema, oppstartSammendragFraSkjema, stillingListeFraSkjema, stillingSammendragFraSkjema)
import List.Extra as List
import Maybe.Extra
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
    | LeggTilYrker YrkeInfo (Typeahead.Model Yrke)
    | LeggTilOmrader OmradeInfo (Typeahead.Model Omrade)
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
    | VilBegynnePåJobbprofil
    | YrkeTypeaheadMsg (Typeahead.Msg Yrke)
    | HentetYrkeTypeahead Typeahead.Query (Result Http.Error (List Yrke))
    | VilLeggeTilYrke Yrke
    | FjernValgtYrke Yrke
    | VilGåVidereFraYrke YrkeInfo
    | OmradeTypeaheadMsg (Typeahead.Msg Omrade)
    | HentetOmradeTypeahead Typeahead.Query (Result Http.Error (List Omrade))
    | VilLeggeTilOmrade Omrade
    | VilGåVidereFraOmrade OmradeInfo
    | FjernValgtOmrade Omrade
    | VilLagreOmfang
    | JobbprofilEndret SkjemaEndring
    | VilLagreJobbprofil
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | WindowEndrerVisibility Visibility
    | ErrorLogget
    | FeltMisterFokus
    | TimeoutEtterAtFeltMistetFokus


type SkjemaEndring
    = Omfang


type alias YrkeInfo =
    { yrker : List Yrke
    , underOppfølging : Bool -- todo: fjern underOppfølging herfra
    , visFeilmelding : Bool
    }


type alias OmradeInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , visFeilmelding : Bool
    }


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
                                        initYrkeTypeahead
                                            |> Tuple.first
                                            |> LeggTilYrker { yrker = [], underOppfølging = True, visFeilmelding = False }

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
            ( initYrkeTypeahead
                |> Tuple.first
                |> LeggTilYrker { yrker = [], underOppfølging = False, visFeilmelding = False }
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        OmradeTypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                LeggTilOmrader info typeaheadModel ->
                    updateSamtaleOmradeTypeahead model info typeaheadMsg typeaheadModel

                EndreOppsummering gammelTypeaheadModel skjema ->
                    --Todo: implementer
                    IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FjernValgtOmrade omrade ->
            case model.aktivSamtale of
                LeggTilOmrader info typeaheadModel ->
                    ( typeaheadModel
                        |> LeggTilOmrader { info | omrader = List.remove omrade info.omrader }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLeggeTilOmrade _ ->
            IkkeFerdig ( Model model, Cmd.none )

        VilGåVidereFraOmrade info ->
            case List.isEmpty info.omrader of
                True ->
                    Debug.log "Liste er tom"
                        IkkeFerdig
                        ( Model model, Cmd.none )

                False ->
                    Debug.log "Liste har verdier"
                        IkkeFerdig
                        ( Model model, Cmd.none )

        HentetOmradeTypeahead query result ->
            case model.aktivSamtale of
                LeggTilOmrader info typeaheadModel ->
                    let
                        resultWithoutSelected =
                            result
                                |> Result.map (List.filter (\omrade_ -> List.notMember omrade_ info.omrader))
                    in
                    ( resultWithoutSelected
                        |> Typeahead.updateSuggestions Omrade.tittel typeaheadModel query
                        |> LeggTilOmrader info
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente omradetypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                EndreOppsummering typeaheadModel skjema ->
                    -- todo: Gjør det samme som for legg til yrker her
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        YrkeTypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                LeggTilYrker info typeaheadModel ->
                    updateSamtaleYrkeTypeahead model info typeaheadMsg typeaheadModel

                EndreOppsummering gammelTypeaheadModel skjema ->
                    --Todo: implementer
                    IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetYrkeTypeahead query result ->
            case model.aktivSamtale of
                LeggTilYrker info typeaheadModel ->
                    let
                        resultWithoutSelected =
                            result
                                |> Result.map (List.filter (\yrke_ -> List.notMember yrke_ info.yrker))
                    in
                    ( resultWithoutSelected
                        |> Typeahead.updateSuggestions Yrke.label typeaheadModel query
                        |> LeggTilYrker info
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente Yrketypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                EndreOppsummering typeaheadModel skjema ->
                    -- todo: Gjør det samme som for legg til yrker her
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilLeggeTilYrke _ ->
            IkkeFerdig ( Model model, Cmd.none )

        FjernValgtYrke yrke ->
            case model.aktivSamtale of
                LeggTilYrker info typeaheadModel ->
                    ( typeaheadModel
                        |> LeggTilYrker { info | yrker = List.remove yrke info.yrker }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilGåVidereFraYrke info ->
            case List.isEmpty info.yrker of
                True ->
                    {- ( typeaheadModel
                           |> LeggTilYrker { info | visFeilmelding = True }
                           |> oppdaterSamtale model IngenNyeMeldinger
                       , lagtTilSpørsmålCmd model.debugStatus
                       )
                           |> IkkeFerdig
                    -}
                    IkkeFerdig ( Model model, Cmd.none )

                False ->
                    ( initOmradeTypeahead
                        |> Tuple.first
                        |> LeggTilOmrader { yrker = info.yrker, omrader = [], visFeilmelding = False }
                        |> oppdaterSamtale model
                            (ManueltSvar
                                (Melding.svar
                                    [ String.join ", " (List.map (\it -> Yrke.label it) info.yrker)
                                    ]
                                )
                            )
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

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
                LeggTilYrker info typeaheadModel ->
                    visFeilmeldingForYrke model info typeaheadModel

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )


updateSamtaleOmradeTypeahead : ModelInfo -> OmradeInfo -> Typeahead.Msg Omrade -> Typeahead.Model Omrade -> SamtaleStatus
updateSamtaleOmradeTypeahead model info msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Omrade.tittel msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just omrade ->
                    ( nyTypeaheadModel
                        |> LeggTilOmrader { info | omrader = List.append info.omrader [ omrade ], visFeilmelding = False }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForOmrade model info typeaheadModel

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilOmrader info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilOmrader info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        Api.getOmradeJobbprofilTypeahead HentetOmradeTypeahead query

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilOmrader info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView Omrade.tittel Nothing
                    |> Cmd.map OmradeTypeaheadMsg
                )


updateSamtaleYrkeTypeahead : ModelInfo -> YrkeInfo -> Typeahead.Msg Yrke -> Typeahead.Model Yrke -> SamtaleStatus
updateSamtaleYrkeTypeahead model info msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Yrke.label msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just yrke ->
                    -- brukerVelgerYrke model (YrkeTypeaheadMsg msg) yrke
                    ( nyTypeaheadModel
                        |> LeggTilYrker { yrker = List.append info.yrker [ yrke ], underOppfølging = info.underOppfølging, visFeilmelding = False }
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                Nothing ->
                    visFeilmeldingForYrke model info nyTypeaheadModel

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilYrker info
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> LeggTilYrker info
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
                    |> LeggTilYrker info
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

        LeggTilYrker info _ ->
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

        LeggTilOmrader info _ ->
            [ Melding.spørsmål [ "Hvor vil du jobbe? For eksempel Oslo eller Kristiansund." ] ]

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
    | OmradeTypeaheadId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        BekreftJobbprofilId ->
            "jobbprofil-bekreft-id"

        BegynnPåJobbprofilId ->
            "jobbprofil-begynn-id"

        StillingYrkeTypeaheadId ->
            "jobbprofil-yrke-typeahaed-id"

        OmradeTypeaheadId ->
            "jobbprofil-omrade-typeahead-id"


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

            LeggTilOmrader info typeaheadModel ->
                BrukerInput.skjema { lagreMsg = VilGåVidereFraOmrade info, lagreKnappTekst = "Gå videre" }
                    (List.concat
                        [ [ Nothing
                                |> Typeahead.view Omrade.tittel typeaheadModel
                                |> Html.map OmradeTypeaheadMsg
                          ]
                        , if List.length info.omrader > 0 then
                            [ List.map (\x -> Merkelapp.merkelapp (FjernValgtOmrade x) (Omrade.tittel x)) info.omrader
                                |> Merkelapp.merkelapper
                            ]

                          else
                            []
                        ]
                    )

            LeggTilYrker info typeaheadModel ->
                -- todo: Lag en variant av skjema for dette tilfelle som har gåvidereknapp istedet for lagre
                BrukerInput.skjema { lagreMsg = VilGåVidereFraYrke info, lagreKnappTekst = "Gå videre" }
                    (List.concat
                        [ [ --info.yrker
                            --  |> feilmeldingTypeahead
                            --  |> maybeHvisTrue info.visFeilmelding
                            Nothing
                                |> Typeahead.view Yrke.label typeaheadModel
                                |> Html.map YrkeTypeaheadMsg
                          ]
                        , if List.length info.yrker > 0 then
                            [ List.map (\x -> Merkelapp.merkelapp (FjernValgtYrke x) (Yrke.label x)) info.yrker
                                |> Merkelapp.merkelapper
                            ]

                          else
                            []
                        ]
                    )

            {--
                 (typeaheadModel
                     |> feilmeldingTypeahead
                     |> maybeHvisTrue info.visFeilmelding
                     |> Typeahead.toViewElement Yrke.label typeaheadModel
                     |> FrontendModuler.Typeahead.map YrkeTypeaheadMsg
                 )
                     |> BrukerInputMedGåVidereKnapp.typeahead VilGåVidereFraYrke
                     |> BrukerInput.brukerInputMedGåVidereKnapp


                                    --}
            VelgOmfang ->
                BrukerInput.skjema { lagreMsg = VilLagreOmfang, lagreKnappTekst = "Gå videre" }
                    [ Checkbox.checkbox "Heltid" (JobbprofilEndret Omfang) False
                        |> Checkbox.toHtml
                    ]

            VelgArbeidstid ->
                BrukerInput.utenInnhold

            EndreOppsummering typeaheadModel jobbprofilSkjema ->
                Debug.log "yo"
                    BrukerInput.utenInnhold

    else
        Debug.log "hei"
            BrukerInput.utenInnhold


omradeMerkelapp : Omrade -> Merkelapp Msg
omradeMerkelapp omrade =
    Merkelapp.merkelapp (FjernValgtOmrade omrade) (Omrade.tittel omrade)


yrkeMerkelapp : Yrke -> Merkelapp Msg
yrkeMerkelapp yrke =
    Merkelapp.merkelapp (FjernValgtYrke yrke) (Yrke.label yrke)


feilmeldingTypeahead : List Yrke -> Maybe String
feilmeldingTypeahead yrker =
    if List.length yrker == 0 then
        Just "Skriv inn et yrke eller en stilling. Velg fra listen med forslag som kommer opp."

    else
        Nothing


visFeilmeldingForOmrade : ModelInfo -> OmradeInfo -> Typeahead.Model Omrade -> SamtaleStatus
visFeilmeldingForOmrade model info typeaheadModel =
    ( typeaheadModel
        |> LeggTilOmrader { info | visFeilmelding = True }
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig


visFeilmeldingForYrke : ModelInfo -> YrkeInfo -> Typeahead.Model Yrke -> SamtaleStatus
visFeilmeldingForYrke model info typeaheadModel =
    ( typeaheadModel
        |> LeggTilYrker { info | visFeilmelding = True }
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


initOmradeTypeahead : ( Typeahead.Model Omrade, Typeahead.Query )
initOmradeTypeahead =
    Typeahead.init
        { value = ""
        , label = "Skriv inn fylker eller kommuner"
        , id = inputIdTilString OmradeTypeaheadId
        , toString = Omrade.tittel
        }
        |> Tuple.mapFirst Typeahead.withSubmitOnElementSelected


initYrkeTypeahead : ( Typeahead.Model Yrke, Typeahead.Query )
initYrkeTypeahead =
    Typeahead.init
        { value = ""
        , label = "Hvilken stilling/yrke har du?"
        , id = inputIdTilString StillingYrkeTypeaheadId
        , toString = Yrke.label
        }
        |> Tuple.mapFirst Typeahead.withSubmitOnElementSelected


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Browser.Events.onVisibilityChange WindowEndrerVisibility
        , model.seksjonsMeldingsLogg
            |> SamtaleAnimasjon.subscriptions
            |> Sub.map SamtaleAnimasjonMsg
        ]
