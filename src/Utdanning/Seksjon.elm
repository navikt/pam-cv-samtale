module Utdanning.Seksjon exposing
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
import Dato.Dato as Dato exposing (TilDato(..), År)
import Dato.Maned as Måned exposing (Måned(..))
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.BrukerInputMedGaVidereKnapp as BrukerInputMedGåVidereKnapp
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp exposing (Knapp, Type(..))
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Select as Select
import FrontendModuler.Textarea as Textarea
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import LagreStatus exposing (LagreStatus)
import Meldinger.Melding as Melding exposing (Melding, Tekstområde(..))
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Process
import Task
import Tid exposing (nyestePosix)
import Time exposing (Posix)
import Utdanning.Skjema as Skjema exposing (Felt(..), UtdanningSkjema, ValidertUtdanningSkjema)
import Utdanning.Utdanning as Utdanning exposing (Nivå(..), Utdanning)
import Validering



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , utdanningListe : List Utdanning
    , debugStatus : DebugStatus
    , sistLagretFraForrigeSeksjon : Posix
    , harFullførtEnSamtale : Bool
    }


type AvsluttetGrunn
    = AvbruttPåbegynt
    | SlettetPåbegynt
    | EndretEksisterende
    | FullførtRegistrering


type OppsummeringsType
    = FørsteGang
    | EtterEndring
    | AvbrøtSletting
    | NyUtdanning


type SkjemaType
    = OppsummeringsSkjema
    | NyUtdanningSkjema


type NivåValg
    = AlleNivåer
    | GrunnskoleVideregående


type Samtale
    = SpoerBrukerOmVitnemaalsportalen (List Utdanning)
    | Intro (List Utdanning)
    | BekreftHarIkkeUtdanning
    | VelgEnUtdanningÅRedigere
    | RegistrerNivå NivåValg
    | RegistrerSkole SkoleInfo
    | RegistrerRetning RetningInfo
    | RegistrerBeskrivelse Bool BeskrivelseInfo
    | RegistrereFraDato FraDatoInfo
    | RegistrereNåværende NåværendeInfo
    | RegistrereTilDato TilDatoInfo
    | Oppsummering OppsummeringsType ValidertUtdanningSkjema
    | EndrerSkjema SkjemaType UtdanningSkjema
    | BekreftSlettingAvPåbegynt ValidertUtdanningSkjema
    | LagrerSkjema ValidertUtdanningSkjema LagreStatus
    | LagringFeilet Http.Error ValidertUtdanningSkjema
    | LeggTilFlereUtdanninger (List Utdanning) AvsluttetGrunn
    | BekreftAvbrytingAvRegistreringen Samtale
    | VenterPåAnimasjonFørFullføring (List Utdanning) AvsluttetGrunn


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Posix (List Utdanning) FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


sistLagret : Model -> Posix
sistLagret (Model model) =
    let
        sistLagretListe =
            List.map (\x -> Time.posixToMillis (Utdanning.sistEndretDato x)) model.utdanningListe
    in
    case List.maximum sistLagretListe of
        Just value ->
            model.sistLagretFraForrigeSeksjon
                |> nyestePosix (Time.millisToPosix value)

        Nothing ->
            model.sistLagretFraForrigeSeksjon


type alias SkoleInfo =
    { nivå : Nivå, skole : String }


type alias RetningInfo =
    { forrige : SkoleInfo, retning : String }


type alias BeskrivelseInfo =
    { forrige : RetningInfo, beskrivelse : String }


type alias FraDatoInfo =
    { forrige : BeskrivelseInfo
    , fraMåned : Måned
    , fraÅr : String
    , visÅrFeilmelding : Bool
    }


type alias NåværendeInfo =
    { forrige : FraDatoInfo
    , fraÅr : År
    }


type alias TilDatoInfo =
    { forrige : NåværendeInfo
    , tilÅr : String
    , tilMåned : Måned
    , visÅrFeilmelding : Bool
    }


maxLengthBeskrivelse =
    2000


maxLengthSkole =
    250


maxLengthStudieretning =
    250


forrigeTilRetningInfo : SkoleInfo -> RetningInfo
forrigeTilRetningInfo skole =
    { forrige = skole, retning = "" }


nivåTilSkoleInfo : Nivå -> SkoleInfo
nivåTilSkoleInfo nivå =
    { nivå = nivå, skole = "" }


forrigeTilBeskrivelseInfo : RetningInfo -> BeskrivelseInfo
forrigeTilBeskrivelseInfo retning =
    { forrige = retning, beskrivelse = "" }


forrigeTilFraDatoInfo : BeskrivelseInfo -> FraDatoInfo
forrigeTilFraDatoInfo beskrivelse =
    { forrige = beskrivelse
    , fraMåned = Januar
    , fraÅr = ""
    , visÅrFeilmelding = False
    }


fraDatoTilNåværende : FraDatoInfo -> År -> NåværendeInfo
fraDatoTilNåværende info år =
    { forrige = info
    , fraÅr = år
    }


forrigeTilTilDatoInfo : NåværendeInfo -> TilDatoInfo
forrigeTilTilDatoInfo nåværendeInfo =
    { forrige = nåværendeInfo
    , tilMåned = Januar
    , tilÅr = ""
    , visÅrFeilmelding = False
    }


nåværendeTilSkjema : NåværendeInfo -> ValidertUtdanningSkjema
nåværendeTilSkjema nåværendeInfo =
    Skjema.initValidertSkjema
        { nivå = nåværendeInfo.forrige.forrige.forrige.forrige.nivå
        , studiested = nåværendeInfo.forrige.forrige.forrige.forrige.skole
        , utdanningsretning = nåværendeInfo.forrige.forrige.forrige.retning
        , beskrivelse = nåværendeInfo.forrige.forrige.beskrivelse
        , fraMåned = nåværendeInfo.forrige.fraMåned
        , fraÅr = nåværendeInfo.fraÅr
        , tilDato = Nåværende
        , id = Nothing
        }


tilDatoInfoTilSkjema : TilDatoInfo -> År -> ValidertUtdanningSkjema
tilDatoInfoTilSkjema info år =
    Skjema.initValidertSkjema
        { nivå = info.forrige.forrige.forrige.forrige.forrige.nivå
        , studiested = info.forrige.forrige.forrige.forrige.forrige.skole
        , utdanningsretning = info.forrige.forrige.forrige.forrige.retning
        , beskrivelse = info.forrige.forrige.forrige.beskrivelse
        , fraMåned = info.forrige.forrige.fraMåned
        , fraÅr = info.forrige.fraÅr
        , tilDato = Avsluttet info.tilMåned år
        , id = Nothing
        }



--- UPDATE ---


type Msg
    = BrukerVilSeVitnemaalsportalen (List Utdanning)
    | BrukerVilIkkeSeVitnemaalsportalen (List Utdanning)
    | BrukerVilRegistrereUtdanning
    | SvarerNeiTilUtdanning
    | BrukerVilRedigereUtdanning
    | BrukerHarValgtUtdanningÅRedigere Utdanning
    | GåTilArbeidserfaring AvsluttetGrunn
    | BrukerVilRegistrereNivå Nivå
    | OppdaterSkole String
    | BrukerVilRegistrereSkole
    | OppdaterRetning String
    | BrukerVilRegistrereRetning
    | OppdaterBeskrivelse String
    | VilSeEksempel
    | BrukerVilRegistrereBeskrivelse
    | OppdaterFraMåned String
    | OppdaterFraÅr String
    | BrukerVilGåVidereMedFraDato
    | BrukerSvarerJaTilNaavarende
    | BrukerSvarerNeiTilNaavarende
    | OppdaterTilMåned String
    | OppdaterTilÅr String
    | BrukerVilRegistrereTilDato
    | BrukerVilEndreOppsummering
    | VilSlettePåbegynt
    | BekrefterSlettPåbegynt
    | AngrerSlettPåbegynt
    | OppsummeringBekreftet
    | OppsummeringEndret SkjemaEndring
    | OppsummeringSkjemaLagreknappTrykket
    | UtdanningSendtTilApi (Result Http.Error (List Utdanning))
    | BrukerVilPrøveÅLagrePåNytt
    | BrukerVilAvbryteLagringen
    | BrukerVilAvbryteRegistreringen
    | BrukerBekrefterAvbrytingAvRegistrering
    | BrukerVilIkkeAvbryteRegistreringen
    | VilRegistrereFlereUtdanninger
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())
    | FeltMisterFokus
    | TimeoutEtterAtFeltMistetFokus
    | ErrorLogget


type SkjemaEndring
    = Tekst Felt String
    | NåværendeToggled
    | FraMåned String
    | TilMåned String
    | Nivå String
    | FraÅrBlurred
    | TilÅrBlurred


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        BrukerVilSeVitnemaalsportalen utdanninger ->
            IkkeFerdig
                ( Intro utdanninger
                    |> oppdaterSamtale model (SvarFraMsg msg)
                , lagtTilSpørsmålCmd model.debugStatus
                )

        BrukerVilIkkeSeVitnemaalsportalen utdanninger ->
            IkkeFerdig
                ( Intro utdanninger
                    |> oppdaterSamtale model (SvarFraMsg msg)
                , lagtTilSpørsmålCmd model.debugStatus
                )

        BrukerVilRegistrereUtdanning ->
            case model.aktivSamtale of
                BekreftHarIkkeUtdanning ->
                    IkkeFerdig
                        ( RegistrerNivå GrunnskoleVideregående
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

                _ ->
                    IkkeFerdig
                        ( RegistrerNivå AlleNivåer
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

        SvarerNeiTilUtdanning ->
            IkkeFerdig
                ( BekreftHarIkkeUtdanning
                    |> oppdaterSamtale model (SvarFraMsg msg)
                , lagtTilSpørsmålCmd model.debugStatus
                )

        BrukerVilRedigereUtdanning ->
            case model.utdanningListe of
                enesteUtdanning :: [] ->
                    ( enesteUtdanning
                        |> Skjema.fraUtdanning
                        |> EndrerSkjema OppsummeringsSkjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                [] ->
                    IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    ( VelgEnUtdanningÅRedigere
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BrukerHarValgtUtdanningÅRedigere utdanning ->
            ( utdanning
                |> Skjema.fraUtdanning
                |> EndrerSkjema OppsummeringsSkjema
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        GåTilArbeidserfaring avsluttetGrunn ->
            ( VenterPåAnimasjonFørFullføring model.utdanningListe avsluttetGrunn
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilRegistrereNivå nivå ->
            IkkeFerdig
                ( nivå
                    |> nivåTilSkoleInfo
                    |> RegistrerSkole
                    |> oppdaterSamtale model (SvarFraMsg msg)
                , lagtTilSpørsmålCmd model.debugStatus
                )

        OppdaterSkole skole ->
            case model.aktivSamtale of
                RegistrerSkole skoleinfo ->
                    IkkeFerdig
                        ( { skoleinfo | skole = skole }
                            |> RegistrerSkole
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereSkole ->
            case model.aktivSamtale of
                RegistrerSkole skoleinfo ->
                    case Validering.feilmeldingMaxAntallTegn skoleinfo.skole maxLengthSkole of
                        Nothing ->
                            IkkeFerdig
                                ( skoleinfo
                                    |> forrigeTilRetningInfo
                                    |> RegistrerRetning
                                    |> oppdaterSamtale model (SvarFraMsg msg)
                                , lagtTilSpørsmålCmd model.debugStatus
                                )

                        Just _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterRetning retning ->
            case model.aktivSamtale of
                RegistrerRetning retningsinfo ->
                    IkkeFerdig
                        ( { retningsinfo | retning = retning }
                            |> RegistrerRetning
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereRetning ->
            case model.aktivSamtale of
                RegistrerRetning retninginfo ->
                    case Validering.feilmeldingMaxAntallTegn retninginfo.retning maxLengthStudieretning of
                        Nothing ->
                            IkkeFerdig
                                ( retninginfo
                                    |> forrigeTilBeskrivelseInfo
                                    |> RegistrerBeskrivelse (detFinnesEksemplerForNivå retninginfo.forrige.nivå)
                                    |> oppdaterSamtale model (SvarFraMsg msg)
                                , lagtTilSpørsmålCmd model.debugStatus
                                )

                        Just _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilSeEksempel ->
            case model.aktivSamtale of
                RegistrerBeskrivelse _ beskrivelseinfo ->
                    let
                        oppdatertMeldingslogg =
                            model.seksjonsMeldingsLogg
                                |> MeldingsLogg.leggTilSvar (svarFraBrukerInput model msg)
                                |> MeldingsLogg.leggTilSpørsmål (eksemplerPåUtdanning beskrivelseinfo.forrige.forrige.nivå)
                    in
                    IkkeFerdig
                        ( beskrivelseinfo
                            |> RegistrerBeskrivelse False
                            |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg } IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterBeskrivelse beskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse medEksempelKnapp beskrivelseinfo ->
                    IkkeFerdig
                        ( { beskrivelseinfo | beskrivelse = beskrivelse }
                            |> RegistrerBeskrivelse medEksempelKnapp
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereBeskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse _ beskrivelseinfo ->
                    case Validering.feilmeldingMaxAntallTegn beskrivelseinfo.beskrivelse maxLengthBeskrivelse of
                        Nothing ->
                            IkkeFerdig
                                ( { beskrivelseinfo | beskrivelse = String.trim beskrivelseinfo.beskrivelse }
                                    |> forrigeTilFraDatoInfo
                                    |> RegistrereFraDato
                                    |> oppdaterSamtale model (SvarFraMsg msg)
                                , lagtTilSpørsmålCmd model.debugStatus
                                )

                        Just _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterFraMåned string ->
            case model.aktivSamtale of
                RegistrereFraDato fraDatoInfo ->
                    ( { fraDatoInfo | fraMåned = Måned.stringTilMåned string }
                        |> RegistrereFraDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterFraÅr string ->
            case model.aktivSamtale of
                RegistrereFraDato fraDatoInfo ->
                    ( { fraDatoInfo | fraÅr = string }
                        |> RegistrereFraDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilGåVidereMedFraDato ->
            case model.aktivSamtale of
                RegistrereFraDato fraDatoInfo ->
                    case Dato.stringTilÅr fraDatoInfo.fraÅr of
                        Just fraÅr ->
                            ( fraÅr
                                |> fraDatoTilNåværende fraDatoInfo
                                |> RegistrereNåværende
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            IkkeFerdig
                                ( { fraDatoInfo | visÅrFeilmelding = True }
                                    |> RegistrereFraDato
                                    |> oppdaterSamtale model IngenNyeMeldinger
                                , Cmd.none
                                )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerSvarerJaTilNaavarende ->
            case model.aktivSamtale of
                RegistrereNåværende nåværendeInfo ->
                    ( nåværendeInfo
                        |> nåværendeTilSkjema
                        |> Oppsummering FørsteGang
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerSvarerNeiTilNaavarende ->
            case model.aktivSamtale of
                RegistrereNåværende nåværendeInfo ->
                    ( nåværendeInfo
                        |> forrigeTilTilDatoInfo
                        |> RegistrereTilDato
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterTilMåned string ->
            case model.aktivSamtale of
                RegistrereTilDato tilDatoInfo ->
                    ( { tilDatoInfo | tilMåned = Måned.stringTilMåned string }
                        |> RegistrereTilDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        OppdaterTilÅr string ->
            case model.aktivSamtale of
                RegistrereTilDato tilDatoInfo ->
                    ( { tilDatoInfo | tilÅr = string }
                        |> RegistrereTilDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilRegistrereTilDato ->
            case model.aktivSamtale of
                RegistrereTilDato tilDatoInfo ->
                    case Dato.stringTilÅr tilDatoInfo.tilÅr of
                        Just tilÅr ->
                            ( tilÅr
                                |> tilDatoInfoTilSkjema tilDatoInfo
                                |> Oppsummering FørsteGang
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { tilDatoInfo | visÅrFeilmelding = True }
                                |> RegistrereTilDato
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilEndreOppsummering ->
            case model.aktivSamtale of
                Oppsummering _ utdanningskjema ->
                    updateEtterVilEndreSkjema model msg utdanningskjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilSlettePåbegynt ->
            case model.aktivSamtale of
                Oppsummering _ skjema ->
                    ( BekreftSlettingAvPåbegynt skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BekrefterSlettPåbegynt ->
            case model.aktivSamtale of
                BekreftSlettingAvPåbegynt _ ->
                    ( SlettetPåbegynt
                        |> LeggTilFlereUtdanninger model.utdanningListe
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        AngrerSlettPåbegynt ->
            case model.aktivSamtale of
                BekreftSlettingAvPåbegynt skjema ->
                    ( Oppsummering AvbrøtSletting skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppsummeringBekreftet ->
            case model.aktivSamtale of
                Oppsummering _ ferdigskjema ->
                    updateEtterLagreKnappTrykket model msg ferdigskjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppsummeringSkjemaLagreknappTrykket ->
            case model.aktivSamtale of
                EndrerSkjema skjemaType skjema ->
                    case Skjema.validerSkjema skjema of
                        Just validertSkjema ->
                            let
                                oppsummeringstype =
                                    case skjemaType of
                                        NyUtdanningSkjema ->
                                            NyUtdanning

                                        _ ->
                                            EtterEndring
                            in
                            ( validertSkjema
                                |> Oppsummering oppsummeringstype
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar (validertSkjemaTilSetninger validertSkjema)))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            IkkeFerdig
                                ( skjema
                                    |> Skjema.gjørAlleFeilmeldingerSynlig
                                    |> EndrerSkjema skjemaType
                                    |> oppdaterSamtale model IngenNyeMeldinger
                                , Cmd.none
                                )

                LagringFeilet error feiletskjema ->
                    IkkeFerdig
                        ( error
                            |> LagreStatus.fraError
                            |> LagrerSkjema feiletskjema
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , Cmd.batch
                            [ lagreUtdanning UtdanningSendtTilApi feiletskjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        UtdanningSendtTilApi result ->
            case model.aktivSamtale of
                LagrerSkjema skjema lagreStatus ->
                    case result of
                        Ok nyUtdanningsListe ->
                            let
                                avsluttetGrunn =
                                    if List.length model.utdanningListe == List.length nyUtdanningsListe then
                                        EndretEksisterende

                                    else
                                        FullførtRegistrering

                                nyModel =
                                    if List.length model.utdanningListe == List.length nyUtdanningsListe then
                                        { model | utdanningListe = nyUtdanningsListe }

                                    else
                                        { model | utdanningListe = nyUtdanningsListe, harFullførtEnSamtale = True }
                            in
                            ( if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                avsluttetGrunn
                                    |> LeggTilFlereUtdanninger nyUtdanningsListe
                                    |> oppdaterSamtale nyModel (ManueltSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ]))

                              else
                                avsluttetGrunn
                                    |> LeggTilFlereUtdanninger nyUtdanningsListe
                                    |> oppdaterSamtale nyModel UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , lagreUtdanning UtdanningSendtTilApi skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre utdanning" error
                                    )
                                        |> IkkeFerdig

                            else
                                ( skjema
                                    |> LagringFeilet error
                                    |> oppdaterSamtale model UtenSvar
                                , Cmd.batch
                                    [ lagtTilSpørsmålCmd model.debugStatus
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre utdanning" error
                                    ]
                                )
                                    |> IkkeFerdig

                Oppsummering _ skjema ->
                    case result of
                        Ok nyUtdanningsListe ->
                            let
                                avsluttetGrunn =
                                    if List.length model.utdanningListe == List.length nyUtdanningsListe then
                                        EndretEksisterende

                                    else
                                        FullførtRegistrering

                                nyModel =
                                    if List.length model.utdanningListe == List.length nyUtdanningsListe then
                                        { model | utdanningListe = nyUtdanningsListe }

                                    else
                                        { model | utdanningListe = nyUtdanningsListe, harFullførtEnSamtale = True }
                            in
                            ( avsluttetGrunn
                                |> LeggTilFlereUtdanninger nyUtdanningsListe
                                |> oppdaterSamtale nyModel UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( skjema
                                |> LagringFeilet error
                                |> oppdaterSamtale model UtenSvar
                            , skjema
                                |> Skjema.encode
                                |> Api.logErrorWithRequestBody ErrorLogget "Lagre utdanning" error
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilPrøveÅLagrePåNytt ->
            case model.aktivSamtale of
                LagringFeilet error validertSkjema ->
                    IkkeFerdig
                        ( error
                            |> LagreStatus.fraError
                            |> LagrerSkjema validertSkjema
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , Cmd.batch
                            [ lagreUtdanning UtdanningSendtTilApi validertSkjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilAvbryteLagringen ->
            ( model.utdanningListe
                |> Intro
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilAvbryteRegistreringen ->
            case model.aktivSamtale of
                RegistrerNivå _ ->
                    avbrytRegistrering model msg

                RegistrerSkole _ ->
                    avbrytRegistrering model msg

                _ ->
                    ( model.aktivSamtale
                        |> BekreftAvbrytingAvRegistreringen
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BrukerBekrefterAvbrytingAvRegistrering ->
            avbrytRegistrering model msg

        BrukerVilIkkeAvbryteRegistreringen ->
            case model.aktivSamtale of
                BekreftAvbrytingAvRegistreringen samtaleStegFørAvbryting ->
                    ( Model
                        { model
                            | aktivSamtale = samtaleStegFørAvbryting
                            , seksjonsMeldingsLogg =
                                model.seksjonsMeldingsLogg
                                    |> MeldingsLogg.leggTilSvar (svarFraBrukerInput model msg)
                                    |> MeldingsLogg.leggTilSpørsmål
                                        (List.concat
                                            [ [ Melding.spørsmål [ "Ok. Da fortsetter vi der vi slapp." ] ]
                                            , samtaleTilMeldingsLogg samtaleStegFørAvbryting
                                            ]
                                        )
                        }
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereFlereUtdanninger ->
            ( Skjema.init
                |> EndrerSkjema NyUtdanningSkjema
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case model.aktivSamtale of
                        LagringFeilet error validertSkjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSkjema validertSkjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , lagreUtdanning UtdanningSendtTilApi validertSkjema
                                    )

                            else
                                IkkeFerdig ( Model model, Cmd.none )

                        LagrerSkjema skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerSkjema skjema
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

        OppsummeringEndret skjemaEndring ->
            case model.aktivSamtale of
                EndrerSkjema skjemaType skjema ->
                    ( skjema
                        |> oppdaterSkjema skjemaEndring
                        |> EndrerSkjema skjemaType
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )

        FokusSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )

        FeltMisterFokus ->
            IkkeFerdig ( Model model, mistetFokusCmd )

        TimeoutEtterAtFeltMistetFokus ->
            case model.aktivSamtale of
                RegistrereFraDato fraDatoInfo ->
                    ( { fraDatoInfo | visÅrFeilmelding = True }
                        |> RegistrereFraDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrereTilDato tilDatoInfo ->
                    ( { tilDatoInfo | visÅrFeilmelding = True }
                        |> RegistrereTilDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )


avbrytRegistrering : ModelInfo -> Msg -> SamtaleStatus
avbrytRegistrering model msg =
    ( AvbruttPåbegynt
        |> LeggTilFlereUtdanninger model.utdanningListe
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


nivåToString : Nivå -> String
nivåToString nivå =
    case nivå of
        Grunnskole ->
            "Grunnskole"

        VideregåendeYrkesskole ->
            "Videregående/Yrkesskole"

        Fagskole ->
            "Fagskole"

        Folkehøyskole ->
            "Folkehøyskole"

        HøyereUtdanning1til4 ->
            "Høyere utdanning, 1-4 år"

        HøyereUtdanning4pluss ->
            "Høyere utdanning, mer enn 4 år"

        Doktorgrad ->
            "Doktorgrad"


oppdaterSkjema : SkjemaEndring -> UtdanningSkjema -> UtdanningSkjema
oppdaterSkjema skjemaEndring skjema =
    case skjemaEndring of
        Tekst felt innhold ->
            Skjema.oppdaterTekstFelt felt innhold skjema

        NåværendeToggled ->
            Skjema.toggleNavarende skjema

        FraMåned månedString ->
            månedString
                |> Måned.stringTilMåned
                |> Skjema.oppdaterFraMåned skjema

        TilMåned månedString ->
            månedString
                |> Måned.stringTilMåned
                |> Skjema.oppdaterTilMåned skjema

        Nivå nivåString ->
            nivåString
                |> stringToNivå
                |> Maybe.map (Skjema.oppdaterNivå skjema)
                |> Maybe.withDefault skjema

        FraÅrBlurred ->
            Skjema.gjørFeilmeldingFraÅrSynlig skjema

        TilÅrBlurred ->
            Skjema.gjørFeilmeldingTilÅrSynlig skjema


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        SpoerBrukerOmVitnemaalsportalen _ ->
            settFokusCmd VilSeVitnemaalsportalenId

        Intro _ ->
            settFokusCmd HarUtdanningId

        BekreftHarIkkeUtdanning ->
            settFokusCmd BekreftHarIkkeUtdanningId

        RegistrerNivå _ ->
            settFokusCmd VelgNivåInput

        RegistrerSkole _ ->
            settFokusCmd RegistrerSkoleInput

        RegistrerRetning _ ->
            settFokusCmd RegistrerRetningInput

        RegistrerBeskrivelse _ _ ->
            settFokusCmd RegistrerBeskrivelseInput

        RegistrereFraDato _ ->
            settFokusCmd FraMånedId

        RegistrereNåværende _ ->
            settFokusCmd NåværendeId

        RegistrereTilDato _ ->
            settFokusCmd TilMånedId

        EndrerSkjema _ _ ->
            settFokusCmd VelgNivåISkjemaId

        Oppsummering _ _ ->
            settFokusCmd BekreftOppsummeringId

        LeggTilFlereUtdanninger _ _ ->
            settFokusCmd LeggTilUtdanningId

        VelgEnUtdanningÅRedigere ->
            settFokusCmd RedigerUtdanningId

        LagringFeilet _ _ ->
            settFokusCmd LagringFeiletActionId

        BekreftSlettingAvPåbegynt _ ->
            settFokusCmd SlettePåbegyntId

        BekreftAvbrytingAvRegistreringen _ ->
            settFokusCmd AvbrytSlettingId

        _ ->
            Cmd.none


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding model ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring utdanningsListe _ ->
                    Ferdig (sistLagret (Model model)) utdanningsListe ferdigAnimertSamtale

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


updateEtterVilEndreSkjema : ModelInfo -> Msg -> ValidertUtdanningSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model msg skjema =
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> EndrerSkjema OppsummeringsSkjema
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> Msg -> ValidertUtdanningSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model msg skjema =
    ( LagreStatus.init
        |> LagrerSkjema skjema
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagreUtdanning UtdanningSendtTilApi skjema
    )
        |> IkkeFerdig


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


mistetFokusCmd : Cmd Msg
mistetFokusCmd =
    Process.sleep 100
        |> Task.perform (\_ -> TimeoutEtterAtFeltMistetFokus)


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
        |> Task.attempt FokusSatt


stringToNivå : String -> Maybe Nivå
stringToNivå string =
    case string of
        "Grunnskole" ->
            Just Grunnskole

        "VideregåendeYrkesskole" ->
            Just VideregåendeYrkesskole

        "Fagskole" ->
            Just Fagskole

        "Folkehøyskole" ->
            Just Folkehøyskole

        "HøyereUtdanning1til4" ->
            Just HøyereUtdanning1til4

        "HøyereUtdanning4pluss" ->
            Just HøyereUtdanning4pluss

        "Doktorgrad" ->
            Just Doktorgrad

        _ ->
            Nothing


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


utdanningslisteTilString : List Utdanning -> List Tekstområde
utdanningslisteTilString utdanninger =
    utdanninger
        |> List.map utdanningTilTekstområde
        |> List.intersperse (Avsnitt Melding.tomLinje)


utdanningTilTekstområde : Utdanning -> Tekstområde
utdanningTilTekstområde utdanning =
    Seksjon (Utdanning.utdanningsretning utdanning |> Maybe.withDefault "")
        [ Dato.periodeTilString (Utdanning.fraMåned utdanning) (Utdanning.fraÅr utdanning) (Utdanning.tilDato utdanning)
        , (Utdanning.utdanningsretning utdanning |> Maybe.withDefault "")
            ++ " ved  "
            ++ (Utdanning.studiested utdanning |> Maybe.withDefault "")
        ]


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg utdanningSeksjon =
    case utdanningSeksjon of
        SpoerBrukerOmVitnemaalsportalen _ ->
            [ Melding.spørsmål [ "Lurer du på hvilken fullførte utdanning du har, kan du se dette på Vitnemålsportalen." ]
            , Melding.spørsmål [ "Vil du se din fullførte utdanning?" ]
            ]

        Intro utdanninger ->
            if List.isEmpty utdanninger then
                [ Melding.spørsmål [ "Har du utdanning du vil legge inn i CV-en?" ]
                , Melding.spørsmål [ "Legg inn videregående og høyere utdanning hvis du har det. Hvis du kun har grunnskole, legger du inn det." ]
                ]

            else
                [ Melding.spørsmål [ "Nå skal vi legge til utdanning. Vi ser at du allerede har lagt inn disse utdanningene: " ]
                , Melding.spørsmålMedTekstområder (utdanningslisteTilString utdanninger)
                , Melding.spørsmål [ "Vil du legge inn flere utdanninger? " ]
                ]

        BekreftHarIkkeUtdanning ->
            [ Melding.spørsmål [ "Er du sikker på at du ikke har utdanning? Du kan også legge inn grunnskole og videregående. Vil du legge inn det?" ]
            ]

        VelgEnUtdanningÅRedigere ->
            [ Melding.spørsmål [ "Hvilken registrerte utdanning ønsker du å redigere?" ] ]

        RegistrerNivå nivåValg ->
            [ case nivåValg of
                AlleNivåer ->
                    [ Melding.spørsmål [ "Legg inn én utdanning av gangen." ]
                    , Melding.spørsmål [ "Hvis du har en bachelorgrad, velg høyere utdanning 1-4 år. Har du en mastergrad, velg høyere utdanning mer enn 4 år." ]
                    ]

                GrunnskoleVideregående ->
                    []
            , [ Melding.spørsmål [ "Hvilket nivå har utdanningen du skal legge inn?" ]
              ]
            ]
                |> List.concat

        RegistrerSkole skoleinfo ->
            case skoleinfo.nivå of
                Fagskole ->
                    [ Melding.spørsmål [ "Hvilken skole gikk du på?" ]
                    , Melding.spørsmål [ "For eksempel Fagskolen i Østfold" ]
                    ]

                VideregåendeYrkesskole ->
                    [ Melding.spørsmål [ "Hvilken skole gikk du på?" ]
                    , Melding.spørsmål [ "For eksempel Drammen videregående skole" ]
                    ]

                Grunnskole ->
                    [ Melding.spørsmål [ "Hvilken grunnskole gikk du på?" ]
                    , Melding.spørsmål [ "For eksempel Huseby skole" ]
                    ]

                Folkehøyskole ->
                    [ Melding.spørsmål [ "Hvilken folkehøgskole gikk du på?" ]
                    , Melding.spørsmål [ "For eksempel Nordfjordeid folkehøgskule" ]
                    ]

                _ ->
                    [ Melding.spørsmål [ "Hvilken skole gikk du på?" ]
                    , Melding.spørsmål [ "For eksempel Universitet i Oslo" ]
                    ]

        RegistrerRetning _ ->
            [ Melding.spørsmål [ "Hvis du har fagbrev/svennebrev, mesterbrev eller autorisasjon, kan du legge inn dette senere." ]
            , Melding.spørsmål [ "Hva er navnet på graden din, og hvilken utdanningsretning gikk du?" ]
            , Melding.spørsmål [ "Kanskje du har en bachelor i historie, eller elektrofag fra videregående?" ]
            ]

        RegistrerBeskrivelse _ _ ->
            [ Melding.spørsmål [ "Skriv noen ord om denne utdanningen. Har du fordypning i noen fag?" ] ]

        RegistrereFraDato _ ->
            [ Melding.spørsmål [ "Når begynte du på utdanningen din?" ]
            , Melding.spørsmål [ "De fleste studiene i Norge begynner i august." ]
            ]

        RegistrereNåværende _ ->
            [ Melding.spørsmål [ "Holder du fortsatt på med utdanningen?" ] ]

        RegistrereTilDato _ ->
            [ Melding.spørsmål [ "Når fullførte du utdanningen din?" ]
            , Melding.spørsmål [ "De fleste studier i Norge er ferdig i juni." ]
            ]

        Oppsummering oppsummeringsType validertSkjema ->
            case oppsummeringsType of
                AvbrøtSletting ->
                    [ Melding.spørsmål [ "Ok, da lar jeg utdanningen stå." ]
                    , oppsummeringsSpørsmål validertSkjema
                    ]

                EtterEndring ->
                    [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

                FørsteGang ->
                    [ oppsummeringsSpørsmål validertSkjema
                    ]

                NyUtdanning ->
                    [ Melding.spørsmål [ "Du har lagt til en utdanning. Er informasjonen riktig?" ] ]

        EndrerSkjema skjemaType _ ->
            case skjemaType of
                OppsummeringsSkjema ->
                    [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

                _ ->
                    [ Melding.spørsmål [ "Legg inn utdanningen din under." ] ]

        BekreftSlettingAvPåbegynt _ ->
            [ Melding.spørsmål [ "Er du sikker på at du vil slette denne utdanningen?" ] ]

        LeggTilFlereUtdanninger utdanninger avsluttetGrunn ->
            [ case avsluttetGrunn of
                AvbruttPåbegynt ->
                    Melding.spørsmål [ "Nå har jeg avbrutt." ]

                SlettetPåbegynt ->
                    Melding.spørsmål [ "Nå har jeg slettet utdanningen." ]

                EndretEksisterende ->
                    Melding.spørsmål [ "Så bra! Nå er utdanningen endret👍" ]

                FullførtRegistrering ->
                    Melding.spørsmål [ "Så bra! Nå er utdanningen lagret👍" ]
            , if List.isEmpty utdanninger then
                Melding.spørsmål [ "Har du utdanning du vil legge inn i CV-en?" ]

              else
                Melding.spørsmål [ "Vil du legge inn flere utdanninger?" ]
            ]

        BekreftAvbrytingAvRegistreringen _ ->
            [ Melding.spørsmål [ "Hvis du avbryter, blir ikke denne utdanningen lagret på CV-en din. Er du sikker på at du vil avbryte?" ] ]

        LagrerSkjema _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre utdanning" } ]

        VenterPåAnimasjonFørFullføring liste avsluttetGrunn ->
            if List.isEmpty liste then
                if avsluttetGrunn == SlettetPåbegynt then
                    [ Melding.spørsmål [ "Da går vi videre til arbeidserfaring." ] ]

                else
                    [ Melding.spørsmål [ "Ok 😊 Da går vi videre til arbeidserfaring." ] ]

            else
                [ Melding.spørsmål [ "Bra jobba! Da går vi videre." ] ]


validertSkjemaTilSetninger : ValidertUtdanningSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    let
        utdanningsskjema =
            Skjema.tilUvalidertSkjema validertSkjema
    in
    [ Dato.periodeTilString (Skjema.fraMåned utdanningsskjema) (Skjema.fraÅrValidert validertSkjema) (Skjema.tilDatoValidert validertSkjema)
    , Melding.tomLinje
    , "Utdanningsnivå: " ++ nivåToString (Skjema.nivå utdanningsskjema)
    , "Grad og studieretning: " ++ Skjema.innholdTekstFelt Utdanningsretning utdanningsskjema
    , "Skole/studiested: " ++ Skjema.innholdTekstFelt Studiested utdanningsskjema
    , Melding.tomLinje
    , "Beskrivelse:"
    , Skjema.innholdTekstFelt Beskrivelse utdanningsskjema
    ]


oppsummeringsSpørsmål : ValidertUtdanningSkjema -> Melding
oppsummeringsSpørsmål skjema =
    [ [ "Du har lagt inn dette:"
      , Melding.tomLinje
      ]
    , validertSkjemaTilSetninger skjema
    , [ Melding.tomLinje
      , "Er informasjonen riktig?"
      ]
    ]
        |> List.concat
        |> Melding.spørsmål


detFinnesEksemplerForNivå : Nivå -> Bool
detFinnesEksemplerForNivå nivå =
    nivå
        |> eksemplerPåUtdanning
        |> (not << List.isEmpty)


eksemplerPåUtdanning : Nivå -> List Melding
eksemplerPåUtdanning nivå =
    case nivå of
        Grunnskole ->
            []

        VideregåendeYrkesskole ->
            [ Melding.eksempelMedTittel "Eksempel 1:" [ "Fordypning i matematikk og fysikk." ]
            , Melding.eksempelMedTittel "Eksempel 2:" [ "Elektrofag Vg1 og Vg2, spesialisering i datateknologi og elektronikk." ]
            ]

        Fagskole ->
            [ Melding.eksempel [ "Maskinteknikk i mekanisk industri, prosjekt- og kvalitetsledelse og økonomistyring." ]
            ]

        Folkehøyskole ->
            []

        HøyereUtdanning1til4 ->
            [ Melding.eksempel [ "Fordypning i offentlig politikk og administrasjon. Bacheloroppgave om ulik politisk utvikling i de skandinaviske landene etter 1970." ]
            ]

        HøyereUtdanning4pluss ->
            [ Melding.eksempel [ "Spesialisering i anvendt finans. Utvekslingsstudent på University of London (høstsemesteret 2017)." ]
            ]

        Doktorgrad ->
            []



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput model =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            SpoerBrukerOmVitnemaalsportalen utdanninger ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp (BrukerVilSeVitnemaalsportalen utdanninger) "Ja, se min fullførte utdanning"
                        |> Knapp.withLink "https://www.vitnemalsportalen.no/"
                        |> Knapp.withId (inputIdTilString VilSeVitnemaalsportalenId)
                    , Knapp.knapp (BrukerVilIkkeSeVitnemaalsportalen utdanninger) "Nei, ikke nå"
                    ]

            Intro _ ->
                if List.isEmpty model.utdanningListe then
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp BrukerVilRegistrereUtdanning "Ja, jeg har utdanning"
                            |> Knapp.withId (inputIdTilString HarUtdanningId)
                        , Knapp.knapp SvarerNeiTilUtdanning "Nei, jeg har ikke utdanning"
                        ]

                else
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp BrukerVilRegistrereUtdanning "Ja, legg til en utdanning"
                            |> Knapp.withId (inputIdTilString HarUtdanningId)
                        , Knapp.knapp (GåTilArbeidserfaring FullførtRegistrering) "Nei, jeg er ferdig"
                        , Knapp.knapp BrukerVilRedigereUtdanning "Nei, jeg vil endre det jeg har lagt inn"
                        ]

            BekreftHarIkkeUtdanning ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerVilRegistrereUtdanning "Ja, det vil jeg"
                        |> Knapp.withId (inputIdTilString BekreftHarIkkeUtdanningId)
                    , Knapp.knapp (GåTilArbeidserfaring FullførtRegistrering) "Nei, det vil jeg ikke"
                    ]

            VelgEnUtdanningÅRedigere ->
                BrukerInput.knapper Kolonne
                    (case model.utdanningListe of
                        first :: rest ->
                            (utdanningKnapp first
                                |> Knapp.withId (inputIdTilString RedigerUtdanningId)
                            )
                                :: List.map utdanningKnapp rest

                        _ ->
                            []
                    )

            RegistrerNivå nivåValg ->
                BrukerInput.knapper Kolonne
                    (case nivåValg of
                        AlleNivåer ->
                            [ velgNivåKnapp Grunnskole
                                |> Knapp.withId (inputIdTilString VelgNivåInput)
                            , velgNivåKnapp VideregåendeYrkesskole
                            , velgNivåKnapp Fagskole
                            , velgNivåKnapp Folkehøyskole
                            , velgNivåKnapp HøyereUtdanning1til4
                            , velgNivåKnapp HøyereUtdanning4pluss
                            , velgNivåKnapp Doktorgrad
                            , Knapp.knapp BrukerVilAvbryteRegistreringen "Avbryt"
                                |> Knapp.withType Flat
                            ]

                        GrunnskoleVideregående ->
                            [ velgNivåKnapp Grunnskole
                                |> Knapp.withId (inputIdTilString VelgNivåInput)
                            , velgNivåKnapp VideregåendeYrkesskole
                            , Knapp.knapp BrukerVilAvbryteRegistreringen "Avbryt"
                                |> Knapp.withType Flat
                            ]
                    )

            RegistrerSkole skoleinfo ->
                BrukerInput.inputMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilRegistrereSkole }
                    (skoleinfo.skole
                        |> Input.input { msg = OppdaterSkole, label = "Skole/studiested" }
                        |> Input.withFeilmelding (Validering.feilmeldingMaxAntallTegn skoleinfo.skole maxLengthSkole)
                        |> Input.withOnEnter BrukerVilRegistrereSkole
                        |> Input.withId (inputIdTilString RegistrerSkoleInput)
                    )

            RegistrerRetning retningsinfo ->
                BrukerInput.inputMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilRegistrereRetning }
                    (retningsinfo.retning
                        |> Input.input { msg = OppdaterRetning, label = "Grad og utdanningsretning" }
                        |> Input.withFeilmelding (Validering.feilmeldingMaxAntallTegn retningsinfo.retning maxLengthStudieretning)
                        |> Input.withId (inputIdTilString RegistrerRetningInput)
                        |> Input.withOnEnter BrukerVilRegistrereRetning
                    )

            RegistrerBeskrivelse medEksempelKnapp beskrivelseinfo ->
                beskrivelseinfo.beskrivelse
                    |> Textarea.textarea { msg = OppdaterBeskrivelse, label = "Beskriv utdanningen" }
                    |> Textarea.withId (inputIdTilString RegistrerBeskrivelseInput)
                    |> Textarea.withFeilmelding (Validering.feilmeldingMaxAntallTegn beskrivelseinfo.beskrivelse maxLengthBeskrivelse)
                    |> BrukerInputMedGåVidereKnapp.textarea BrukerVilRegistrereBeskrivelse
                    |> BrukerInputMedGåVidereKnapp.withVisEksempelKnapp medEksempelKnapp VilSeEksempel
                    |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp BrukerVilAvbryteRegistreringen
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            RegistrereFraDato fraDatoInfo ->
                DatoInput.datoInput
                    { onMånedChange = OppdaterFraMåned
                    , måned = fraDatoInfo.fraMåned
                    , onÅrChange = OppdaterFraÅr
                    , år = fraDatoInfo.fraÅr
                    }
                    |> DatoInput.withFeilmeldingÅr
                        (fraDatoInfo.fraÅr
                            |> Dato.feilmeldingÅr
                            |> maybeHvisTrue fraDatoInfo.visÅrFeilmelding
                        )
                    |> DatoInput.withFokusId (inputIdTilString FraMånedId)
                    |> DatoInput.withOnBlurÅr FeltMisterFokus
                    |> BrukerInputMedGåVidereKnapp.datoMånedÅr BrukerVilGåVidereMedFraDato
                    |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp BrukerVilAvbryteRegistreringen
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            RegistrereNåværende _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerSvarerJaTilNaavarende "Ja, jeg holder fortsatt på"
                        |> Knapp.withId (inputIdTilString NåværendeId)
                    , Knapp.knapp BrukerSvarerNeiTilNaavarende "Nei, jeg er ferdig"
                    ]

            RegistrereTilDato tilDatoInfo ->
                DatoInput.datoInput
                    { onMånedChange = OppdaterTilMåned
                    , måned = tilDatoInfo.tilMåned
                    , onÅrChange = OppdaterTilÅr
                    , år = tilDatoInfo.tilÅr
                    }
                    |> DatoInput.withFeilmeldingÅr
                        (tilDatoInfo.tilÅr
                            |> Dato.feilmeldingÅr
                            |> maybeHvisTrue tilDatoInfo.visÅrFeilmelding
                        )
                    |> DatoInput.withFokusId (inputIdTilString TilMånedId)
                    |> DatoInput.withOnBlurÅr FeltMisterFokus
                    |> BrukerInputMedGåVidereKnapp.datoMånedÅr BrukerVilRegistrereTilDato
                    |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp BrukerVilAvbryteRegistreringen
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            Oppsummering _ skjema ->
                case Skjema.id (Skjema.tilUvalidertSkjema skjema) of
                    Just _ ->
                        viewBekreftOppsummering False

                    Nothing ->
                        viewBekreftOppsummering True

            EndrerSkjema skjemaType utdanningsskjema ->
                case skjemaType of
                    OppsummeringsSkjema ->
                        viewSkjema utdanningsskjema

                    NyUtdanningSkjema ->
                        viewNyUtdanningSkjema utdanningsskjema

            BekreftSlettingAvPåbegynt _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BekrefterSlettPåbegynt "Ja, jeg vil slette"
                        |> Knapp.withId (inputIdTilString SlettePåbegyntId)
                    , Knapp.knapp AngrerSlettPåbegynt "Nei, jeg vil ikke slette"
                    ]

            LeggTilFlereUtdanninger utdanninger avsluttetGrunn ->
                let
                    nyUtdanningMsg =
                        if model.harFullførtEnSamtale then
                            VilRegistrereFlereUtdanninger

                        else
                            BrukerVilRegistrereUtdanning
                in
                BrukerInput.knapper Flytende
                    ([ [ Knapp.knapp nyUtdanningMsg "Ja, legg til en utdanning"
                            |> Knapp.withId (inputIdTilString LeggTilUtdanningId)
                       , Knapp.knapp (GåTilArbeidserfaring avsluttetGrunn) "Nei, jeg er ferdig"
                       ]
                     , if List.length utdanninger > 0 then
                        [ Knapp.knapp BrukerVilRedigereUtdanning "Nei, jeg vil endre det jeg har lagt inn" ]

                       else
                        []
                     ]
                        |> List.concat
                    )

            LagrerSkjema _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    BrukerInput.utenInnhold

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilAvbryteLagringen "Gå videre"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilPrøveÅLagrePåNytt "Prøv igjen"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            , Knapp.knapp BrukerVilAvbryteLagringen "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            BekreftAvbrytingAvRegistreringen _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerBekrefterAvbrytingAvRegistrering "Ja, jeg vil avbryte"
                        |> Knapp.withId (inputIdTilString AvbrytSlettingId)
                    , Knapp.knapp BrukerVilIkkeAvbryteRegistreringen "Nei, jeg vil fortsette"
                    ]

            VenterPåAnimasjonFørFullføring _ _ ->
                BrukerInput.utenInnhold

    else
        BrukerInput.utenInnhold


type InputId
    = VilSeVitnemaalsportalenId
    | HarUtdanningId
    | BekreftHarIkkeUtdanningId
    | RedigerUtdanningId
    | VelgNivåInput
    | RegistrerSkoleInput
    | RegistrerRetningInput
    | RegistrerBeskrivelseInput
    | FraMånedId
    | TilMånedId
    | NåværendeId
    | VelgNivåISkjemaId
    | BekreftOppsummeringId
    | LeggTilUtdanningId
    | SlettePåbegyntId
    | LagringFeiletActionId
    | AvbrytSlettingId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        VilSeVitnemaalsportalenId ->
            "vil-se-vitnemaalsportalen-id"

        HarUtdanningId ->
            "har-utdanning"

        BekreftHarIkkeUtdanningId ->
            "bekreft-har-ikke-utdanning-id"

        RedigerUtdanningId ->
            "utdanning-rediger"

        VelgNivåInput ->
            "utdanning-velg-nivå"

        RegistrerSkoleInput ->
            "utdanning-registrer-skole"

        RegistrerRetningInput ->
            "utdanning-registrer-grad"

        RegistrerBeskrivelseInput ->
            "utdanning-registrer-beskrivelse"

        FraMånedId ->
            "utdanning-fra-måned-id"

        TilMånedId ->
            "utdanning-til-måned-id"

        NåværendeId ->
            "utdanning-nåværende-id"

        VelgNivåISkjemaId ->
            "utdanning-velg-nivå-skjema-id"

        BekreftOppsummeringId ->
            "utdanning-bekreft-oppsummering-id"

        LeggTilUtdanningId ->
            "utdanning-legg-til-id"

        SlettePåbegyntId ->
            "utdanning-slett-påbegynt-id"

        LagringFeiletActionId ->
            "utdanning-lagring-feilet-id"

        AvbrytSlettingId ->
            "utdanning-avbrytt-slett-id"


velgNivåKnapp : Nivå -> Knapp Msg
velgNivåKnapp nivå =
    nivå
        |> nivåToString
        |> Knapp.knapp (BrukerVilRegistrereNivå nivå)


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


viewSkjema : UtdanningSkjema -> BrukerInput Msg
viewSkjema utdanningsskjema =
    BrukerInput.skjema { lagreMsg = OppsummeringSkjemaLagreknappTrykket, lagreKnappTekst = "Lagre endringer" }
        [ Select.select "Utdanningsnivå" (Nivå >> OppsummeringEndret) selectNivåListe
            |> Select.withSelected (utdanningsskjema |> Skjema.nivå |> tilNivåKey)
            |> Select.withErObligatorisk
            |> Select.withId (inputIdTilString VelgNivåISkjemaId)
            |> Select.toHtml
        , utdanningsskjema
            |> Skjema.innholdTekstFelt Studiested
            |> Input.input { label = "Skole/studiested", msg = Tekst Studiested >> OppsummeringEndret }
            |> Input.withFeilmelding (Validering.feilmeldingMaxAntallTegn (Skjema.innholdTekstFelt Studiested utdanningsskjema) maxLengthSkole)
            |> Input.toHtml
        , utdanningsskjema
            |> Skjema.innholdTekstFelt Utdanningsretning
            |> Input.input { label = "Grad og utdanningsretning", msg = Tekst Utdanningsretning >> OppsummeringEndret }
            |> Input.withFeilmelding (Validering.feilmeldingMaxAntallTegn (Skjema.innholdTekstFelt Utdanningsretning utdanningsskjema) maxLengthStudieretning)
            |> Input.toHtml
        , utdanningsskjema
            |> Skjema.innholdTekstFelt Beskrivelse
            |> Textarea.textarea { label = "Beskriv utdanningen", msg = Tekst Beskrivelse >> OppsummeringEndret }
            |> Textarea.withFeilmelding (Validering.feilmeldingMaxAntallTegn (Skjema.innholdTekstFelt Beskrivelse utdanningsskjema) maxLengthBeskrivelse)
            |> Textarea.toHtml
        , div [ class "DatoInput-fra-til-rad" ]
            [ DatoInput.datoInput
                { onMånedChange = FraMåned >> OppsummeringEndret
                , måned = Skjema.fraMåned utdanningsskjema
                , onÅrChange = Tekst FraÅr >> OppsummeringEndret
                , år = Skjema.innholdTekstFelt FraÅr utdanningsskjema
                }
                |> DatoInput.withLabel "Når startet du på utdanningen?"
                |> DatoInput.withFeilmeldingÅr (Skjema.feilmeldingFraÅr utdanningsskjema)
                |> DatoInput.withOnBlurÅr (OppsummeringEndret FraÅrBlurred)
                |> DatoInput.toHtml
            , if not (Skjema.nåværende utdanningsskjema) then
                DatoInput.datoInput
                    { onMånedChange = TilMåned >> OppsummeringEndret
                    , måned = Skjema.tilMåned utdanningsskjema
                    , onÅrChange = Tekst TilÅr >> OppsummeringEndret
                    , år = Skjema.innholdTekstFelt TilÅr utdanningsskjema
                    }
                    |> DatoInput.withLabel "Når avsluttet du utdanningen?"
                    |> DatoInput.withFeilmeldingÅr (Skjema.feilmeldingTilÅr utdanningsskjema)
                    |> DatoInput.withOnBlurÅr (OppsummeringEndret TilÅrBlurred)
                    |> DatoInput.toHtml

              else
                text ""
            ]
        , utdanningsskjema
            |> Skjema.nåværende
            |> Checkbox.checkbox "Jeg holder fortsatt på med utdanningen" (OppsummeringEndret NåværendeToggled)
            |> Checkbox.withClass "blokk-m"
            |> Checkbox.toHtml
        ]


viewNyUtdanningSkjema : UtdanningSkjema -> BrukerInput Msg
viewNyUtdanningSkjema utdanningsskjema =
    BrukerInput.skjemaMedAvbryt { lagreMsg = OppsummeringSkjemaLagreknappTrykket, lagreKnappTekst = "Lagre utdanning", onAvbryt = Just BrukerVilAvbryteRegistreringen }
        [ ( tilNivåKey Grunnskole, "" )
            :: selectNivåListe
            |> Select.select "Utdanningsnivå" (Nivå >> OppsummeringEndret)
            |> Select.withSelected ""
            |> Select.withErObligatorisk
            |> Select.withId (inputIdTilString VelgNivåISkjemaId)
            |> Select.toHtml
        , utdanningsskjema
            |> Skjema.innholdTekstFelt Studiested
            |> Input.input { label = "Skole/studiested", msg = Tekst Studiested >> OppsummeringEndret }
            |> Input.toHtml
        , utdanningsskjema
            |> Skjema.innholdTekstFelt Utdanningsretning
            |> Input.input { label = "Grad og utdanningsretning", msg = Tekst Utdanningsretning >> OppsummeringEndret }
            |> Input.toHtml
        , utdanningsskjema
            |> Skjema.innholdTekstFelt Beskrivelse
            |> Textarea.textarea { label = "Beskriv utdanningen", msg = Tekst Beskrivelse >> OppsummeringEndret }
            |> Textarea.withFeilmelding (Validering.feilmeldingMaxAntallTegn (Skjema.innholdTekstFelt Beskrivelse utdanningsskjema) maxLengthBeskrivelse)
            |> Textarea.toHtml
        , div [ class "DatoInput-fra-til-rad" ]
            [ DatoInput.datoInput
                { onMånedChange = FraMåned >> OppsummeringEndret
                , måned = Skjema.fraMåned utdanningsskjema
                , onÅrChange = Tekst FraÅr >> OppsummeringEndret
                , år = Skjema.innholdTekstFelt FraÅr utdanningsskjema
                }
                |> DatoInput.withLabel "Når startet du på utdanningen?"
                |> DatoInput.withFeilmeldingÅr (Skjema.feilmeldingFraÅr utdanningsskjema)
                |> DatoInput.withOnBlurÅr (OppsummeringEndret FraÅrBlurred)
                |> DatoInput.toHtml
            , if not (Skjema.nåværende utdanningsskjema) then
                DatoInput.datoInput
                    { onMånedChange = TilMåned >> OppsummeringEndret
                    , måned = Skjema.tilMåned utdanningsskjema
                    , onÅrChange = Tekst TilÅr >> OppsummeringEndret
                    , år = Skjema.innholdTekstFelt TilÅr utdanningsskjema
                    }
                    |> DatoInput.withLabel "Når avsluttet du utdanningen?"
                    |> DatoInput.withFeilmeldingÅr (Skjema.feilmeldingTilÅr utdanningsskjema)
                    |> DatoInput.withOnBlurÅr (OppsummeringEndret TilÅrBlurred)
                    |> DatoInput.toHtml

              else
                text ""
            ]
        , utdanningsskjema
            |> Skjema.nåværende
            |> Checkbox.checkbox "Jeg holder fortsatt på med utdanningen" (OppsummeringEndret NåværendeToggled)
            |> Checkbox.withClass "blokk-m"
            |> Checkbox.toHtml
        ]


viewBekreftOppsummering : Bool -> BrukerInput Msg
viewBekreftOppsummering skalViseSlett =
    if skalViseSlett then
        BrukerInput.knapper Kolonne
            [ Knapp.knapp OppsummeringBekreftet "Ja, det er riktig"
                |> Knapp.withId (inputIdTilString BekreftOppsummeringId)
            , Knapp.knapp BrukerVilEndreOppsummering "Nei, jeg vil endre"
            , Knapp.knapp VilSlettePåbegynt "Nei, jeg vil slette"
            ]

    else
        BrukerInput.knapper Flytende
            [ Knapp.knapp OppsummeringBekreftet "Ja, det er riktig"
                |> Knapp.withId (inputIdTilString BekreftOppsummeringId)
            , Knapp.knapp BrukerVilEndreOppsummering "Nei, jeg vil endre"
            ]


utdanningKnapp : Utdanning -> Knapp Msg
utdanningKnapp utdanning =
    let
        text =
            case Utdanning.utdanningsretning utdanning of
                Just value ->
                    if value == "" then
                        utdanning |> Utdanning.nivå |> nivåToString

                    else
                        value

                Nothing ->
                    utdanning |> Utdanning.nivå |> nivåToString
    in
    Knapp.knapp (BrukerHarValgtUtdanningÅRedigere utdanning) text


selectNivåListe : List ( String, String )
selectNivåListe =
    [ ( tilNivåKey Grunnskole, "Grunnskole" )
    , ( tilNivåKey VideregåendeYrkesskole, "Videregående/Yrkesskole" )
    , ( tilNivåKey Fagskole, "Fagskole" )
    , ( tilNivåKey Folkehøyskole, "Folkehøyskole" )
    , ( tilNivåKey HøyereUtdanning1til4, "Høyere utdanning, 1-4 år" )
    , ( tilNivåKey HøyereUtdanning4pluss, "Høyere utdanning, mer enn 4 år" )
    , ( tilNivåKey Doktorgrad, "Doktorgrad" )
    ]


tilNivåKey : Nivå -> String
tilNivåKey nivå =
    case nivå of
        Grunnskole ->
            "Grunnskole"

        VideregåendeYrkesskole ->
            "VideregåendeYrkesskole"

        Fagskole ->
            "Fagskole"

        Folkehøyskole ->
            "Folkehøyskole"

        HøyereUtdanning1til4 ->
            "HøyereUtdanning1til4"

        HøyereUtdanning4pluss ->
            "HøyereUtdanning4pluss"

        Doktorgrad ->
            "Doktorgrad"


lagreUtdanning : (Result Error (List Utdanning) -> msg) -> ValidertUtdanningSkjema -> Cmd msg
lagreUtdanning msgConstructor skjema =
    case (Skjema.tilUvalidertSkjema >> Skjema.id) skjema of
        Just id ->
            Api.endreUtdanning msgConstructor skjema id

        Nothing ->
            Api.opprettUtdanning msgConstructor skjema


init : DebugStatus -> Posix -> FerdigAnimertMeldingsLogg -> List Utdanning -> ( Model, Cmd Msg )
init debugStatus sistLagretFraForrigeSeksjon gammelMeldingsLogg utdanningListe =
    let
        aktivSamtale =
            SpoerBrukerOmVitnemaalsportalen utdanningListe
    in
    ( Model
        { seksjonsMeldingsLogg =
            gammelMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale)
        , aktivSamtale = aktivSamtale
        , utdanningListe = utdanningListe
        , debugStatus = debugStatus
        , sistLagretFraForrigeSeksjon = sistLagretFraForrigeSeksjon
        , harFullførtEnSamtale = False
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
