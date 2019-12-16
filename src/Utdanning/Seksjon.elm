module Utdanning.Seksjon exposing
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
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import Cv.Utdanning as Utdanning exposing (Nivå(..), Utdanning)
import Dato.Dato as Dato exposing (TilDato(..), År)
import Dato.Maned as Måned exposing (Måned(..))
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.BrukerInputMedGaVidereKnapp as BrukerInputMedGåVidereKnapp
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp exposing (Knapp)
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Select as Select
import FrontendModuler.Textarea as Textarea
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import LagreStatus exposing (LagreStatus)
import Meldinger.Melding as Melding exposing (Melding, Tekstområde(..))
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Process
import Task
import Utdanning.Skjema as Skjema exposing (Felt(..), UtdanningSkjema, ValidertUtdanningSkjema)
import Validering



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , utdanningListe : List Utdanning
    , debugStatus : DebugStatus
    }


type AvsluttetGrunn
    = SlettetPåbegynt
    | EndretEksisterende
    | AnnenAvslutning


type OppsummeringsType
    = FørsteGang
    | EtterEndring
    | AvbrøtSletting


type Samtale
    = Intro (List Utdanning)
    | VelgEnUtdanningÅRedigere
    | RegistrerNivå
    | RegistrerSkole SkoleInfo
    | RegistrerRetning RetningInfo
    | RegistrerBeskrivelse Bool BeskrivelseInfo
    | RegistrereFraMåned FraDatoInfo
    | RegistrereFraÅr FraDatoInfo
    | RegistrereNåværende NåværendeInfo
    | RegistrereTilMåned TilDatoInfo
    | RegistrereTilÅr TilDatoInfo
    | Oppsummering OppsummeringsType ValidertUtdanningSkjema
    | EndrerOppsummering UtdanningSkjema
    | BekreftSlettingAvPåbegynt ValidertUtdanningSkjema
    | LagrerSkjema ValidertUtdanningSkjema LagreStatus
    | LagringFeilet Http.Error ValidertUtdanningSkjema
    | LeggTilFlereUtdanninger AvsluttetGrunn
    | VenterPåAnimasjonFørFullføring (List Utdanning) AvsluttetGrunn


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Utdanning) FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type alias SkoleInfo =
    { forrige : Nivå, skole : String }


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
    { forrige : BeskrivelseInfo
    , fraMåned : Måned
    , fraÅr : År
    }


type alias TilDatoInfo =
    { forrige : NåværendeInfo
    , tilMåned : Måned
    , tilÅr : String
    , visÅrFeilmelding : Bool
    }


maxLengthBeskrivelse =
    2000


forrigeTilRetningInfo : SkoleInfo -> RetningInfo
forrigeTilRetningInfo skole =
    { forrige = skole, retning = "" }


forrigeTilSkoleInfo : Nivå -> SkoleInfo
forrigeTilSkoleInfo nivå =
    { forrige = nivå, skole = "" }


forrigeTilBeskrivelseInfo : RetningInfo -> BeskrivelseInfo
forrigeTilBeskrivelseInfo retning =
    { forrige = retning, beskrivelse = "" }


forrigeTilFradatoInfo : BeskrivelseInfo -> FraDatoInfo
forrigeTilFradatoInfo beskrivelse =
    { forrige = beskrivelse
    , fraMåned = Januar
    , fraÅr = ""
    , visÅrFeilmelding = False
    }


forrigeTilTildatoInfo : NåværendeInfo -> TilDatoInfo
forrigeTilTildatoInfo nåværendeInfo =
    { forrige = nåværendeInfo
    , tilMåned = Januar
    , tilÅr = ""
    , visÅrFeilmelding = False
    }


forrigeTilOppsummeringInfo : TilDatoInfo -> År -> ValidertUtdanningSkjema
forrigeTilOppsummeringInfo tildatoInfo tilÅr =
    Skjema.initValidertSkjema
        { nivå = tildatoInfo.forrige.forrige.forrige.forrige.forrige
        , studiested = tildatoInfo.forrige.forrige.forrige.forrige.skole
        , utdanningsretning = tildatoInfo.forrige.forrige.forrige.retning
        , beskrivelse = tildatoInfo.forrige.forrige.beskrivelse
        , fraMåned = tildatoInfo.forrige.fraMåned
        , fraÅr = tildatoInfo.forrige.fraÅr
        , tilDato = Avsluttet tildatoInfo.tilMåned tilÅr
        , id = Nothing
        }


nåværendeInfoTilUtdanningsSkjema : NåværendeInfo -> ValidertUtdanningSkjema
nåværendeInfoTilUtdanningsSkjema nåværendeInfo =
    Skjema.initValidertSkjema
        { nivå = nåværendeInfo.forrige.forrige.forrige.forrige
        , studiested = nåværendeInfo.forrige.forrige.forrige.skole
        , utdanningsretning = nåværendeInfo.forrige.forrige.retning
        , beskrivelse = nåværendeInfo.forrige.beskrivelse
        , fraMåned = nåværendeInfo.fraMåned
        , fraÅr = nåværendeInfo.fraÅr
        , tilDato = Nåværende
        , id = Nothing
        }



--- UPDATE ---


type Msg
    = BrukerVilRegistrereUtdanning
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
    | BrukerTrykketFraMånedKnapp Måned
    | OppdaterFraÅr String
    | FraÅrMisterFokus
    | BrukerVilGåVidereMedFraÅr
    | BrukerSvarerJaTilNaavarende
    | BrukerSvarerNeiTilNaavarende
    | BrukerTrykketTilMånedKnapp Måned
    | OppdaterTilÅr String
    | TilÅrMisterFokus
    | BrukerVilGåTilOppsummering
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
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())
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
        BrukerVilRegistrereUtdanning ->
            IkkeFerdig
                ( RegistrerNivå
                    |> oppdaterSamtale model (SvarFraMsg msg)
                , lagtTilSpørsmålCmd model.debugStatus
                )

        BrukerVilRedigereUtdanning ->
            case model.utdanningListe of
                enesteUtdanning :: [] ->
                    ( enesteUtdanning
                        |> Skjema.fraUtdanning
                        |> EndrerOppsummering
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
                |> EndrerOppsummering
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
                    |> forrigeTilSkoleInfo
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
                    IkkeFerdig
                        ( skoleinfo
                            |> forrigeTilRetningInfo
                            |> RegistrerRetning
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

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
                    IkkeFerdig
                        ( retninginfo
                            |> forrigeTilBeskrivelseInfo
                            |> RegistrerBeskrivelse (detFinnesEksemplerForNivå retninginfo.forrige.forrige)
                            |> oppdaterSamtale model (SvarFraMsg msg)
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilSeEksempel ->
            case model.aktivSamtale of
                RegistrerBeskrivelse _ beskrivelseinfo ->
                    let
                        oppdatertMeldingslogg =
                            model.seksjonsMeldingsLogg
                                |> MeldingsLogg.leggTilSvar (svarFraBrukerInput model msg)
                                |> MeldingsLogg.leggTilSpørsmål (eksemplerPåUtdanning beskrivelseinfo.forrige.forrige.forrige)
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
                                    |> forrigeTilFradatoInfo
                                    |> RegistrereFraMåned
                                    |> oppdaterSamtale model (SvarFraMsg msg)
                                , lagtTilSpørsmålCmd model.debugStatus
                                )

                        Just _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerTrykketFraMånedKnapp måned ->
            case model.aktivSamtale of
                RegistrereFraMåned fraDatoInfo ->
                    ( { fraDatoInfo | fraMåned = måned }
                        |> RegistrereFraÅr
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterFraÅr string ->
            case model.aktivSamtale of
                RegistrereFraÅr fraDatoInfo ->
                    ( { fraDatoInfo | fraÅr = string }
                        |> RegistrereFraÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FraÅrMisterFokus ->
            case model.aktivSamtale of
                RegistrereFraÅr fraDatoInfo ->
                    ( { fraDatoInfo | visÅrFeilmelding = True }
                        |> RegistrereFraÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilGåVidereMedFraÅr ->
            case model.aktivSamtale of
                RegistrereFraÅr datoInfo ->
                    case Dato.stringTilÅr datoInfo.fraÅr of
                        Just fraÅr ->
                            ( { forrige = datoInfo.forrige
                              , fraMåned = datoInfo.fraMåned
                              , fraÅr = fraÅr
                              }
                                |> RegistrereNåværende
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            IkkeFerdig
                                ( { datoInfo | visÅrFeilmelding = True }
                                    |> RegistrereFraÅr
                                    |> oppdaterSamtale model IngenNyeMeldinger
                                , Cmd.none
                                )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerSvarerJaTilNaavarende ->
            case model.aktivSamtale of
                RegistrereNåværende nåværendeInfo ->
                    ( nåværendeInfo
                        |> nåværendeInfoTilUtdanningsSkjema
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
                        |> forrigeTilTildatoInfo
                        |> RegistrereTilMåned
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerTrykketTilMånedKnapp måned ->
            case model.aktivSamtale of
                RegistrereTilMåned tilDatoInfo ->
                    ( { tilDatoInfo | tilMåned = måned }
                        |> RegistrereTilÅr
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterTilÅr string ->
            case model.aktivSamtale of
                RegistrereTilÅr tilDatoInfo ->
                    ( { tilDatoInfo | tilÅr = string }
                        |> RegistrereTilÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        TilÅrMisterFokus ->
            case model.aktivSamtale of
                RegistrereTilÅr tilDatoInfo ->
                    ( { tilDatoInfo | visÅrFeilmelding = True }
                        |> RegistrereTilÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilGåTilOppsummering ->
            case model.aktivSamtale of
                RegistrereTilÅr tilDatoInfo ->
                    case Dato.stringTilÅr tilDatoInfo.tilÅr of
                        Just år ->
                            ( år
                                |> forrigeTilOppsummeringInfo tilDatoInfo
                                |> Oppsummering FørsteGang
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { tilDatoInfo | visÅrFeilmelding = True }
                                |> RegistrereTilÅr
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
                    ( LeggTilFlereUtdanninger SlettetPåbegynt
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
                EndrerOppsummering skjema ->
                    case Skjema.validerSkjema skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> Oppsummering EtterEndring
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar (validertSkjemaTilSetninger validertSkjema)))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            IkkeFerdig
                                ( skjema
                                    |> Skjema.gjørAlleFeilmeldingerSynlig
                                    |> EndrerOppsummering
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
                            [ postEllerPutUtdanning UtdanningSendtTilApi feiletskjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        UtdanningSendtTilApi result ->
            case model.aktivSamtale of
                LagrerSkjema skjema lagreStatus ->
                    case result of
                        Ok value ->
                            let
                                avsluttetGrunn =
                                    if List.length model.utdanningListe == List.length value then
                                        EndretEksisterende

                                    else
                                        AnnenAvslutning
                            in
                            ( if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                oppdaterSamtale
                                    { model | utdanningListe = value }
                                    (ManueltSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ]))
                                    (LeggTilFlereUtdanninger avsluttetGrunn)

                              else
                                LeggTilFlereUtdanninger avsluttetGrunn
                                    |> oppdaterSamtale { model | utdanningListe = value } UtenSvar
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
                                    , postEllerPutUtdanning UtdanningSendtTilApi skjema
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
                        Ok value ->
                            let
                                avsluttetGrunn =
                                    if List.length model.utdanningListe == List.length value then
                                        EndretEksisterende

                                    else
                                        AnnenAvslutning
                            in
                            ( LeggTilFlereUtdanninger avsluttetGrunn
                                |> oppdaterSamtale { model | utdanningListe = value } UtenSvar
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
                            [ postEllerPutUtdanning UtdanningSendtTilApi validertSkjema
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
                                    , postEllerPutUtdanning UtdanningSendtTilApi validertSkjema
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
                EndrerOppsummering skjema ->
                    ( skjema
                        |> oppdaterSkjema skjemaEndring
                        |> EndrerOppsummering
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
        RegistrerSkole _ ->
            settFokusCmd RegistrerSkoleInput

        RegistrerRetning _ ->
            settFokusCmd RegistrerRetningInput

        RegistrerBeskrivelse _ _ ->
            settFokusCmd RegistrerBeskrivelseInput

        RegistrereFraÅr _ ->
            settFokusCmd RegistrereFraÅrInput

        RegistrereTilÅr _ ->
            settFokusCmd RegistrereTilÅrInput

        EndrerOppsummering _ ->
            settFokusCmd EndrerOppsummeringInput

        _ ->
            Cmd.none


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding model ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring utdanningsListe _ ->
                    Ferdig utdanningsListe ferdigAnimertSamtale

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
        |> EndrerOppsummering
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> Msg -> ValidertUtdanningSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model msg skjema =
    ( LagreStatus.init
        |> LagrerSkjema skjema
        |> oppdaterSamtale model (SvarFraMsg msg)
    , postEllerPutUtdanning UtdanningSendtTilApi skjema
    )
        |> IkkeFerdig


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


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

        VelgEnUtdanningÅRedigere ->
            [ Melding.spørsmål [ "Hvilken registrerte utdanning ønsker du å redigere?" ] ]

        RegistrerNivå ->
            [ Melding.spørsmål [ "Legg inn én utdanning av gangen." ]
            , Melding.spørsmål [ "Hvis du har en bachelorgrad, velg høyere utdanning 1-4 år. Har du en mastergrad, velg høyere utdanning mer enn 4 år." ]
            , Melding.spørsmål [ "Hvilket nivå har utdanningen du skal legge inn?" ]
            ]

        RegistrerSkole skoleinfo ->
            case skoleinfo.forrige of
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

        RegistrereFraMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned begynte du på utdanningen din?" ]
            , Melding.spørsmål [ "De fleste studiene i Norge begynner i august." ]
            ]

        RegistrereFraÅr _ ->
            [ Melding.spørsmål [ "Hvilket år begynte du på utdanningen din?" ] ]

        RegistrereNåværende _ ->
            [ Melding.spørsmål [ "Holder du fortsatt på med utdanningen?" ] ]

        RegistrereTilMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned fullførte du utdanningen din?" ]
            , Melding.spørsmål [ "De fleste studier i Norge er ferdig i juni." ]
            ]

        RegistrereTilÅr _ ->
            [ Melding.spørsmål [ "Hvilket år fullførte du utdanningen din?" ] ]

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

        EndrerOppsummering _ ->
            [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

        BekreftSlettingAvPåbegynt _ ->
            [ Melding.spørsmål [ "Er du sikker på at du vil slette denne utdanningen?" ] ]

        LeggTilFlereUtdanninger avsluttetGrunn ->
            case avsluttetGrunn of
                SlettetPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg slettet utdanningen. Vil du legge inn flere utdanninger?" ] ]

                EndretEksisterende ->
                    [ Melding.spørsmål [ "Så bra! Nå er utdanningen endret👍" ]
                    , Melding.spørsmål [ "Vil du legge inn flere utdanninger? " ]
                    ]

                AnnenAvslutning ->
                    [ Melding.spørsmål [ "Så bra! Nå er utdanningen lagret👍" ]
                    , Melding.spørsmål [ "Vil du legge inn flere utdanninger? " ]
                    ]

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre utdanning" } ]

        VenterPåAnimasjonFørFullføring liste avsluttetGrunn ->
            if List.isEmpty liste then
                if avsluttetGrunn == SlettetPåbegynt then
                    [ Melding.spørsmål [ "Da går vi videre til arbeidserfaring." ] ]

                else
                    [ Melding.spørsmål [ "Siden du ikke har utdanning, går vi videre til arbeidserfaring." ] ]

            else
                [ Melding.spørsmål [ "Bra jobba! Da går vi videre." ] ]

        LagrerSkjema _ _ ->
            []


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
            Intro _ ->
                if List.isEmpty model.utdanningListe then
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp BrukerVilRegistrereUtdanning "Ja, jeg har utdanning"
                        , Knapp.knapp (GåTilArbeidserfaring AnnenAvslutning) "Nei, jeg har ikke utdanning"
                        ]

                else
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp BrukerVilRegistrereUtdanning "Ja, legg til en utdanning"
                        , Knapp.knapp (GåTilArbeidserfaring AnnenAvslutning) "Nei, jeg er ferdig"
                        , Knapp.knapp BrukerVilRedigereUtdanning "Nei, jeg vil endre det jeg har lagt inn"
                        ]

            VelgEnUtdanningÅRedigere ->
                BrukerInput.knapper Kolonne
                    (lagUtdanningKnapper model.utdanningListe)

            RegistrerNivå ->
                BrukerInput.knapper Kolonne
                    [ Knapp.knapp (BrukerVilRegistrereNivå Grunnskole) (nivåToString Grunnskole)
                    , Knapp.knapp (BrukerVilRegistrereNivå VideregåendeYrkesskole) (nivåToString VideregåendeYrkesskole)
                    , Knapp.knapp (BrukerVilRegistrereNivå Fagskole) (nivåToString Fagskole)
                    , Knapp.knapp (BrukerVilRegistrereNivå Folkehøyskole) (nivåToString Folkehøyskole)
                    , Knapp.knapp (BrukerVilRegistrereNivå HøyereUtdanning1til4) (nivåToString HøyereUtdanning1til4)
                    , Knapp.knapp (BrukerVilRegistrereNivå HøyereUtdanning4pluss) (nivåToString HøyereUtdanning4pluss)
                    , Knapp.knapp (BrukerVilRegistrereNivå Doktorgrad) (nivåToString Doktorgrad)
                    ]

            RegistrerSkole skoleinfo ->
                BrukerInput.inputMedGåVidereKnapp BrukerVilRegistrereSkole
                    (skoleinfo.skole
                        |> Input.input { msg = OppdaterSkole, label = "Skole/studiested" }
                        |> Input.withOnEnter BrukerVilRegistrereSkole
                        |> Input.withId (inputIdTilString RegistrerSkoleInput)
                    )

            RegistrerRetning retningsinfo ->
                BrukerInput.inputMedGåVidereKnapp BrukerVilRegistrereRetning
                    (retningsinfo.retning
                        |> Input.input { msg = OppdaterRetning, label = "Grad og utdanningsretning" }
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
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            RegistrereFraMåned _ ->
                BrukerInput.månedKnapper BrukerTrykketFraMånedKnapp

            RegistrereFraÅr fraDatoInfo ->
                BrukerInput.inputMedGåVidereKnapp BrukerVilGåVidereMedFraÅr
                    (fraDatoInfo.fraÅr
                        |> Input.input { label = "År", msg = OppdaterFraÅr }
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue fraDatoInfo.visÅrFeilmelding) fraDatoInfo.fraÅr)
                        |> Input.withId (inputIdTilString RegistrereFraÅrInput)
                        |> Input.withOnEnter BrukerVilGåVidereMedFraÅr
                        |> Input.withOnBlur FraÅrMisterFokus
                        |> Input.withErObligatorisk
                    )

            RegistrereNåværende _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerSvarerJaTilNaavarende "Ja, jeg holder fortsatt på"
                    , Knapp.knapp BrukerSvarerNeiTilNaavarende "Nei, jeg er ferdig"
                    ]

            RegistrereTilMåned _ ->
                BrukerInput.månedKnapper BrukerTrykketTilMånedKnapp

            RegistrereTilÅr tilDatoInfo ->
                BrukerInput.inputMedGåVidereKnapp BrukerVilGåTilOppsummering
                    (tilDatoInfo.tilÅr
                        |> Input.input { label = "År", msg = OppdaterTilÅr }
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue tilDatoInfo.visÅrFeilmelding) tilDatoInfo.tilÅr)
                        |> Input.withId (inputIdTilString RegistrereTilÅrInput)
                        |> Input.withOnEnter BrukerVilGåTilOppsummering
                        |> Input.withOnBlur TilÅrMisterFokus
                        |> Input.withErObligatorisk
                    )

            Oppsummering _ skjema ->
                case Skjema.id (Skjema.tilUvalidertSkjema skjema) of
                    Just _ ->
                        viewBekreftOppsummering False

                    Nothing ->
                        viewBekreftOppsummering True

            EndrerOppsummering utdanningsskjema ->
                viewSkjema utdanningsskjema

            BekreftSlettingAvPåbegynt _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BekrefterSlettPåbegynt "Ja, jeg vil slette"
                    , Knapp.knapp AngrerSlettPåbegynt "Nei, jeg vil ikke slette"
                    ]

            LeggTilFlereUtdanninger avsluttetGrunn ->
                BrukerInput.knapper Flytende
                    ([ [ Knapp.knapp BrukerVilRegistrereUtdanning "Ja, legg til en utdanning"
                       , Knapp.knapp (GåTilArbeidserfaring avsluttetGrunn) "Nei, jeg er ferdig"
                       ]
                     , if List.length model.utdanningListe > 0 then
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
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilPrøveÅLagrePåNytt "Prøv igjen"
                            , Knapp.knapp BrukerVilAvbryteLagringen "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            VenterPåAnimasjonFørFullføring _ _ ->
                BrukerInput.utenInnhold

    else
        BrukerInput.utenInnhold


type InputId
    = RegistrerSkoleInput
    | RegistrerRetningInput
    | RegistrerBeskrivelseInput
    | RegistrereFraÅrInput
    | RegistrereTilÅrInput
    | EndrerOppsummeringInput


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        RegistrerSkoleInput ->
            "utdanning-registrer-skole"

        RegistrerRetningInput ->
            "utdanning-registrer-grad"

        RegistrerBeskrivelseInput ->
            "utdanning-registrer-beskrivelse"

        RegistrereFraÅrInput ->
            "utdanning-registrere-fra-år"

        RegistrereTilÅrInput ->
            "utdanning-registrere-til-år"

        EndrerOppsummeringInput ->
            "utdanning-endrer-oppsummering"


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
                { label = "Når startet du på utdanningen?"
                , onMånedChange = FraMåned >> OppsummeringEndret
                , måned = Skjema.fraMåned utdanningsskjema
                , onÅrChange = Tekst FraÅr >> OppsummeringEndret
                , år = Skjema.innholdTekstFelt FraÅr utdanningsskjema
                }
                |> DatoInput.withFeilmeldingÅr (Skjema.feilmeldingFraÅr utdanningsskjema)
                |> DatoInput.withOnBlurÅr (OppsummeringEndret FraÅrBlurred)
                |> DatoInput.toHtml
            , if not (Skjema.nåværende utdanningsskjema) then
                DatoInput.datoInput
                    { label = "Når avsluttet du utdanningen?"
                    , onMånedChange = TilMåned >> OppsummeringEndret
                    , måned = Skjema.tilMåned utdanningsskjema
                    , onÅrChange = Tekst TilÅr >> OppsummeringEndret
                    , år = Skjema.innholdTekstFelt TilÅr utdanningsskjema
                    }
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
            , Knapp.knapp BrukerVilEndreOppsummering "Nei, jeg vil endre"
            , Knapp.knapp VilSlettePåbegynt "Nei, jeg vil slette"
            ]

    else
        BrukerInput.knapper Flytende
            [ Knapp.knapp OppsummeringBekreftet "Ja, det er riktig"
            , Knapp.knapp BrukerVilEndreOppsummering "Nei, jeg vil endre"
            ]


lagUtdanningKnapper : List Utdanning -> List (Knapp Msg)
lagUtdanningKnapper utdanninger =
    utdanninger
        |> List.map
            (\utdanning ->
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
            )


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


postEllerPutUtdanning : (Result Error (List Utdanning) -> msg) -> ValidertUtdanningSkjema -> Cmd msg
postEllerPutUtdanning msgConstructor skjema =
    case (Skjema.tilUvalidertSkjema >> Skjema.id) skjema of
        Just id ->
            Api.putUtdanning msgConstructor skjema id

        Nothing ->
            Api.postUtdanning msgConstructor skjema


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List Utdanning -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg utdanningListe =
    let
        aktivSamtale =
            Intro utdanningListe
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål
                (samtaleTilMeldingsLogg aktivSamtale)
                (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , utdanningListe = utdanningListe
        , debugStatus = debugStatus
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
