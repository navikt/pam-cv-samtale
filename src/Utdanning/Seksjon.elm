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
import Dato exposing (Måned(..), TilDato(..), År)
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.ManedKnapper as MånedKnapper
import FrontendModuler.Select as Select
import FrontendModuler.Textarea as Textarea
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import LagreStatus exposing (LagreStatus)
import Meldinger.Melding as Melding exposing (Melding, Tekstområde(..))
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
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
    | LaTilNy
    | EndretEksisterende


type Samtale
    = Intro (List Utdanning)
    | VelgEnUtdanningÅRedigere
    | RegistrerNivå
    | RegistrerSkole SkoleInfo
    | RegistrerRetning RetningInfo
    | RegistrerBeskrivelse BeskrivelseInfo
    | RegistrereFraMåned FraDatoInfo
    | RegistrereFraÅr FraDatoInfo
    | RegistrereNåværende NåværendeInfo
    | RegistrereTilMåned TilDatoInfo
    | RegistrereTilÅr TilDatoInfo
    | Oppsummering ValidertUtdanningSkjema
    | EndrerOppsummering UtdanningSkjema
    | OppsummeringEtterEndring ValidertUtdanningSkjema
    | BekreftSlettingAvPåbegynt ValidertUtdanningSkjema
    | LagrerSkjema ValidertUtdanningSkjema LagreStatus
    | LagringFeilet Http.Error ValidertUtdanningSkjema
    | LeggTilFlereUtdanninger AvsluttetGrunn
    | VenterPåAnimasjonFørFullføring (List Utdanning)


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
    | BrukerVilRedigereUtdanning String
    | BrukerHarValgtUtdanningÅRedigere Utdanning String
    | GåTilArbeidserfaring String
    | BrukerVilRegistrereNivå Nivå
    | BrukerVilRegistrereSkole
    | OppdaterSkole String
    | BrukerVilRegistrereRetning
    | OppdaterRetning String
    | BrukerVilRegistrereBeskrivelse
    | OppdaterBeskrivelse String
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
            case model.aktivSamtale of
                Intro utdanningListe ->
                    if List.isEmpty utdanningListe then
                        IkkeFerdig
                            ( nesteSamtaleSteg model (Melding.svar [ "Ja, jeg har utdannning" ]) RegistrerNivå
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                    else
                        IkkeFerdig
                            ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til en utdanning" ]) RegistrerNivå
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                LeggTilFlereUtdanninger _ ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til en utdanning" ]) RegistrerNivå
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRedigereUtdanning knappeTekst ->
            case model.utdanningListe of
                enesteUtdanning :: [] ->
                    ( enesteUtdanning
                        |> Skjema.fraUtdanning
                        |> EndrerOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                [] ->
                    IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    ( VelgEnUtdanningÅRedigere
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BrukerHarValgtUtdanningÅRedigere utdanning knappeTekst ->
            ( utdanning
                |> Skjema.fraUtdanning
                |> EndrerOppsummering
                |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        GåTilArbeidserfaring knappeTekst ->
            ( model.utdanningListe
                |> VenterPåAnimasjonFørFullføring
                |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilRegistrereNivå nivå ->
            case model.aktivSamtale of
                RegistrerNivå ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ nivåToString nivå ]) (RegistrerSkole (forrigeTilSkoleInfo nivå))
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereSkole ->
            case model.aktivSamtale of
                RegistrerSkole skoleinfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ skoleinfo.skole ]) (RegistrerRetning (forrigeTilRetningInfo skoleinfo))
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereRetning ->
            case model.aktivSamtale of
                RegistrerRetning retninginfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ retninginfo.retning ]) (RegistrerBeskrivelse (forrigeTilBeskrivelseInfo retninginfo))
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereBeskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse beskrivelseinfo ->
                    case Validering.feilmeldingMaxAntallTegn beskrivelseinfo.beskrivelse maxLengthBeskrivelse of
                        Nothing ->
                            let
                                trimmetBeskrivelseinfo =
                                    { beskrivelseinfo | beskrivelse = String.trim beskrivelseinfo.beskrivelse }
                            in
                            IkkeFerdig
                                ( nesteSamtaleSteg model (Melding.svar [ trimmetBeskrivelseinfo.beskrivelse ]) (RegistrereFraMåned (forrigeTilFradatoInfo trimmetBeskrivelseinfo))
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
                        |> nesteSamtaleSteg model (Melding.svar [ Dato.månedTilString måned ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterFraÅr string ->
            case model.aktivSamtale of
                RegistrereFraÅr fraDatoInfo ->
                    ( Model
                        { model
                            | aktivSamtale =
                                RegistrereFraÅr { fraDatoInfo | fraÅr = string }
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FraÅrMisterFokus ->
            case model.aktivSamtale of
                RegistrereFraÅr fraDatoInfo ->
                    ( Model
                        { model
                            | aktivSamtale =
                                RegistrereFraÅr { fraDatoInfo | visÅrFeilmelding = True }
                        }
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
                                |> nesteSamtaleSteg model (Melding.svar [ datoInfo.fraÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            IkkeFerdig
                                ( { datoInfo | visÅrFeilmelding = True }
                                    |> RegistrereFraÅr
                                    |> oppdaterSamtaleSteg model
                                , Cmd.none
                                )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerSvarerJaTilNaavarende ->
            case model.aktivSamtale of
                RegistrereNåværende nåværendeInfo ->
                    ( nåværendeInfo
                        |> nåværendeInfoTilUtdanningsSkjema
                        |> Oppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, jeg holder fortsatt på" ])
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
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg er ferdig" ])
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
                        |> nesteSamtaleSteg model (Melding.svar [ Dato.månedTilString måned ])
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
                        |> oppdaterSamtaleSteg model
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
                        |> oppdaterSamtaleSteg model
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
                            ( forrigeTilOppsummeringInfo tilDatoInfo år
                                |> Oppsummering
                                |> nesteSamtaleSteg model (Melding.svar [ tilDatoInfo.tilÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { tilDatoInfo | visÅrFeilmelding = True }
                                |> RegistrereTilÅr
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilEndreOppsummering ->
            case model.aktivSamtale of
                Oppsummering utdanningskjema ->
                    updateEtterVilEndreSkjema model utdanningskjema

                OppsummeringEtterEndring utdanningskjema ->
                    updateEtterVilEndreSkjema model utdanningskjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterRetning retning ->
            case model.aktivSamtale of
                RegistrerRetning retningsinfo ->
                    IkkeFerdig ( oppdaterSamtaleSteg model (RegistrerRetning { retningsinfo | retning = retning }), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterSkole skole ->
            case model.aktivSamtale of
                RegistrerSkole skoleinfo ->
                    IkkeFerdig ( oppdaterSamtaleSteg model (RegistrerSkole { skoleinfo | skole = skole }), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterBeskrivelse beskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse beskrivelseinfo ->
                    IkkeFerdig ( oppdaterSamtaleSteg model (RegistrerBeskrivelse { beskrivelseinfo | beskrivelse = beskrivelse }), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilSlettePåbegynt ->
            case model.aktivSamtale of
                Oppsummering skjema ->
                    ( BekreftSlettingAvPåbegynt skjema
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil slette" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                OppsummeringEtterEndring skjema ->
                    ( BekreftSlettingAvPåbegynt skjema
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil slette" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BekrefterSlettPåbegynt ->
            case model.aktivSamtale of
                BekreftSlettingAvPåbegynt _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, jeg vil slette" ]) (LeggTilFlereUtdanninger SlettetPåbegynt), lagtTilSpørsmålCmd model.debugStatus )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        AngrerSlettPåbegynt ->
            case model.aktivSamtale of
                BekreftSlettingAvPåbegynt skjema ->
                    ( Oppsummering skjema
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil ikke slette." ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppsummeringBekreftet ->
            case model.aktivSamtale of
                Oppsummering ferdigskjema ->
                    updateEtterLagreKnappTrykket model ferdigskjema

                OppsummeringEtterEndring ferdigskjema ->
                    updateEtterLagreKnappTrykket model ferdigskjema

                LeggTilFlereUtdanninger _ ->
                    ( VenterPåAnimasjonFørFullføring model.utdanningListe
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg er ferdig." ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppsummeringSkjemaLagreknappTrykket ->
            case model.aktivSamtale of
                EndrerOppsummering skjema ->
                    case Skjema.validerSkjema skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> OppsummeringEtterEndring
                                |> nesteSamtaleSteg model (Melding.svar (validertSkjemaTilSetninger validertSkjema))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            IkkeFerdig
                                ( skjema
                                    |> Skjema.gjørAlleFeilmeldingerSynlig
                                    |> EndrerOppsummering
                                    |> oppdaterSamtaleSteg model
                                , Cmd.none
                                )

                LagringFeilet error feiletskjema ->
                    IkkeFerdig
                        ( error
                            |> LagreStatus.fraError
                            |> LagrerSkjema feiletskjema
                            |> nesteSamtaleSteg model (Melding.svar [ "Bekreft" ])
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
                                        LaTilNy
                            in
                            ( if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                LeggTilFlereUtdanninger avsluttetGrunn
                                    |> nesteSamtaleSteg { model | utdanningListe = value } (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])

                              else
                                LeggTilFlereUtdanninger avsluttetGrunn
                                    |> nesteSamtaleStegUtenMelding { model | utdanningListe = value }
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtaleSteg model
                                    , postEllerPutUtdanning UtdanningSendtTilApi skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtaleSteg model
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre utdanning" error
                                    )
                                        |> IkkeFerdig

                            else
                                ( skjema
                                    |> LagringFeilet error
                                    |> nesteSamtaleStegUtenMelding model
                                , Cmd.batch
                                    [ lagtTilSpørsmålCmd model.debugStatus
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre utdanning" error
                                    ]
                                )
                                    |> IkkeFerdig

                Oppsummering skjema ->
                    case result of
                        Ok value ->
                            let
                                avsluttetGrunn =
                                    if List.length model.utdanningListe == List.length value then
                                        EndretEksisterende

                                    else
                                        LaTilNy
                            in
                            ( nesteSamtaleStegUtenMelding { model | utdanningListe = value } (LeggTilFlereUtdanninger avsluttetGrunn), lagtTilSpørsmålCmd model.debugStatus )
                                |> IkkeFerdig

                        Err error ->
                            ( nesteSamtaleStegUtenMelding model (LagringFeilet error skjema)
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
                            |> nesteSamtaleSteg model (Melding.svar [ "Prøv igjen" ])
                        , Cmd.batch
                            [ postEllerPutUtdanning UtdanningSendtTilApi validertSkjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilAvbryteLagringen ->
            ( nesteSamtaleSteg model (Melding.svar [ "Avbryt lagring" ]) (Intro model.utdanningListe)
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
                                        |> oppdaterSamtaleSteg model
                                    , postEllerPutUtdanning UtdanningSendtTilApi validertSkjema
                                    )

                            else
                                IkkeFerdig ( Model model, Cmd.none )

                        LagrerSkjema skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerSkjema skjema
                                |> oppdaterSamtaleSteg model
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
                    ( Model
                        { model
                            | aktivSamtale =
                                skjema
                                    |> oppdaterSkjema skjemaEndring
                                    |> EndrerOppsummering
                        }
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
                |> Dato.stringTilMåned
                |> Skjema.oppdaterFraMåned skjema

        TilMåned månedString ->
            månedString
                |> Dato.stringTilMåned
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

        RegistrerBeskrivelse _ ->
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
                VenterPåAnimasjonFørFullføring utdanningsListe ->
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


updateEtterVilEndreSkjema : ModelInfo -> ValidertUtdanningSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model skjema =
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> EndrerOppsummering
        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> ValidertUtdanningSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model skjema =
    ( LagreStatus.init
        |> LagrerSkjema skjema
        |> nesteSamtaleSteg model (Melding.svar [ "Ja, det er riktig" ])
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


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
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

        RegistrerBeskrivelse _ ->
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

        Oppsummering validertSkjema ->
            [ [ [ "Du har lagt inn dette:"
                , Melding.tomLinje
                ]
              , validertSkjemaTilSetninger validertSkjema
              , [ Melding.tomLinje
                , "Er informasjonen riktig?"
                ]
              ]
                |> List.concat
                |> Melding.spørsmål
            ]

        EndrerOppsummering _ ->
            [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

        OppsummeringEtterEndring _ ->
            [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

        BekreftSlettingAvPåbegynt _ ->
            [ Melding.spørsmål [ "Er du sikker på at du vil slette denne utdanningen?" ] ]

        LeggTilFlereUtdanninger avsluttetGrunn ->
            case avsluttetGrunn of
                SlettetPåbegynt ->
                    [ Melding.spørsmål [ "Utdanningen ble ikke lagret." ]
                    , Melding.spørsmål [ "Vil du legge inn andre utdanninger? " ]
                    ]

                EndretEksisterende ->
                    [ Melding.spørsmål [ "Så bra! Nå er utdanningen endret👍" ]
                    , Melding.spørsmål [ "Vil du legge inn flere utdanninger? " ]
                    ]

                LaTilNy ->
                    [ Melding.spørsmål [ "Så bra! Nå er utdanningen lagret👍" ]
                    , Melding.spørsmål [ "Vil du legge inn flere utdanninger? " ]
                    ]

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre utdanning" } ]

        VenterPåAnimasjonFørFullføring liste ->
            if List.isEmpty liste then
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



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            Intro _ ->
                if List.isEmpty model.utdanningListe then
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerVilRegistrereUtdanning "Ja, jeg har utdannning"
                            |> Knapp.toHtml
                        , "Nei, jeg har ikke utdanning"
                            |> Knapp.knapp (GåTilArbeidserfaring "Nei, jeg har ikke utdanning")
                            |> Knapp.toHtml
                        ]

                else
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerVilRegistrereUtdanning "Ja, legg til en utdanning"
                            |> Knapp.toHtml
                        , "Nei, jeg er ferdig"
                            |> Knapp.knapp (GåTilArbeidserfaring "Nei, jeg er ferdig")
                            |> Knapp.toHtml
                        , "Nei, jeg vil endre det jeg har lagt inn"
                            |> Knapp.knapp (BrukerVilRedigereUtdanning "Nei, jeg vil endre det jeg har lagt inn")
                            |> Knapp.toHtml
                        ]

            VelgEnUtdanningÅRedigere ->
                Containers.knapper Kolonne
                    (lagUtdanningKnapper model.utdanningListe)

            RegistrerNivå ->
                Containers.knapper Kolonne
                    [ Knapp.knapp (BrukerVilRegistrereNivå Grunnskole) (nivåToString Grunnskole)
                        |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå VideregåendeYrkesskole) (nivåToString VideregåendeYrkesskole)
                        |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå Fagskole) (nivåToString Fagskole)
                        |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå Folkehøyskole) (nivåToString Folkehøyskole)
                        |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå HøyereUtdanning1til4) (nivåToString HøyereUtdanning1til4)
                        |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå HøyereUtdanning4pluss) (nivåToString HøyereUtdanning4pluss)
                        |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå Doktorgrad) (nivåToString Doktorgrad)
                        |> Knapp.toHtml
                    ]

            RegistrerSkole skoleinfo ->
                Containers.inputMedGåVidereKnapp BrukerVilRegistrereSkole
                    [ skoleinfo.skole
                        |> Input.input { msg = OppdaterSkole, label = "Skole/studiested" }
                        |> Input.withOnEnter BrukerVilRegistrereSkole
                        |> Input.withId (inputIdTilString RegistrerSkoleInput)
                        |> Input.toHtml
                    ]

            RegistrerRetning retningsinfo ->
                Containers.inputMedGåVidereKnapp BrukerVilRegistrereRetning
                    [ retningsinfo.retning
                        |> Input.input { msg = OppdaterRetning, label = "Grad og utdanningsretning" }
                        |> Input.withId (inputIdTilString RegistrerRetningInput)
                        |> Input.withOnEnter BrukerVilRegistrereRetning
                        |> Input.toHtml
                    ]

            RegistrerBeskrivelse beskrivelseinfo ->
                Containers.inputMedGåVidereKnapp BrukerVilRegistrereBeskrivelse
                    [ beskrivelseinfo.beskrivelse
                        |> Textarea.textarea { msg = OppdaterBeskrivelse, label = "Beskriv utdanningen" }
                        |> Textarea.withId (inputIdTilString RegistrerBeskrivelseInput)
                        |> Textarea.withMaybeFeilmelding (Validering.feilmeldingMaxAntallTegn beskrivelseinfo.beskrivelse maxLengthBeskrivelse)
                        |> Textarea.toHtml
                    ]

            RegistrereFraMåned _ ->
                MånedKnapper.månedKnapper BrukerTrykketFraMånedKnapp

            RegistrereFraÅr fraDatoInfo ->
                Containers.inputMedGåVidereKnapp BrukerVilGåVidereMedFraÅr
                    [ div [ class "år-wrapper" ]
                        [ fraDatoInfo.fraÅr
                            |> Input.input { label = "År", msg = OppdaterFraÅr }
                            |> Input.withMaybeFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue fraDatoInfo.visÅrFeilmelding) fraDatoInfo.fraÅr)
                            |> Input.withId (inputIdTilString RegistrereFraÅrInput)
                            |> Input.withOnEnter BrukerVilGåVidereMedFraÅr
                            |> Input.withOnBlur FraÅrMisterFokus
                            |> Input.withErObligatorisk
                            |> Input.toHtml
                        ]
                    ]

            RegistrereNåværende _ ->
                Containers.knapper Flytende
                    [ Knapp.knapp BrukerSvarerJaTilNaavarende "Ja, jeg holder fortsatt på"
                        |> Knapp.toHtml
                    , Knapp.knapp BrukerSvarerNeiTilNaavarende "Nei, jeg er ferdig"
                        |> Knapp.toHtml
                    ]

            RegistrereTilMåned _ ->
                MånedKnapper.månedKnapper BrukerTrykketTilMånedKnapp

            RegistrereTilÅr tilDatoInfo ->
                Containers.inputMedGåVidereKnapp BrukerVilGåTilOppsummering
                    [ div [ class "år-wrapper" ]
                        [ tilDatoInfo.tilÅr
                            |> Input.input { label = "År", msg = OppdaterTilÅr }
                            |> Input.withMaybeFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue tilDatoInfo.visÅrFeilmelding) tilDatoInfo.tilÅr)
                            |> Input.withId (inputIdTilString RegistrereTilÅrInput)
                            |> Input.withOnEnter BrukerVilGåTilOppsummering
                            |> Input.withOnBlur TilÅrMisterFokus
                            |> Input.withErObligatorisk
                            |> Input.toHtml
                        ]
                    ]

            Oppsummering skjema ->
                case Skjema.id (Skjema.tilUvalidertSkjema skjema) of
                    Just _ ->
                        viewBekreftOppsummering False

                    Nothing ->
                        viewBekreftOppsummering True

            OppsummeringEtterEndring skjema ->
                case Skjema.id (Skjema.tilUvalidertSkjema skjema) of
                    Just _ ->
                        viewBekreftOppsummering False

                    Nothing ->
                        viewBekreftOppsummering True

            EndrerOppsummering utdanningsskjema ->
                viewSkjema utdanningsskjema

            BekreftSlettingAvPåbegynt _ ->
                Containers.knapper Flytende
                    [ Knapp.knapp BekrefterSlettPåbegynt "Ja, jeg vil slette"
                        |> Knapp.toHtml
                    , Knapp.knapp AngrerSlettPåbegynt "Nei, jeg vil ikke slette"
                        |> Knapp.toHtml
                    ]

            LeggTilFlereUtdanninger _ ->
                Containers.knapper Flytende
                    [ Knapp.knapp BrukerVilRegistrereUtdanning "Ja, legg til en utdanning"
                        |> Knapp.toHtml
                    , Knapp.knapp OppsummeringBekreftet "Nei, jeg er ferdig"
                        |> Knapp.toHtml
                    , if List.length model.utdanningListe > 0 then
                        "Nei, jeg vil endre det jeg har lagt inn"
                            |> Knapp.knapp (BrukerVilRedigereUtdanning "Nei, jeg vil endre det jeg har lagt inn")
                            |> Knapp.toHtml

                      else
                        text ""
                    ]

            LagrerSkjema _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    text ""

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        Containers.knapper Flytende
                            [ Knapp.knapp BrukerVilAvbryteLagringen "Gå videre"
                                |> Knapp.toHtml
                            ]

                    PrøvPåNytt ->
                        Containers.knapper Flytende
                            [ Knapp.knapp BrukerVilPrøveÅLagrePåNytt "Prøv igjen"
                                |> Knapp.toHtml
                            , Knapp.knapp BrukerVilAvbryteLagringen "Gå videre"
                                |> Knapp.toHtml
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            VenterPåAnimasjonFørFullføring _ ->
                div [] []

    else
        text ""


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


viewSkjema : UtdanningSkjema -> Html Msg
viewSkjema utdanningsskjema =
    Containers.skjema { lagreMsg = OppsummeringSkjemaLagreknappTrykket, lagreKnappTekst = "Lagre endringer" }
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
            |> Textarea.withMaybeFeilmelding (Validering.feilmeldingMaxAntallTegn (Skjema.innholdTekstFelt Beskrivelse utdanningsskjema) maxLengthBeskrivelse)
            |> Textarea.toHtml
        , div [ class "DatoInput-fra-til-rad" ]
            [ DatoInput.datoInput
                { label = "Når startet du på utdanningen?"
                , onMånedChange = FraMåned >> OppsummeringEndret
                , måned = Skjema.fraMåned utdanningsskjema
                , onÅrChange = Tekst FraÅr >> OppsummeringEndret
                , år = Skjema.innholdTekstFelt FraÅr utdanningsskjema
                }
                |> DatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingFraÅr utdanningsskjema)
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
                    |> DatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingTilÅr utdanningsskjema)
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


viewBekreftOppsummering : Bool -> Html Msg
viewBekreftOppsummering skalViseSlett =
    Containers.knapper Kolonne
        [ Knapp.knapp OppsummeringBekreftet "Ja, det er riktig"
            |> Knapp.toHtml
        , Knapp.knapp BrukerVilEndreOppsummering "Nei, jeg vil endre"
            |> Knapp.toHtml
        , if skalViseSlett then
            Knapp.knapp VilSlettePåbegynt "Nei, jeg vil slette"
                |> Knapp.toHtml

          else
            text ""
        ]


lagUtdanningKnapper : List Utdanning -> List (Html Msg)
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
                Knapp.knapp (BrukerHarValgtUtdanningÅRedigere utdanning text) text
                    |> Knapp.toHtml
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
