module Utdanning.Seksjon exposing
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
import Cv.Utdanning as Utdanning exposing (Nivå(..), Utdanning)
import Dato exposing (Måned(..), TilDato(..), År)
import DebugStatus exposing (DebugStatus)
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.ManedKnapper as MånedKnapper
import FrontendModuler.Select as Select
import FrontendModuler.Textarea as Textarea
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import Task
import Utdanning.Skjema as Skjema exposing (Felt(..), UtdanningSkjema, ValidertUtdanningSkjema)



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , utdanningListe : List Utdanning
    , debugStatus : DebugStatus
    }


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
    | LagrerSkjema ValidertUtdanningSkjema
    | LagringFeilet Http.Error ValidertUtdanningSkjema
    | LeggTilFlereUtdanninger
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
    | OriginalOppsummeringBekreftet
    | OppsummeringEndret SkjemaEndring
    | OppsummeringSkjemaLagreknappTrykket
    | UtdanningSendtTilApi (Result Http.Error (List Utdanning))
    | AvbrytLagringOgTaMegTilIntro
    | ViewportSatt (Result Dom.Error ())
    | FokusSatt (Result Dom.Error ())
    | ErrorLogget
    | StartÅSkrive
    | FullførMelding


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

                LeggTilFlereUtdanninger ->
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
            case model.aktivSamtale of
                Intro _ ->
                    ( model.utdanningListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                LeggTilFlereUtdanninger ->
                    ( model.utdanningListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
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
                    let
                        trimmetBeskrivelseinfo =
                            { beskrivelseinfo | beskrivelse = String.trim beskrivelseinfo.beskrivelse }
                    in
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ trimmetBeskrivelseinfo.beskrivelse ]) (RegistrereFraMåned (forrigeTilFradatoInfo trimmetBeskrivelseinfo))
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

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
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, jeg går på studiet" ])
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
                    IkkeFerdig
                        ( utdanningskjema
                            |> Skjema.tilUvalidertSkjema
                            |> EndrerOppsummering
                            |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

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

        OriginalOppsummeringBekreftet ->
            case model.aktivSamtale of
                Oppsummering ferdigskjema ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Ja, informasjonen er riktig" ]) (LagrerSkjema ferdigskjema)
                        , Cmd.batch
                            [ Api.postUtdanning UtdanningSendtTilApi ferdigskjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                LeggTilFlereUtdanninger ->
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
                            IkkeFerdig
                                ( nesteSamtaleSteg model (Melding.svar [ "Bekreft" ]) (LagrerSkjema validertSkjema)
                                , Cmd.batch
                                    [ postEllerPutUtdanning UtdanningSendtTilApi validertSkjema
                                    , lagtTilSpørsmålCmd model.debugStatus
                                    ]
                                )

                        Nothing ->
                            IkkeFerdig
                                ( skjema
                                    |> Skjema.gjørAlleFeilmeldingerSynlig
                                    |> EndrerOppsummering
                                    |> oppdaterSamtaleSteg model
                                , Cmd.none
                                )

                LagringFeilet _ feiletskjema ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Bekreft" ]) (LagrerSkjema feiletskjema)
                        , Cmd.batch
                            [ postEllerPutUtdanning UtdanningSendtTilApi feiletskjema
                            , lagtTilSpørsmålCmd model.debugStatus
                            ]
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        UtdanningSendtTilApi result ->
            case model.aktivSamtale of
                LagrerSkjema skjema ->
                    case result of
                        Ok value ->
                            ( nesteSamtaleStegUtenMelding { model | utdanningListe = value } LeggTilFlereUtdanninger, lagtTilSpørsmålCmd model.debugStatus )
                                |> IkkeFerdig

                        Err error ->
                            ( nesteSamtaleStegUtenMelding model (LagringFeilet error skjema)
                            , skjema
                                |> Skjema.encode
                                |> Api.logErrorWithRequestBody ErrorLogget "Lagre utdanning" error
                            )
                                |> IkkeFerdig

                Oppsummering skjema ->
                    case result of
                        Ok value ->
                            ( nesteSamtaleStegUtenMelding { model | utdanningListe = value } LeggTilFlereUtdanninger, lagtTilSpørsmålCmd model.debugStatus )
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

        AvbrytLagringOgTaMegTilIntro ->
            ( nesteSamtaleSteg model (Melding.svar [ "Avbryt lagring" ]) (Intro model.utdanningListe), lagtTilSpørsmålCmd model.debugStatus )
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

        FullførMelding ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.fullførMelding
                |> updateEtterFullførtMelding model

        ViewportSatt _ ->
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
            "Høyere Utdanning (1-4 år)"

        HøyereUtdanning4pluss ->
            "Høyere Utdanning (mer enn 4 år)"

        Phd ->
            "PhD"


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


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring utdanningsListe ->
                    Ferdig utdanningsListe ferdigAnimertSamtale

                _ ->
                    ( Model
                        { model
                            | seksjonsMeldingsLogg =
                                nyMeldingsLogg
                        }
                    , Cmd.batch
                        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        , settFokus model.aktivSamtale
                        ]
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


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    inputId
        |> inputIdTilString
        |> Dom.focus
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

        "Phd" ->
            Just Phd

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


utdanningslisteTilString : List Utdanning -> List String
utdanningslisteTilString utdanninger =
    utdanninger
        |> List.map utdanningTilStrings
        |> List.intersperse [ Melding.tomLinje ]
        |> List.concat


utdanningTilStrings : Utdanning -> List String
utdanningTilStrings utdanning =
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
                [ Melding.spørsmål [ "Har du utdanning du vil legge inn i CV-en din?" ]
                , Melding.spørsmål [ "Husk at du kan legge inn grunnskole og videregående." ]
                ]

            else
                [ Melding.spørsmål [ "Nå skal vi legge til utdanning. Vi ser at du allerede har lagt inn disse utdanningene: " ]
                , Melding.spørsmål (utdanningslisteTilString utdanninger)
                , Melding.spørsmål [ "Vil du legge inn flere utdanninger? " ]
                ]

        VelgEnUtdanningÅRedigere ->
            [ Melding.spørsmål [ "Hvilken registrerte utdanning ønsker du å redigere?" ] ]

        RegistrerNivå ->
            [ Melding.spørsmål [ "Legg inn én utdanning av gangen." ]
            , Melding.spørsmål [ "Her kommer et lite tips. Hvis du har en bachelorgrad, velg høyere utdanning 1-4 år. Har du en mastergrad, velg høyere utdanning 4+ år." ]
            , Melding.spørsmål [ "Hvilket nivå er det på utdanningen du skal legge inn?" ]
            ]

        RegistrerSkole skoleinfo ->
            case skoleinfo.forrige of
                Fagskole ->
                    [ Melding.spørsmål [ "Merk at du kan legge til fagbrev/svennebrev eller mesterbrev mot slutten av samtalen, om du har det" ]
                    , Melding.spørsmål [ "Hvilken skole gikk du på?" ]
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
            [ Melding.spørsmål [ "Hva er navnet på graden din, og hvilken utdanningsretning gikk du?" ] ]

        RegistrerBeskrivelse _ ->
            [ Melding.spørsmål [ "Fortell om utdanningen du tok, hva lærte du? Har du fordypning i noen fag? Skriv om det." ] ]

        RegistrereFraMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned begynte du på utdanningen din?" ]
            , Melding.spørsmål [ "De fleste studiene i Norge begynner i august." ]
            ]

        RegistrereFraÅr _ ->
            [ Melding.spørsmål [ "Hvilket år begynte du på utdanningen din?" ] ]

        RegistrereNåværende _ ->
            [ Melding.spørsmål [ "Går du fremdeles på studiet?" ] ]

        RegistrereTilMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned fullførte du utdanningen din?" ]
            , Melding.spørsmål [ "De fleste studier i Norge er ferdig i juni." ]
            ]

        RegistrereTilÅr _ ->
            [ Melding.spørsmål [ "Hvilket år fullførte du utdanningen din?" ] ]

        Oppsummering validertSkjema ->
            let
                utdanningsskjema =
                    Skjema.tilUvalidertSkjema validertSkjema
            in
            [ Melding.spørsmål
                [ "Du har lagt inn dette:"
                , Melding.tomLinje
                , Dato.periodeTilString (Skjema.fraMåned utdanningsskjema) (Skjema.fraÅrValidert validertSkjema) (Skjema.tilDatoValidert validertSkjema)
                , Melding.tomLinje
                , "Utdanningsnivå: " ++ nivåToString (Skjema.nivå utdanningsskjema)
                , "Grad og studieretning: " ++ Skjema.innholdTekstFelt Utdanningsretning utdanningsskjema
                , "Skole/studiested: " ++ Skjema.innholdTekstFelt Studiested utdanningsskjema
                , Melding.tomLinje
                , "Beskrivelse:"
                , Skjema.innholdTekstFelt Beskrivelse utdanningsskjema
                , Melding.tomLinje
                , "Er informasjonen riktig?"
                ]
            ]

        EndrerOppsummering _ ->
            [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

        LeggTilFlereUtdanninger ->
            [ Melding.spørsmål [ "Så bra! Nå har du lagt inn en ny utdanning 👍" ]
            , Melding.spørsmål [ "Vil du legge inn flere utdanninger? " ]
            ]

        LagringFeilet _ _ ->
            [ Melding.spørsmål [ "Klarte ikke å lagre skjemaet. Mulig du ikke har internett, eller at du har skrevet noe i skjemaet som jeg ikke forventet. Vennligst se over skjemaet og forsøk på nytt" ] ]

        VenterPåAnimasjonFørFullføring _ ->
            [ Melding.spørsmål [ "Bra jobba! Da går vi videre." ] ]

        LagrerSkjema _ ->
            []



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
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
                            , "Jeg vil redigere det jeg har lagt inn"
                                |> Knapp.knapp (BrukerVilRedigereUtdanning "Jeg vil redigere det jeg har lagt inn")
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
                        , Knapp.knapp (BrukerVilRegistrereNivå Phd) (nivåToString Phd)
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
                                |> Input.toHtml
                            ]
                        ]

                RegistrereNåværende _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerSvarerJaTilNaavarende "Ja, jeg går på studiet"
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
                                |> Input.toHtml
                            ]
                        ]

                Oppsummering _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerVilEndreOppsummering "Nei, jeg vil endre"
                            |> Knapp.toHtml
                        , Knapp.knapp OriginalOppsummeringBekreftet "Ja, informasjonen er riktig"
                            |> Knapp.toHtml
                        ]

                EndrerOppsummering utdanningsskjema ->
                    viewSkjema utdanningsskjema

                LeggTilFlereUtdanninger ->
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerVilRegistrereUtdanning "Ja, legg til en utdanning"
                            |> Knapp.toHtml
                        , Knapp.knapp OriginalOppsummeringBekreftet "Nei, jeg er ferdig"
                            |> Knapp.toHtml
                        , "Jeg vil redigere det jeg har lagt inn"
                            |> Knapp.knapp (BrukerVilRedigereUtdanning "Jeg vil redigere det jeg har lagt inn")
                            |> Knapp.toHtml
                        ]

                LagringFeilet _ _ ->
                    --                    Debug.todo "View når lagring feiler"
                    -- TODO: Test
                    text ""

                LagrerSkjema _ ->
                    div [] []

                VenterPåAnimasjonFørFullføring _ ->
                    div [] []

        MeldingerGjenstår ->
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
            |> Textarea.toHtml
        , div [ class "DatoInput-fra-til-rad" ]
            [ DatoInput.datoInput
                { label = "Fra"
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
                    { label = "Til"
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
            |> Checkbox.checkbox "Nåværende" (OppsummeringEndret NåværendeToggled)
            |> Checkbox.toHtml
        ]


lagUtdanningKnapper : List Utdanning -> List (Html Msg)
lagUtdanningKnapper utdanninger =
    utdanninger
        |> List.map
            (\utdanning ->
                let
                    text =
                        Maybe.withDefault "" (Utdanning.utdanningsretning utdanning)
                            ++ ", "
                            ++ Maybe.withDefault "" (Utdanning.studiested utdanning)
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
    , ( tilNivåKey HøyereUtdanning1til4, "Høyere Utdanning (1-4 år)" )
    , ( tilNivåKey HøyereUtdanning4pluss, "Høyere Utdanning (mer enn 4 år)" )
    , ( tilNivåKey Phd, "PhD" )
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

        Phd ->
            "Phd"


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
