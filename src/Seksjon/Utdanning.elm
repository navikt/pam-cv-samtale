module Seksjon.Utdanning exposing (Model, Msg, SamtaleStatus(..), init, meldingsLogg, update, viewBrukerInput)

import Api
import Browser.Dom as Dom
import Cv.Utdanning as Cv exposing (Nivå(..), Utdanning)
import Dato exposing (Dato)
import Feilmelding
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Input as Input
import FrontendModuler.InputInt as InputInt
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Select as Select
import FrontendModuler.Textarea as Textarea
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import Skjema.Utdanning as Skjema exposing (Felt(..))
import Task



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , utdanningListe : List Utdanning
    }


type Samtale
    = Intro (List Utdanning)
    | RegistrerNivå
    | RegistrerSkole SkoleInfo
    | RegistrerRetning RetningInfo
    | RegistrerBeskrivelse BeskrivelseInfo
    | RegistrereFraMåned FradatoInfo
    | RegistrereFraÅr FradatoInfo
    | RegistrereNavarende FradatoInfo
    | RegistrereTilMåned TildatoInfo
    | RegistrereTilÅr TildatoInfo
    | Oppsummering Skjema.UtdanningSkjema
    | EndrerOppsummering Skjema.UtdanningSkjema
    | OppsummeringLagret Skjema.UtdanningSkjema
    | LeggTilFlereUtdannelser Skjema.UtdanningSkjema
    | LeggTilUtdanningFeiletIApi Http.Error Skjema.UtdanningSkjema
    | VenterPåAnimasjonFørFullføring (List Utdanning)
    | AvsluttSeksjon Skjema.UtdanningSkjema


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


type alias FradatoInfo =
    { forrige : BeskrivelseInfo
    , fraMåned : Dato.Måned
    , fraÅr : String
    , navarende : Bool
    }


type alias TildatoInfo =
    { forrige : FradatoInfo
    , tilMåned : Dato.Måned
    , tilÅr : String
    }


type alias OppsummeringInfo =
    { nivå : Nivå
    , skole : String
    , retning : String
    , beskrivelse : String
    , fradato : Dato
    , navarende : Bool
    , tilDato : Maybe Dato
    }


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


forrigeTilRetningInfo : SkoleInfo -> RetningInfo
forrigeTilRetningInfo skole =
    { forrige = skole, retning = "" }


forrigeTilSkoleInfo : Nivå -> SkoleInfo
forrigeTilSkoleInfo nivå =
    { forrige = nivå, skole = "" }


forrigeTilBeskrivelseInfo : RetningInfo -> BeskrivelseInfo
forrigeTilBeskrivelseInfo retning =
    { forrige = retning, beskrivelse = "" }


forrigeTilFradatoInfo : BeskrivelseInfo -> FradatoInfo
forrigeTilFradatoInfo beskrivelse =
    { forrige = beskrivelse
    , fraMåned = Dato.Januar
    , fraÅr = ""
    , navarende = False
    }


forrigeTilTildatoInfo : FradatoInfo -> TildatoInfo
forrigeTilTildatoInfo fradatoInfo =
    { forrige = fradatoInfo
    , tilMåned = Dato.Januar
    , tilÅr = ""
    }


forrigeTilOppsummeringInfo : TildatoInfo -> Skjema.UtdanningSkjema
forrigeTilOppsummeringInfo tildatoInfo =
    Skjema.initManueltSkjema
        { nuskode = tildatoInfo.forrige.forrige.forrige.forrige.forrige
        , studiested = tildatoInfo.forrige.forrige.forrige.forrige.skole
        , utdanningsretning = tildatoInfo.forrige.forrige.forrige.retning
        , beskrivelse = tildatoInfo.forrige.forrige.beskrivelse
        , fradato = Dato.tilDato (tildatoInfo.forrige.fraÅr ++ "-" ++ (tildatoInfo.forrige.fraMåned |> Dato.månedTilString))
        , tildato =
            if tildatoInfo.forrige.navarende then
                Nothing

            else
                Just (Dato.tilDato (tildatoInfo.tilÅr ++ "-" ++ (tildatoInfo.tilMåned |> Dato.månedTilString)))
        , navarende = tildatoInfo.forrige.navarende
        }



--- UPDATE ---


type Msg
    = BrukerVilRegistrereUtdanning
    | GåTilArbeidserfaring String
    | BekreftAlleredeRegistrert
    | BrukerVilRegistrereNivå Nivå
    | BrukerVilRegistrereSkole
    | OppdaterSkole String
    | BrukerVilRegistrereRetning
    | OppdaterRetning String
    | BrukerVilRegistrereBeskrivelse
    | OppdaterBeskrivelse String
    | BrukerVilRegistrereFraMåned FradatoInfo
    | BrukerTrykketFraMånedKnapp FradatoInfo
    | OppdaterFraÅr FradatoInfo String
    | BrukerVilRegistrereNaavarende
    | BrukerSvarerJaTilNaavarende
    | BrukerSvarerNeiTilNaavarende FradatoInfo
    | BrukerTrykketTilMånedKnapp TildatoInfo
    | OppdaterTilÅr TildatoInfo String
    | BrukerVilGåTilOppsummering
    | BrukerVilEndreOppsummering
    | OriginalOppsummeringBekreftet
    | OppsummeringEndret Skjema.Felt String
    | OppsummeringBoolEndret Skjema.Felt
    | OppsummeringSkjemaLagreknappTrykket
    | SkjemaOppdatert SkjemaEndring
    | UtdanningSendtTilApi (Result Http.Error (List Utdanning))
    | AvbrytLagringOgTaMegTilIntro
    | ViewportSatt (Result Dom.Error ())
    | ErrorLogget (Result Http.Error ())
    | StartÅSkrive
    | FullførMelding


type SkjemaEndring
    = NivåEndret String
    | BeskrivelseEndret String
    | SkoleEndret String
    | FraMånedEndret String
    | FraÅrEndret String
    | TilMånedEndret String
    | TilÅrEndret String
    | NavarendeEndret Bool
    | RetningEndret String


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        BrukerVilRegistrereUtdanning ->
            case model.aktivSamtale of
                Intro utdanningListe ->
                    if List.isEmpty utdanningListe then
                        IkkeFerdig
                            ( nesteSamtaleSteg model (Melding.svar [ "Jeg vil registrere utdannning" ]) RegistrerNivå
                            , lagtTilSpørsmålCmd
                            )

                    else
                        IkkeFerdig
                            ( nesteSamtaleSteg model (Melding.svar [ "Jeg vil legge til flere utdannelser" ]) RegistrerNivå
                            , lagtTilSpørsmålCmd
                            )

                LeggTilFlereUtdannelser _ ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Legg til flere" ]) RegistrerNivå
                        , lagtTilSpørsmålCmd
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        GåTilArbeidserfaring knappeTekst ->
            case model.aktivSamtale of
                Intro _ ->
                    ( model.utdanningListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                LeggTilFlereUtdannelser _ ->
                    ( model.utdanningListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                _ ->
                    ( model.utdanningListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ knappeTekst ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

        BrukerVilRegistrereNivå nivå ->
            case model.aktivSamtale of
                RegistrerNivå ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ nivåToString nivå ]) (RegistrerSkole (forrigeTilSkoleInfo nivå))
                        , lagtTilSpørsmålCmd
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereSkole ->
            case model.aktivSamtale of
                RegistrerSkole skoleinfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ skoleinfo.skole ]) (RegistrerRetning (forrigeTilRetningInfo skoleinfo))
                        , lagtTilSpørsmålCmd
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereRetning ->
            case model.aktivSamtale of
                RegistrerRetning retninginfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ retninginfo.retning ]) (RegistrerBeskrivelse (forrigeTilBeskrivelseInfo retninginfo))
                        , lagtTilSpørsmålCmd
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereBeskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse beskrivelseinfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ beskrivelseinfo.beskrivelse ]) (RegistrereFraMåned (forrigeTilFradatoInfo beskrivelseinfo))
                        , lagtTilSpørsmålCmd
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereFraMåned fraDatoInfo ->
            ( RegistrereFraMåned fraDatoInfo
                |> nesteSamtaleSteg model (Melding.svar [ fraDatoInfo.forrige.beskrivelse ])
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        BrukerTrykketFraMånedKnapp fraDatoInfo ->
            ( RegistrereFraÅr fraDatoInfo
                |> nesteSamtaleSteg model
                    (Melding.svar
                        [ fraDatoInfo.fraMåned
                            |> Dato.månedTilString
                        ]
                    )
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        OppdaterFraÅr fraDatoInfo string ->
            ( Model
                { model
                    | aktivSamtale =
                        RegistrereFraÅr { fraDatoInfo | fraÅr = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrereNaavarende ->
            case model.aktivSamtale of
                RegistrereFraÅr datoInfo ->
                    ( RegistrereNavarende datoInfo
                        |> nesteSamtaleSteg model (Melding.svar [ datoInfo.fraÅr ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerSvarerJaTilNaavarende ->
            case model.aktivSamtale of
                RegistrereNavarende datoInfo ->
                    ( datoInfo
                        |> forrigeTilTildatoInfo
                        |> setNavarendeTilTrue
                        |> forrigeTilOppsummeringInfo
                        |> Oppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja" ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerSvarerNeiTilNaavarende fraDatoInfo ->
            ( fraDatoInfo
                |> forrigeTilTildatoInfo
                |> RegistrereTilMåned
                |> nesteSamtaleSteg model (Melding.svar [ "Nei" ])
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        BrukerTrykketTilMånedKnapp tilDatoInfo ->
            ( RegistrereTilÅr tilDatoInfo
                |> nesteSamtaleSteg model
                    (Melding.svar
                        [ tilDatoInfo.tilMåned
                            |> Dato.månedTilString
                        ]
                    )
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        OppdaterTilÅr tilDatoInfo string ->
            ( Model
                { model
                    | aktivSamtale =
                        RegistrereTilÅr { tilDatoInfo | tilÅr = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilGåTilOppsummering ->
            case model.aktivSamtale of
                RegistrereTilÅr tilDatoInfo ->
                    ( tilDatoInfo
                        |> forrigeTilOppsummeringInfo
                        |> Oppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ tilDatoInfo.tilÅr ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        BrukerVilEndreOppsummering ->
            case model.aktivSamtale of
                Oppsummering utdanningskjema ->
                    IkkeFerdig ( nesteSamtaleSteg model (Melding.svar [ "Jeg vil endre" ]) (EndrerOppsummering utdanningskjema), lagtTilSpørsmålCmd )

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

        SkjemaOppdatert skjemaendring ->
            case model.aktivSamtale of
                EndrerOppsummering skjemaendringinfo ->
                    IkkeFerdig ( oppdaterSamtaleSteg model (EndrerOppsummering (oppdaterSkjema skjemaendring skjemaendringinfo)), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OriginalOppsummeringBekreftet ->
            case model.aktivSamtale of
                Oppsummering ferdigskjema ->
                    IkkeFerdig ( nesteSamtaleSteg model (Melding.svar [ "Bekreft" ]) (OppsummeringLagret ferdigskjema), Cmd.batch [ Api.postUtdanning UtdanningSendtTilApi ferdigskjema, lagtTilSpørsmålCmd ] )

                LeggTilFlereUtdannelser ferdigskjema ->
                    ( VenterPåAnimasjonFørFullføring model.utdanningListe
                        |> nesteSamtaleSteg model (Melding.svar [ "Ferdig med å legge til utdannelser" ])
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppsummeringSkjemaLagreknappTrykket ->
            case model.aktivSamtale of
                EndrerOppsummering ferdigskjema ->
                    IkkeFerdig ( nesteSamtaleSteg model (Melding.svar [ "Bekreft" ]) (OppsummeringLagret ferdigskjema), Cmd.batch [ Api.postUtdanning UtdanningSendtTilApi ferdigskjema, lagtTilSpørsmålCmd ] )

                LeggTilUtdanningFeiletIApi _ feiletskjema ->
                    IkkeFerdig ( nesteSamtaleSteg model (Melding.svar [ "Bekreft" ]) (OppsummeringLagret feiletskjema), Cmd.batch [ Api.postUtdanning UtdanningSendtTilApi feiletskjema, lagtTilSpørsmålCmd ] )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        UtdanningSendtTilApi result ->
            case model.aktivSamtale of
                OppsummeringLagret skjema ->
                    case result of
                        Ok value ->
                            ( nesteSamtaleStegUtenMelding { model | utdanningListe = value } (LeggTilFlereUtdannelser skjema), lagtTilSpørsmålCmd )
                                |> IkkeFerdig

                        Err error ->
                            ( nesteSamtaleStegUtenMelding model (LeggTilUtdanningFeiletIApi error skjema), logFeilmelding error "API kall" )
                                |> IkkeFerdig

                Oppsummering skjema ->
                    case result of
                        Ok value ->
                            ( nesteSamtaleStegUtenMelding { model | utdanningListe = value } (LeggTilFlereUtdannelser skjema), lagtTilSpørsmålCmd )
                                |> IkkeFerdig

                        Err error ->
                            ( nesteSamtaleStegUtenMelding model (LeggTilUtdanningFeiletIApi error skjema), logFeilmelding error "API kall" )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        AvbrytLagringOgTaMegTilIntro ->
            ( nesteSamtaleSteg model (Melding.svar [ "Avbryt lagring" ]) (Intro model.utdanningListe), lagtTilSpørsmålCmd )
                |> IkkeFerdig

        StartÅSkrive ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        MeldingsLogg.startÅSkrive model.seksjonsMeldingsLogg
                }
            , Cmd.batch
                [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                , Process.sleep (MeldingsLogg.nesteMeldingToString model.seksjonsMeldingsLogg * 1000.0)
                    |> Task.perform (\_ -> FullførMelding)
                ]
            )
                |> IkkeFerdig

        FullførMelding ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.fullførMelding
                |> updateEtterFullførtMelding model

        _ ->
            IkkeFerdig ( Model model, Cmd.none )


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


fullførSeksjonHvisMeldingsloggErFerdig : ModelInfo -> List Utdanning -> SamtaleStatus
fullførSeksjonHvisMeldingsloggErFerdig modelInfo utdanningListe =
    case MeldingsLogg.ferdigAnimert modelInfo.seksjonsMeldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            Ferdig utdanningListe ferdigAnimertMeldingsLogg

        MeldingerGjenstår ->
            ( Model { modelInfo | aktivSamtale = VenterPåAnimasjonFørFullføring utdanningListe }, Cmd.none )
                |> IkkeFerdig


lagtTilSpørsmålCmd : Cmd Msg
lagtTilSpørsmålCmd =
    Cmd.batch
        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
        , Process.sleep 200
            |> Task.perform (\_ -> StartÅSkrive)
        ]


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError ErrorLogget)
        |> Maybe.withDefault Cmd.none


oppdaterSkjema : SkjemaEndring -> Skjema.UtdanningSkjema -> Skjema.UtdanningSkjema
oppdaterSkjema endring skjema =
    case endring of
        NivåEndret nivåstring ->
            case stringToNivå nivåstring of
                Just nivå ->
                    Skjema.oppdaterNuskode nivå skjema

                Nothing ->
                    skjema

        BeskrivelseEndret string ->
            Skjema.oppdaterBeskrivelse string skjema

        SkoleEndret string ->
            Skjema.oppdaterStudiested string skjema

        FraMånedEndret string ->
            Skjema.oppdaterFraMåned string skjema

        FraÅrEndret string ->
            Skjema.oppdaterFraÅr string skjema

        TilMånedEndret string ->
            Skjema.oppdaterTilMåned string skjema

        TilÅrEndret string ->
            Skjema.oppdaterTilÅr string skjema

        NavarendeEndret bool ->
            Skjema.oppdaterNavarende bool skjema

        RetningEndret string ->
            Skjema.oppdaterUtdanningsretning string skjema


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


setNavarendeTilTrue : TildatoInfo -> TildatoInfo
setNavarendeTilTrue info =
    { info | forrige = setTilTrue info.forrige }


setTilTrue : FradatoInfo -> FradatoInfo
setTilTrue fraDatoInfo =
    { fraDatoInfo | navarende = True }


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


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg utdanningSeksjon =
    case utdanningSeksjon of
        Intro utdannelseListe ->
            if List.isEmpty utdannelseListe then
                [ Melding.spørsmål
                    [ "Nå skal vi legge til din utdanning" ]
                ]

            else
                [ Melding.spørsmål [ "Nå skal vi legge til utdanning. Vi ser at du allerede har lagt inn utdannelser tidligere." ]
                , Melding.spørsmål
                    (List.map
                        (\el ->
                            if Cv.navarende el |> Maybe.withDefault False then
                                (Cv.fradato el |> Maybe.withDefault "")
                                    ++ ": "
                                    ++ (Cv.studiested el |> Maybe.withDefault "")
                                    ++ ", "
                                    ++ " "
                                    ++ (Cv.utdanningsretning el |> Maybe.withDefault "")

                            else
                                (Cv.fradato el |> Maybe.withDefault "")
                                    ++ " til "
                                    ++ (Cv.tildato el |> Maybe.withDefault "")
                                    ++ ": "
                                    ++ (Cv.studiested el |> Maybe.withDefault "")
                                    ++ ", "
                                    ++ " "
                                    ++ (Cv.utdanningsretning el |> Maybe.withDefault "")
                        )
                        utdannelseListe
                    )
                , Melding.spørsmål [ "Har du flere utdannelser du ønsker å ha med på CVen din?" ]
                ]

        RegistrerNivå ->
            [ Melding.spørsmål
                [ "Hvilket nivå er utdanningen på?" ]
            ]

        RegistrerSkole _ ->
            [ Melding.spørsmål
                [ " Hvilken skole gikk du på? " ]
            ]

        RegistrerRetning _ ->
            [ Melding.spørsmål
                [ "Hvilken studieretning gikk du?" ]
            ]

        RegistrerBeskrivelse _ ->
            [ Melding.spørsmål
                [ " Kan du beskrive denne utdannelsen din for meg? " ]
            ]

        RegistrereFraMåned _ ->
            [ Melding.spørsmål
                [ "Hvilken måned begynte du utdannelsen din? " ]
            , Melding.spørsmål [ "Hint! De fleste utdannelser begynner i August" ]
            ]

        RegistrereFraÅr _ ->
            [ Melding.spørsmål [ "Hvilket år begynte du på denne utdannelsen?" ] ]

        RegistrereNavarende _ ->
            [ Melding.spørsmål [ "Er dette et pågående studie?" ] ]

        RegistrereTilMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned ble du ferdig med utdannelsen?" ]
            , Melding.spørsmål [ "Hint! De fleste utdannlser slutter i Juni" ]
            ]

        RegistrereTilÅr _ ->
            [ Melding.spørsmål [ "Når ble du ferdig med utdannelsen?" ] ]

        Oppsummering utdanningsskjema ->
            [ Melding.spørsmål
                [ "Her har du en oppsummering av utdannelsen du la inn:"
                , "Nivå: " ++ nivåToString (Skjema.nuskode utdanningsskjema)
                , "Skole: " ++ Skjema.studiested utdanningsskjema
                , "Retning: " ++ Skjema.utdanningsretning utdanningsskjema
                , "Beskrivelse: " ++ Skjema.beskrivelse utdanningsskjema
                , "Fra: " ++ hentFraDato utdanningsskjema
                , if Skjema.navarende utdanningsskjema == True then
                    "Nåværende studie"

                  else
                    hentTilDato utdanningsskjema
                ]
            ]

        EndrerOppsummering _ ->
            [ Melding.spørsmål
                [ "Ok! Vennligst skriv inn riktig informasjon i feltene under: " ]
            ]

        LeggTilFlereUtdannelser _ ->
            [ Melding.spørsmål [ "Har du flere utdannelser du ønsker å legge inn?" ]
            ]

        LeggTilUtdanningFeiletIApi _ _ ->
            [ Melding.spørsmål [ "Klarte ikke å lagre skjemaet. Mulig du ikke har internett, eller at du har skrevet noe i skjemaet som jeg ikke forventet. Vennligst se over skjemaet og forsøk på nytt" ] ]

        VenterPåAnimasjonFørFullføring _ ->
            [ Melding.spørsmål
                [ "Da var vi ferdige med utdanningen og går videre!"
                , "Husk at du kan alltid komme tilbake og legge til flere!"
                ]
            ]

        _ ->
            []


hentFraDato : Skjema.UtdanningSkjema -> String
hentFraDato skjema =
    let
        år =
            skjema |> Skjema.fraDato |> Dato.år |> String.fromInt

        maaned =
            skjema |> Skjema.fraDato |> Dato.måned |> Dato.månedTilString
    in
    maaned
        ++ " "
        ++ år


hentTilDato : Skjema.UtdanningSkjema -> String
hentTilDato skjema =
    if Skjema.navarende skjema == True then
        ""

    else
        let
            dato =
                skjema
                    |> Skjema.tilDato
                    |> Maybe.withDefault (Skjema.fraDato skjema)
        in
        "Til: " ++ (dato |> Dato.måned |> Dato.månedTilString) ++ " " ++ (dato |> Dato.år |> String.fromInt)



-- View --


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                Intro _ ->
                    if List.isEmpty model.utdanningListe then
                        div [ class "skjema-wrapper" ]
                            [ div [ class "sjema" ]
                                [ div [ class "inputkolonne" ]
                                    [ div [ class "inputkolonne-innhold" ]
                                        [ Knapp.knapp BrukerVilRegistrereUtdanning "Jeg vil registrere utdanning"
                                            |> Knapp.withClass Knapp.LeggeTilUtdannelseKnapp
                                            |> Knapp.toHtml
                                        , "Jeg har ingen utdanning"
                                            |> Knapp.knapp (GåTilArbeidserfaring "Jeg har ingen utdanning")
                                            |> Knapp.withClass Knapp.LeggeTilUtdannelseKnapp
                                            |> Knapp.toHtml
                                        ]
                                    ]
                                ]
                            ]

                    else
                        div [ class "inputkolonne" ]
                            [ div [ class "knapperad-wrapper" ]
                                [ div [ class "inputkolonne" ]
                                    [ Knapp.knapp BrukerVilRegistrereUtdanning "Jeg vil legge til flere utdannelser"
                                        |> Knapp.withClass Knapp.LeggeTilUtdannelseKnapp
                                        |> Knapp.toHtml
                                    ]
                                , div [ class "inputkolonne" ]
                                    [ "Jeg er ferdig med å legge til utdannelser"
                                        |> Knapp.knapp (GåTilArbeidserfaring "Jeg er ferdig med å legge til utdannelser")
                                        |> Knapp.withClass Knapp.LeggeTilUtdannelseKnapp
                                        |> Knapp.toHtml
                                    ]
                                ]
                            ]

                RegistrerNivå ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "inputkolonne" ]
                            [ Knapp.knapp (BrukerVilRegistrereNivå Grunnskole) (nivåToString Grunnskole)
                                |> Knapp.withClass Knapp.UtdanningsNivåKnapp
                                |> Knapp.toHtml
                            ]
                        , div [ class "inputkolonne" ]
                            [ Knapp.knapp (BrukerVilRegistrereNivå VideregåendeYrkesskole) (nivåToString VideregåendeYrkesskole)
                                |> Knapp.withClass Knapp.UtdanningsNivåKnapp
                                |> Knapp.toHtml
                            ]
                        , div [ class "inputkolonne" ]
                            [ Knapp.knapp (BrukerVilRegistrereNivå Fagskole) (nivåToString Fagskole)
                                |> Knapp.withClass Knapp.UtdanningsNivåKnapp
                                |> Knapp.toHtml
                            ]
                        , div [ class "inputkolonne" ]
                            [ Knapp.knapp (BrukerVilRegistrereNivå Folkehøyskole) (nivåToString Folkehøyskole)
                                |> Knapp.withClass Knapp.UtdanningsNivåKnapp
                                |> Knapp.toHtml
                            ]
                        , div [ class "inputkolonne" ]
                            [ Knapp.knapp (BrukerVilRegistrereNivå HøyereUtdanning1til4) (nivåToString HøyereUtdanning1til4)
                                |> Knapp.withClass Knapp.UtdanningsNivåKnapp
                                |> Knapp.toHtml
                            ]
                        , div [ class "inputkolonne" ]
                            [ Knapp.knapp (BrukerVilRegistrereNivå HøyereUtdanning4pluss) (nivåToString HøyereUtdanning4pluss)
                                |> Knapp.withClass Knapp.UtdanningsNivåKnapp
                                |> Knapp.toHtml
                            ]
                        , div [ class "inputkolonne" ]
                            [ Knapp.knapp (BrukerVilRegistrereNivå Phd) (nivåToString Phd)
                                |> Knapp.withClass Knapp.UtdanningsNivåKnapp
                                |> Knapp.toHtml
                            ]
                        ]

                RegistrerSkole skoleinfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ skoleinfo.skole |> Input.input { msg = OppdaterSkole, label = "" } |> Input.toHtml
                            , Knapp.knapp BrukerVilRegistrereSkole "Lagre"
                                |> (if skoleinfo.skole /= "" then
                                        Knapp.withEnabled Knapp.Enabled

                                    else
                                        Knapp.withEnabled Knapp.Disabled
                                   )
                                |> Knapp.toHtml
                            ]
                        ]

                RegistrerRetning retningsinfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ retningsinfo.retning |> Input.input { msg = OppdaterRetning, label = "" } |> Input.toHtml
                            , Knapp.knapp BrukerVilRegistrereRetning "Lagre"
                                |> (if retningsinfo.retning /= "" then
                                        Knapp.withEnabled Knapp.Enabled

                                    else
                                        Knapp.withEnabled Knapp.Disabled
                                   )
                                |> Knapp.toHtml
                            ]
                        ]

                RegistrerBeskrivelse beskrivelseinfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ beskrivelseinfo.beskrivelse |> Textarea.textarea { msg = OppdaterBeskrivelse, label = "" } |> Textarea.toHtml
                            , Knapp.knapp BrukerVilRegistrereBeskrivelse "Lagre"
                                |> (if beskrivelseinfo.beskrivelse /= "" then
                                        Knapp.withEnabled Knapp.Enabled

                                    else
                                        Knapp.withEnabled Knapp.Disabled
                                   )
                                |> Knapp.toHtml
                            ]
                        ]

                RegistrereFraMåned fraDatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ div [ class "knapperad-wrapper" ]
                                    [ Dato.Januar
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.Februar
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.Mars
                                        |> lagFraMånedKnapp fraDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.April
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.Mai
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.Juni
                                        |> lagFraMånedKnapp fraDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.Juli
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.August
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.September
                                        |> lagFraMånedKnapp fraDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.Oktober
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.November
                                        |> lagFraMånedKnapp fraDatoInfo
                                    , Dato.Desember
                                        |> lagFraMånedKnapp fraDatoInfo
                                    ]
                                ]
                            ]
                        ]

                RegistrereFraÅr fraDatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema-int" ]
                            [ fraDatoInfo.fraÅr
                                |> InputInt.input { label = "", msg = OppdaterFraÅr fraDatoInfo }
                                |> InputInt.toHtml
                            , BrukerVilRegistrereNaavarende
                                |> lagÅrInputKnapp "Lagre" fraDatoInfo.fraÅr
                            ]
                        ]

                RegistrereNavarende fraDatoInfo ->
                    div []
                        [ div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ BrukerSvarerJaTilNaavarende
                                    |> lagMessageKnapp "Ja"
                                , fraDatoInfo
                                    |> BrukerSvarerNeiTilNaavarende
                                    |> lagMessageKnapp "Nei"
                                ]
                            ]
                        ]

                RegistrereTilMåned tilDatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ div [ class "knapperad-wrapper" ]
                                    [ Dato.Januar
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.Februar
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.Mars
                                        |> lagTilMånedKnapp tilDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.April
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.Mai
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.Juni
                                        |> lagTilMånedKnapp tilDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.Juli
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.August
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.September
                                        |> lagTilMånedKnapp tilDatoInfo
                                    ]
                                , div [ class "knapperad-wrapper" ]
                                    [ Dato.Oktober
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.November
                                        |> lagTilMånedKnapp tilDatoInfo
                                    , Dato.Desember
                                        |> lagTilMånedKnapp tilDatoInfo
                                    ]
                                ]
                            ]
                        ]

                RegistrereTilÅr tilDatoInfo ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema-int" ]
                            [ tilDatoInfo.tilÅr
                                |> InputInt.input { label = "", msg = OppdaterTilÅr tilDatoInfo }
                                |> InputInt.toHtml
                            , BrukerVilGåTilOppsummering
                                |> lagÅrInputKnapp "Lagre" tilDatoInfo.tilÅr
                            ]
                        ]

                Oppsummering _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputrad" ]
                                [ Knapp.knapp BrukerVilEndreOppsummering "Endre"
                                    |> Knapp.toHtml
                                , Knapp.knapp OriginalOppsummeringBekreftet "Bekreft"
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                EndrerOppsummering utdanningsskjema ->
                    endreSkjema model utdanningsskjema

                LeggTilFlereUtdannelser _ ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp BrukerVilRegistrereUtdanning "Legg til flere"
                                    |> Knapp.withClass Knapp.UtdanningsNivåKnapp
                                    |> Knapp.toHtml
                                ]
                            , div [ class "inputkolonne" ]
                                [ Knapp.knapp OriginalOppsummeringBekreftet "Ferdig med å legge til utdannelser"
                                    |> Knapp.withClass Knapp.UtdanningsNivåKnapp
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                LeggTilUtdanningFeiletIApi _ utdanningSkjema ->
                    endreSkjema model utdanningSkjema

                _ ->
                    text ""

        MeldingerGjenstår ->
            text ""


endreSkjema : ModelInfo -> Skjema.UtdanningSkjema -> Html Msg
endreSkjema model utdanningsskjema =
    div [ class "skjema-wrapper" ]
        [ div [ class "skjema" ]
            [ Select.select "Nivå: " (NivåEndret >> SkjemaOppdatert) selectNivåListe
                |> Select.toHtml
            , utdanningsskjema
                |> Skjema.studiested
                |> Input.input { label = "Studiested", msg = SkoleEndret >> SkjemaOppdatert }
                |> Input.toHtml
            , utdanningsskjema
                |> Skjema.utdanningsretning
                |> Input.input { label = "Retning", msg = RetningEndret >> SkjemaOppdatert }
                |> Input.toHtml
            , utdanningsskjema
                |> Skjema.beskrivelse
                |> Input.input { label = "Beskrivelse", msg = BeskrivelseEndret >> SkjemaOppdatert }
                |> Input.toHtml
            , Select.select "Måned"
                (FraMånedEndret >> SkjemaOppdatert)
                [ ( "Januar", "Januar" )
                , ( "Februar", "Februar" )
                , ( "Mars", "Mars" )
                , ( "April", "April" )
                , ( "Mai", "Mai" )
                , ( "Juni", "Juni" )
                , ( "Juli", "Juli" )
                , ( "August", "August" )
                , ( "September", "September" )
                , ( "Oktober", "Oktober" )
                , ( "November", "November" )
                , ( "Desember", "Desember" )
                ]
                |> Select.withSelected (utdanningsskjema |> Skjema.fraDato |> Dato.måned |> Dato.månedTilString)
                |> Select.toHtml
            , utdanningsskjema
                |> Skjema.fraDato
                |> Dato.år
                |> String.fromInt
                |> (\string ->
                        if string == "0" then
                            ""

                        else
                            string
                   )
                |> Input.input { label = "År", msg = FraÅrEndret >> SkjemaOppdatert }
                |> Input.toHtml
            , utdanningsskjema
                |> Skjema.navarende
                |> Checkbox.checkbox "Nåværende" (SkjemaOppdatert (NavarendeEndret (Skjema.navarende (Skjema.toggleBool utdanningsskjema Skjema.Navarende))))
                |> Checkbox.toHtml
            , if Skjema.navarende utdanningsskjema == True then
                text ""

              else
                div []
                    [ Select.select "Måned"
                        (TilMånedEndret >> SkjemaOppdatert)
                        [ ( "Januar", "Januar" )
                        , ( "Februar", "Februar" )
                        , ( "Mars", "Mars" )
                        , ( "April", "April" )
                        , ( "Mai", "Mai" )
                        , ( "Juni", "Juni" )
                        , ( "Juli", "Juli" )
                        , ( "August", "August" )
                        , ( "September", "September" )
                        , ( "Oktober", "Oktober" )
                        , ( "November", "November" )
                        , ( "Desember", "Desember" )
                        ]
                        |> Select.withSelected
                            (utdanningsskjema
                                |> Skjema.tilDato
                                |> Maybe.withDefault (Skjema.fraDato utdanningsskjema)
                                |> Dato.måned
                                |> Dato.månedTilString
                            )
                        |> Select.toHtml
                    , case Skjema.tilDato utdanningsskjema of
                        Just dato ->
                            dato
                                |> Dato.år
                                |> String.fromInt
                                |> (\string ->
                                        if string == "0" then
                                            ""

                                        else
                                            string
                                   )
                                |> Input.input { label = "År", msg = TilÅrEndret >> SkjemaOppdatert }
                                |> Input.toHtml

                        Nothing ->
                            text ""
                    ]
            ]
        , case model.aktivSamtale of
            EndrerOppsummering _ ->
                div [ class "skjema" ]
                    [ div [ class "skjema-wrapper" ]
                        [ div [ class "inputkolonne" ]
                            [ Knapp.knapp OppsummeringSkjemaLagreknappTrykket "Lagre"
                                |> Knapp.toHtml
                            ]
                        ]
                    ]

            LeggTilUtdanningFeiletIApi _ _ ->
                div [ class "inputrad" ]
                    [ Knapp.knapp OppsummeringSkjemaLagreknappTrykket "Lagre"
                        |> Knapp.toHtml
                    , Knapp.knapp AvbrytLagringOgTaMegTilIntro "Avbryt lagring"
                        |> Knapp.toHtml
                    ]

            _ ->
                text ""
        ]


lagÅrInputKnapp : String -> String -> Msg -> Html Msg
lagÅrInputKnapp knappeTekst inputTekst msg =
    Knapp.knapp msg knappeTekst
        |> (if inputTekst /= "" && Dato.validerÅr inputTekst then
                Knapp.withEnabled Knapp.Enabled

            else
                Knapp.withEnabled Knapp.Disabled
           )
        |> Knapp.toHtml


lagMessageKnapp : String -> Msg -> Html Msg
lagMessageKnapp knappeTekst msg =
    Knapp.knapp msg knappeTekst
        |> Knapp.toHtml


lagFraMånedKnapp : FradatoInfo -> Dato.Måned -> Html Msg
lagFraMånedKnapp fraDatoInfo måned =
    let
        msg =
            { fraDatoInfo | fraMåned = måned }
                |> BrukerTrykketFraMånedKnapp
    in
    måned
        |> Dato.månedTilString
        |> Knapp.knapp msg
        |> Knapp.withClass Knapp.MånedKnapp
        |> Knapp.toHtml


lagTilMånedKnapp : TildatoInfo -> Dato.Måned -> Html Msg
lagTilMånedKnapp tilDatoInfo måned =
    let
        msg =
            { tilDatoInfo | tilMåned = måned }
                |> BrukerTrykketTilMånedKnapp
    in
    måned
        |> Dato.månedTilString
        |> Knapp.knapp msg
        |> Knapp.withClass Knapp.MånedKnapp
        |> Knapp.toHtml


selectNivåListe : List ( String, String )
selectNivåListe =
    [ ( "Grunnskole", "Grunnskole" )
    , ( "VideregåendeYrkesskole", "Videregående/Yrkesskole" )
    , ( "Fagskole", "Fagskole" )
    , ( "Folkehøyskole", "Folkehøyskole" )
    , ( "HøyereUtdanning1til4", "Høyere Utdanning (1-4 år)" )
    , ( "HøyereUtdanning4pluss", "Høyere Utdanning (mer enn 4 år)" )
    , ( "Phd", "PhD" )
    ]


init : FerdigAnimertMeldingsLogg -> List Utdanning -> ( Model, Cmd Msg )
init gammelMeldingsLogg utdanningListe =
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
        }
    , Cmd.batch [ lagtTilSpørsmålCmd ]
    )
