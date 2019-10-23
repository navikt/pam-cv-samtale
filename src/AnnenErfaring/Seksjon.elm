module AnnenErfaring.Seksjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , update
    , viewBrukerInput
    )

import AnnenErfaring.Skjema as Skjema exposing (AnnenErfaringSkjema, Felt(..), ValidertAnnenErfaringSkjema, feilmeldingBeskrivelse, feilmeldingRolle)
import Api
import Browser.Dom as Dom
import Cv.AnnenErfaring exposing (AnnenErfaring)
import Dato exposing (Måned(..), TilDato(..), År)
import DebugStatus exposing (DebugStatus)
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.ManedKnapper as MånedKnapper
import FrontendModuler.Textarea as Textarea
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import Task



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , annenErfaringListe : List AnnenErfaring
    , debugStatus : DebugStatus
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List AnnenErfaring) FerdigAnimertMeldingsLogg


type Samtale
    = RegistrerRolle RolleInfo
    | RegistrerBeskrivelse BeskrivelseInfo
    | RegistrerFraMåned FraDatoInfo
    | RegistrerFraÅr FraDatoInfo
    | RegistrerNåværende ValidertFraDatoInfo
    | RegistrerTilMåned TilDatoInfo
    | RegistrerTilÅr TilDatoInfo
    | VisOppsummering ValidertAnnenErfaringSkjema
    | EndreOpplysninger AnnenErfaringSkjema
    | VisOppsummeringEtterEndring ValidertAnnenErfaringSkjema
    | LagrerSkjema ValidertAnnenErfaringSkjema
    | LagringFeilet Http.Error ValidertAnnenErfaringSkjema
    | VenterPåAnimasjonFørFullføring (List AnnenErfaring)


type alias RolleInfo =
    { rolle : String
    , visFeilmeldingRolle : Bool
    }


type alias BeskrivelseInfo =
    { rolle : String
    , beskrivelse : String
    }


type alias FraDatoInfo =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : String
    , visFeilmeldingÅr : Bool
    }


type alias ValidertFraDatoInfo =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : År
    }


type alias TilDatoInfo =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : År
    , tilMåned : Måned
    , tilÅr : String
    , visFeilmeldingÅr : Bool
    }


rolleTilBeskrivelse : String -> BeskrivelseInfo
rolleTilBeskrivelse rolle =
    { rolle = rolle
    , beskrivelse = ""
    }


beskrivelseTilFraDato : BeskrivelseInfo -> FraDatoInfo
beskrivelseTilFraDato input =
    { rolle = input.rolle
    , beskrivelse = input.beskrivelse
    , fraMåned = Januar
    , fraÅr = ""
    , visFeilmeldingÅr = False
    }


fraDatoTilTilDato : ValidertFraDatoInfo -> TilDatoInfo
fraDatoTilTilDato input =
    { rolle = input.rolle
    , beskrivelse = input.beskrivelse
    , fraMåned = input.fraMåned
    , fraÅr = input.fraÅr
    , tilMåned = Januar
    , tilÅr = ""
    , visFeilmeldingÅr = False
    }


fraDatoTilSkjema : ValidertFraDatoInfo -> ValidertAnnenErfaringSkjema
fraDatoTilSkjema info =
    Skjema.initValidertSkjema
        { rolle = info.rolle
        , beskrivelse = info.beskrivelse
        , fraMåned = info.fraMåned
        , fraÅr = info.fraÅr
        , tilDato = Nåværende
        , id = Nothing
        }


tilDatoTilSkjema : TilDatoInfo -> År -> ValidertAnnenErfaringSkjema
tilDatoTilSkjema info tilÅr =
    Skjema.initValidertSkjema
        { rolle = info.rolle
        , beskrivelse = info.beskrivelse
        , fraMåned = info.fraMåned
        , fraÅr = info.fraÅr
        , tilDato = Avsluttet info.tilMåned tilÅr
        , id = Nothing
        }



--- UPDATE ---


type Msg
    = VilRegistrereRolle
    | OppdaterRolle String
    | VilRegistrereBeskrivelse
    | OppdaterBeskrivelse String
    | FraMånedValgt Dato.Måned
    | VilRegistrereFraÅr
    | OppdatererFraÅr String
    | SvarerJaTilNaavarende
    | VilRegistrereTilMåned
    | TilMånedValgt Dato.Måned
    | VilRegistrereTilÅr
    | OppdatererTilÅr String
    | VilLagreAnnenErfaring
    | VilEndreOpplysninger
    | SkjemaEndret SkjemaEndring
    | VilLagreEndretSkjema
    | AnnenErfaringLagret (Result Http.Error (List AnnenErfaring))
    | FerdigMedAnnenErfaring
    | StartÅSkrive
    | FullførMelding
    | ViewportSatt (Result Dom.Error ())
    | FokusSatt (Result Dom.Error ())
    | ÅrMisterFokus
    | ErrorLogget


type SkjemaEndring
    = Tekst Felt String
    | FraMåned String
    | NåværendeToggled
    | TilMåned String
    | FraÅrBlurred
    | TilÅrBlurred


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        VilRegistrereRolle ->
            case model.aktivSamtale of
                RegistrerRolle info ->
                    case feilmeldingRolle info.rolle of
                        Just _ ->
                            ( { info | visFeilmeldingRolle = True }
                                |> RegistrerRolle
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( info.rolle
                                |> rolleTilBeskrivelse
                                |> RegistrerBeskrivelse
                                |> nesteSamtaleSteg model (Melding.svar [ info.rolle ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterRolle string ->
            case model.aktivSamtale of
                RegistrerRolle info ->
                    ( { info | rolle = string }
                        |> RegistrerRolle
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereBeskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse input ->
                    case feilmeldingBeskrivelse input.beskrivelse of
                        Nothing ->
                            ( input
                                |> beskrivelseTilFraDato
                                |> RegistrerFraMåned
                                |> nesteSamtaleSteg model (Melding.svar [ input.beskrivelse ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Just _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterBeskrivelse string ->
            case model.aktivSamtale of
                RegistrerBeskrivelse beskrivelse ->
                    ( { beskrivelse | beskrivelse = string }
                        |> RegistrerBeskrivelse
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FraMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerFraMåned fraDatoInfo ->
                    ( måned
                        |> setFraMåned fraDatoInfo
                        |> RegistrerFraÅr
                        |> nesteSamtaleSteg model
                            (Melding.svar [ måned |> Dato.månedTilString ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereFraÅr ->
            case model.aktivSamtale of
                RegistrerFraÅr fraDatoInfo ->
                    case Dato.stringTilÅr fraDatoInfo.fraÅr of
                        Just fraÅr ->
                            ( { rolle = fraDatoInfo.rolle
                              , beskrivelse = fraDatoInfo.beskrivelse
                              , fraMåned = fraDatoInfo.fraMåned
                              , fraÅr = fraÅr
                              }
                                |> RegistrerNåværende
                                |> nesteSamtaleSteg model (Melding.svar [ fraDatoInfo.fraÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { fraDatoInfo | visFeilmeldingÅr = True }
                                |> RegistrerFraÅr
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererFraÅr string ->
            case model.aktivSamtale of
                RegistrerFraÅr fraDatoInfo ->
                    ( { fraDatoInfo | fraÅr = string }
                        |> RegistrerFraÅr
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SvarerJaTilNaavarende ->
            case model.aktivSamtale of
                RegistrerNåværende nåværendeInfo ->
                    ( nåværendeInfo
                        |> fraDatoTilSkjema
                        |> VisOppsummering
                        |> nesteSamtaleStegUtenMelding model
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereTilMåned ->
            case model.aktivSamtale of
                RegistrerNåværende fraDatoInfo ->
                    ( fraDatoInfo
                        |> fraDatoTilTilDato
                        |> RegistrerTilMåned
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        TilMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerTilMåned tilDatoInfo ->
                    ( { tilDatoInfo | tilMåned = måned }
                        |> RegistrerTilÅr
                        |> nesteSamtaleSteg model
                            (Melding.svar [ måned |> Dato.månedTilString ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereTilÅr ->
            case model.aktivSamtale of
                RegistrerTilÅr tilDatoInfo ->
                    case Dato.stringTilÅr tilDatoInfo.tilÅr of
                        Just tilÅr ->
                            ( tilDatoTilSkjema tilDatoInfo tilÅr
                                |> VisOppsummering
                                |> nesteSamtaleSteg model (Melding.svar [ tilDatoInfo.tilÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { tilDatoInfo | visFeilmeldingÅr = True }
                                |> RegistrerTilÅr
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererTilÅr string ->
            case model.aktivSamtale of
                RegistrerTilÅr tilDatoInfo ->
                    ( { tilDatoInfo | tilÅr = string }
                        |> RegistrerTilÅr
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        ÅrMisterFokus ->
            case model.aktivSamtale of
                RegistrerFraÅr fraDatoInfo ->
                    ( { fraDatoInfo | visFeilmeldingÅr = True }
                        |> RegistrerFraÅr
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerTilÅr tilDatoInfo ->
                    ( { tilDatoInfo | visFeilmeldingÅr = True }
                        |> RegistrerTilÅr
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilEndreOpplysninger ->
            case model.aktivSamtale of
                VisOppsummering validertAnnenErfaringSkjema ->
                    updateEtterVilEndreSkjema model validertAnnenErfaringSkjema

                VisOppsummeringEtterEndring validertAnnenErfaringSkjema ->
                    updateEtterVilEndreSkjema model validertAnnenErfaringSkjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreOpplysninger annenErfaringSkjema ->
                    ( annenErfaringSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> EndreOpplysninger
                        |> oppdaterSamtalesteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreEndretSkjema ->
            case model.aktivSamtale of
                EndreOpplysninger skjema ->
                    case Skjema.valider skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> VisOppsummeringEtterEndring
                                |> nesteSamtaleSteg model (Melding.svar (validertSkjemaTilSetninger validertSkjema))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> Skjema.visAlleFeilmeldinger
                                |> EndreOpplysninger
                                |> oppdaterSamtalesteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreAnnenErfaring ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                VisOppsummeringEtterEndring skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                LagringFeilet _ skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, prøv på nytt" ])

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        AnnenErfaringLagret result ->
            case model.aktivSamtale of
                LagrerSkjema annenErfaringSkjema ->
                    case result of
                        Ok annenErfaringer ->
                            ( Model
                                { model
                                    | aktivSamtale = VenterPåAnimasjonFørFullføring annenErfaringer
                                    , seksjonsMeldingsLogg =
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Bra. Nå har du lagt til denne erfaringen." ] ]
                                }
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( LagringFeilet error annenErfaringSkjema
                                |> nesteSamtaleStegUtenMelding model
                            , Cmd.batch
                                [ annenErfaringSkjema
                                    |> Skjema.encode
                                    |> Api.logErrorWithRequestBody ErrorLogget "Lagre annen erfaring" error
                                , lagtTilSpørsmålCmd model.debugStatus
                                ]
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FerdigMedAnnenErfaring ->
            case model.aktivSamtale of
                LagringFeilet _ _ ->
                    ( model.annenErfaringListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, gå videre" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

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

        ViewportSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )

        FokusSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )


setFraMåned : FraDatoInfo -> Dato.Måned -> FraDatoInfo
setFraMåned fraDatoInfo måned =
    { fraDatoInfo | fraMåned = måned }


oppdaterSamtalesteg : ModelInfo -> Samtale -> Model
oppdaterSamtalesteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }


oppdaterSkjema : SkjemaEndring -> AnnenErfaringSkjema -> AnnenErfaringSkjema
oppdaterSkjema endring skjema =
    case endring of
        Tekst felt innhold ->
            Skjema.oppdaterTekstFelt felt innhold skjema

        FraMåned månedString ->
            månedString
                |> Dato.stringTilMåned
                |> Skjema.oppdaterFraMåned skjema

        TilMåned månedString ->
            månedString
                |> Dato.stringTilMåned
                |> Skjema.oppdaterTilMåned skjema

        NåværendeToggled ->
            Skjema.toggleNavarende skjema

        FraÅrBlurred ->
            Skjema.visFeilmeldingFraÅr skjema

        TilÅrBlurred ->
            Skjema.visFeilmeldingTilÅr skjema


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring annenErfaringListe ->
                    Ferdig annenErfaringListe ferdigAnimertSamtale

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


updateEtterVilEndreSkjema : ModelInfo -> ValidertAnnenErfaringSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model skjema =
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> EndreOpplysninger
        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> ValidertAnnenErfaringSkjema -> Melding -> SamtaleStatus
updateEtterLagreKnappTrykket model skjema melding =
    ( skjema
        |> LagrerSkjema
        |> nesteSamtaleSteg model melding
    , postEllerPutAnnenErfaring AnnenErfaringLagret skjema
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


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg modelInfo melding samtale =
    Model
        { modelInfo
            | aktivSamtale = samtale
            , seksjonsMeldingsLogg =
                modelInfo.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)
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
samtaleTilMeldingsLogg annenErfaringSeksjon =
    case annenErfaringSeksjon of
        RegistrerRolle _ ->
            [ Melding.spørsmål [ "Så bra at du har mer erfaring. Hvilken rolle har du hatt?" ]
            , Melding.spørsmål
                [ "Har du jobbet som fotballtrener, kanskje besøksvenn eller noe helt annet?" ]
            ]

        RegistrerBeskrivelse _ ->
            [ Melding.spørsmål [ "Hva var jobben din?" ]
            ]

        RegistrerFraMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned begynte du?" ]
            ]

        RegistrerFraÅr _ ->
            [ Melding.spørsmål [ "Hvilket år begynte du?" ]
            ]

        RegistrerNåværende info ->
            [ Melding.spørsmål [ "Jobber du fremdeles som «" ++ info.rolle ++ "»?" ] ]

        RegistrerTilMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned sluttet du?" ]
            ]

        RegistrerTilÅr _ ->
            [ Melding.spørsmål [ "Hvilket år sluttet du?" ]
            ]

        VisOppsummering skjema ->
            [ Melding.spørsmål
                ([ "Du har lagt inn dette:"
                 , Melding.tomLinje
                 ]
                    ++ (validertSkjemaTilSetninger skjema
                            ++ [ Melding.tomLinje
                               , "Er informasjonen riktig?"
                               ]
                       )
                )
            ]

        EndreOpplysninger _ ->
            []

        VisOppsummeringEtterEndring _ ->
            [ Melding.spørsmål [ "Er informasjonen riktig nå?" ] ]

        LagrerSkjema _ ->
            []

        LagringFeilet error _ ->
            [ Melding.spørsmål
                [ "Oops... Jeg klarte ikke å lagre annen erfaring. Vil du prøve på nytt?" ]
            ]

        VenterPåAnimasjonFørFullføring _ ->
            []


validertSkjemaTilSetninger : ValidertAnnenErfaringSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    let
        skjema =
            Skjema.tilUvalidertSkjema validertSkjema
    in
    [ "Rolle: " ++ Skjema.innholdTekstFelt Rolle skjema
    , Dato.periodeTilString (Skjema.fraMåned skjema) (Skjema.fraÅrValidert validertSkjema) (Skjema.tilDatoValidert validertSkjema)
    , "Beskrivelse: " ++ Skjema.innholdTekstFelt Beskrivelse skjema
    ]


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        RegistrerRolle _ ->
            settFokusCmd RolleId

        RegistrerBeskrivelse _ ->
            settFokusCmd BeskrivelseId

        RegistrerFraÅr _ ->
            settFokusCmd FraÅrId

        RegistrerTilÅr _ ->
            settFokusCmd TilÅrId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    inputId
        |> inputIdTilString
        |> Dom.focus
        |> Task.attempt FokusSatt



--- VIEW ---


type InputId
    = RolleId
    | BeskrivelseId
    | FraÅrId
    | TilÅrId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        RolleId ->
            "annenErfaring-rolle-id"

        BeskrivelseId ->
            "annenErfaring-beskrivelse-id"

        FraÅrId ->
            "annenErfaring-fraÅr-id"

        TilÅrId ->
            "annenErfaring-tilÅr-id"


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                RegistrerRolle info ->
                    Containers.inputMedGåVidereKnapp VilRegistrereRolle
                        [ info.rolle
                            |> Input.input { label = "Rolle", msg = OppdaterRolle }
                            |> Input.withOnEnter VilRegistrereRolle
                            |> Input.withId (inputIdTilString RolleId)
                            |> Input.withMaybeFeilmelding
                                (info.rolle
                                    |> feilmeldingRolle
                                    |> maybeHvisTrue info.visFeilmeldingRolle
                                )
                            |> Input.toHtml
                        ]

                RegistrerBeskrivelse info ->
                    Containers.inputMedGåVidereKnapp VilRegistrereBeskrivelse
                        [ info.beskrivelse
                            |> Textarea.textarea { label = "Beskriv oppgavene dine", msg = OppdaterBeskrivelse }
                            |> Textarea.withMaybeFeilmelding (feilmeldingBeskrivelse info.beskrivelse)
                            |> Textarea.withId (inputIdTilString BeskrivelseId)
                            |> Textarea.toHtml
                        ]

                RegistrerFraMåned _ ->
                    MånedKnapper.månedKnapper FraMånedValgt

                RegistrerFraÅr fraDatoInfo ->
                    Containers.inputMedGåVidereKnapp VilRegistrereFraÅr
                        [ div [ class "år-wrapper" ]
                            [ fraDatoInfo.fraÅr
                                |> Input.input { label = "År", msg = OppdatererFraÅr }
                                |> Input.withClass "aar"
                                |> Input.withOnEnter VilRegistrereFraÅr
                                |> Input.withOnBlur ÅrMisterFokus
                                |> Input.withId (inputIdTilString FraÅrId)
                                |> Input.withMaybeFeilmelding
                                    (fraDatoInfo.fraÅr
                                        |> Dato.feilmeldingÅr
                                        |> maybeHvisTrue fraDatoInfo.visFeilmeldingÅr
                                    )
                                |> Input.toHtml
                            ]
                        ]

                RegistrerNåværende _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp SvarerJaTilNaavarende "Ja"
                            |> Knapp.toHtml
                        , Knapp.knapp VilRegistrereTilMåned "Nei"
                            |> Knapp.toHtml
                        ]

                RegistrerTilMåned _ ->
                    MånedKnapper.månedKnapper TilMånedValgt

                RegistrerTilÅr tilDatoInfo ->
                    Containers.inputMedGåVidereKnapp VilRegistrereTilÅr
                        [ div [ class "år-wrapper" ]
                            [ tilDatoInfo.tilÅr
                                |> Input.input { label = "År", msg = OppdatererTilÅr }
                                |> Input.withClass "aar"
                                |> Input.withOnEnter VilRegistrereTilÅr
                                |> Input.withOnBlur ÅrMisterFokus
                                |> Input.withId (inputIdTilString TilÅrId)
                                |> Input.withMaybeFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue tilDatoInfo.visFeilmeldingÅr) tilDatoInfo.tilÅr)
                                |> Input.toHtml
                            ]
                        ]

                VisOppsummering _ ->
                    viewBekreftOppsummering

                VisOppsummeringEtterEndring _ ->
                    viewBekreftOppsummering

                EndreOpplysninger skjema ->
                    Containers.skjema { lagreMsg = VilLagreEndretSkjema, lagreKnappTekst = "Lagre endringer" }
                        [ skjema
                            |> Skjema.innholdTekstFelt Rolle
                            |> Input.input { label = "Rolle", msg = Tekst Rolle >> SkjemaEndret }
                            |> Input.withMaybeFeilmelding (Skjema.innholdTekstFelt Rolle skjema |> feilmeldingRolle)
                            |> Input.toHtml
                        , skjema
                            |> Skjema.innholdTekstFelt Beskrivelse
                            |> Textarea.textarea { label = "Beskrivelse", msg = Tekst Beskrivelse >> SkjemaEndret }
                            |> Textarea.withMaybeFeilmelding (Skjema.innholdTekstFelt Beskrivelse skjema |> feilmeldingBeskrivelse)
                            |> Textarea.toHtml
                        , div [ class "DatoInput-fra-til-rad" ]
                            [ DatoInput.datoInput
                                { label = "Fra"
                                , onMånedChange = FraMåned >> SkjemaEndret
                                , måned = Skjema.fraMåned skjema
                                , onÅrChange = Tekst FraÅr >> SkjemaEndret
                                , år = Skjema.innholdTekstFelt FraÅr skjema
                                }
                                |> DatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingFraÅr skjema)
                                |> DatoInput.withOnBlurÅr (SkjemaEndret FraÅrBlurred)
                                |> DatoInput.toHtml
                            , if not (Skjema.nåværende skjema) then
                                DatoInput.datoInput
                                    { label = "Til"
                                    , onMånedChange = TilMåned >> SkjemaEndret
                                    , måned = Skjema.tilMåned skjema
                                    , onÅrChange = Tekst TilÅr >> SkjemaEndret
                                    , år = Skjema.innholdTekstFelt TilÅr skjema
                                    }
                                    |> DatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingTilÅr skjema)
                                    |> DatoInput.withOnBlurÅr (SkjemaEndret TilÅrBlurred)
                                    |> DatoInput.toHtml

                              else
                                text ""
                            ]
                        , skjema
                            |> Skjema.nåværende
                            |> Checkbox.checkbox "Nåværende" (SkjemaEndret NåværendeToggled)
                            |> Checkbox.toHtml
                        ]

                LagrerSkjema _ ->
                    div [] []

                LagringFeilet _ _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp VilLagreAnnenErfaring "Ja, prøv på nytt"
                            |> Knapp.toHtml
                        , Knapp.knapp FerdigMedAnnenErfaring "Nei, gå videre"
                            |> Knapp.toHtml
                        ]

                VenterPåAnimasjonFørFullføring _ ->
                    div [] []

        MeldingerGjenstår ->
            text ""


viewBekreftOppsummering : Html Msg
viewBekreftOppsummering =
    Containers.knapper Flytende
        [ Knapp.knapp VilLagreAnnenErfaring "Ja, informasjonen er riktig"
            |> Knapp.toHtml
        , Knapp.knapp VilEndreOpplysninger "Nei, jeg vil endre"
            |> Knapp.toHtml
        ]


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


postEllerPutAnnenErfaring : (Result Error (List AnnenErfaring) -> msg) -> Skjema.ValidertAnnenErfaringSkjema -> Cmd msg
postEllerPutAnnenErfaring msgConstructor skjema =
    case Skjema.id skjema of
        Just id ->
            Api.putAnnenErfaring msgConstructor skjema id

        Nothing ->
            Api.postAnnenErfaring msgConstructor skjema



--- INIT ---


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List AnnenErfaring -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg annenErfaringListe =
    let
        aktivSamtale =
            RegistrerRolle { rolle = "", visFeilmeldingRolle = False }
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , annenErfaringListe = annenErfaringListe
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )
