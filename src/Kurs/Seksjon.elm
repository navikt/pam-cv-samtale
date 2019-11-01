module Kurs.Seksjon exposing
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
import Cv.Kurs exposing (Kurs)
import Dato exposing (Måned(..), År)
import DebugStatus exposing (DebugStatus)
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.ManedKnapper as MånedKnapper
import FrontendModuler.Select as Select
import FrontendModuler.Textarea as Textarea
import FrontendModuler.ValgfriDatoInput as ValgfriDatoInput
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Kurs.Skjema as Skjema exposing (Felt(..), FullførtDato(..), KursSkjema, ValidertKursSkjema, VarighetEnhet(..))
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Process
import SamtaleAnimasjon
import Task



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , kursListe : List Kurs
    , debugStatus : DebugStatus
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Kurs) FerdigAnimertMeldingsLogg


type Samtale
    = RegistrerKursnavn KursnavnInfo
    | RegistrerKursholder KursholderInfo
    | SpørOmBrukerVilLeggeInnFullførtDato KursholderInfo
    | RegistrerFullførtMåned FullførtDatoInfo
    | RegistrerFullførtÅr FullførtDatoInfo
    | RegistrerVarighetEnhet VarighetInfo
    | RegistrerVarighet VarighetInfo
    | VisOppsummering ValidertKursSkjema
    | EndreOpplysninger KursSkjema
    | VisOppsummeringEtterEndring ValidertKursSkjema
    | LagrerSkjema ValidertKursSkjema
    | LagringFeilet Http.Error ValidertKursSkjema
    | VenterPåAnimasjonFørFullføring (List Kurs)


type alias KursnavnInfo =
    { kursnavn : String
    , tillatÅViseFeilmeldingKursnavn : Bool
    }


type alias KursholderInfo =
    { kursnavn : String
    , kursholder : String
    }


type alias FullførtDatoInfo =
    { kursnavn : String
    , kursholder : String
    , fullførtMåned : Måned
    , fullførtÅr : String
    , tillatÅViseFeilmeldingÅr : Bool
    }


type alias ValidertFullførtDatoInfo =
    { kursnavn : String
    , kursholder : String
    , fullførtDato : FullførtDato
    }


type alias VarighetInfo =
    { kursnavn : String
    , kursholder : String
    , fullførtDato : FullførtDato
    , varighet : String
    , varighetEnhet : VarighetEnhet
    , tillatÅViseFeilmeldingVarighet : Bool
    }


kursnavnTilKursholder : String -> KursholderInfo
kursnavnTilKursholder kursnavn =
    { kursnavn = kursnavn
    , kursholder = ""
    }


kursholderTilFullførtDato : KursholderInfo -> FullførtDatoInfo
kursholderTilFullførtDato input =
    { kursnavn = input.kursnavn
    , kursholder = input.kursholder
    , fullførtMåned = Januar
    , fullførtÅr = ""
    , tillatÅViseFeilmeldingÅr = False
    }


kursholderTilVarighet : KursholderInfo -> VarighetInfo
kursholderTilVarighet input =
    { kursnavn = input.kursnavn
    , kursholder = input.kursholder
    , fullførtDato = IkkeOppgitt
    , varighet = ""
    , varighetEnhet = TIME
    , tillatÅViseFeilmeldingVarighet = False
    }


fullførtDatoTilVarighet : ValidertFullførtDatoInfo -> VarighetInfo
fullførtDatoTilVarighet input =
    { kursnavn = input.kursnavn
    , kursholder = input.kursholder
    , fullførtDato = input.fullførtDato
    , varighet = ""
    , varighetEnhet = TIME
    , tillatÅViseFeilmeldingVarighet = False
    }


fullførtDatoTilSkjema : ValidertFullførtDatoInfo -> ValidertKursSkjema
fullførtDatoTilSkjema info =
    Skjema.initValidertSkjema
        { kursnavn = info.kursnavn
        , kursholder = info.kursholder
        , fullførtDato = info.fullførtDato
        , varighet = Nothing
        , varighetEnhet = TIME
        , id = Nothing
        }


varighetTilSkjema : VarighetInfo -> ValidertKursSkjema
varighetTilSkjema info =
    Skjema.initValidertSkjema
        { kursnavn = info.kursnavn
        , kursholder = info.kursholder
        , fullførtDato = info.fullførtDato
        , varighet = String.toInt info.varighet
        , varighetEnhet = info.varighetEnhet
        , id = Nothing
        }



--- UPDATE ---


type Msg
    = VilRegistrereKursnavn
    | OppdatererKursnavn String
    | VilRegistrereKursholder
    | OppdatererKursholder String
    | SvarerJaTilFullførtDato
    | SvarerNeiTilFullførtDato
    | FullførtMånedValgt Dato.Måned
    | VilRegistrereFullførtÅr
    | OppdatererFullførtÅr String
    | VarighetEnhetValgt VarighetEnhet
    | VilRegistrereVarighet
    | OppdatererVarighet String
    | VilLagreKurs
    | VilEndreOpplysninger
    | SkjemaEndret SkjemaEndring
    | VilLagreEndretSkjema
    | KursLagret (Result Http.Error (List Kurs))
    | FerdigMedKurs
    | StartÅSkrive
    | FullførMelding
    | ViewportSatt (Result Dom.Error ())
    | FokusSatt (Result Dom.Error ())
    | FeltMisterFokus
    | ErrorLogget


type SkjemaEndring
    = Tekst Felt String
    | FullførtMåned String
    | VarighetEnhet String
    | FullførtÅrBlurred
    | KursnavnBlurred


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        VilRegistrereKursnavn ->
            case model.aktivSamtale of
                RegistrerKursnavn info ->
                    case Skjema.feilmeldingKursnavn info.kursnavn of
                        Just _ ->
                            ( { info | tillatÅViseFeilmeldingKursnavn = True }
                                |> RegistrerKursnavn
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( info.kursnavn
                                |> kursnavnTilKursholder
                                |> RegistrerKursholder
                                |> nesteSamtaleSteg model (Melding.svar [ info.kursnavn ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererKursnavn string ->
            case model.aktivSamtale of
                RegistrerKursnavn info ->
                    ( { info | kursnavn = string }
                        |> RegistrerKursnavn
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereKursholder ->
            case model.aktivSamtale of
                RegistrerKursholder input ->
                    case Skjema.feilmeldingKursholder input.kursholder of
                        Nothing ->
                            ( input
                                |> SpørOmBrukerVilLeggeInnFullførtDato
                                |> nesteSamtaleSteg model (Melding.svar [ input.kursholder ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Just _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererKursholder string ->
            case model.aktivSamtale of
                RegistrerKursholder kursholder ->
                    ( { kursholder | kursholder = string }
                        |> RegistrerKursholder
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SvarerJaTilFullførtDato ->
            case model.aktivSamtale of
                SpørOmBrukerVilLeggeInnFullførtDato info ->
                    ( info
                        |> kursholderTilFullførtDato
                        |> RegistrerFullførtMåned
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, det vil jeg" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SvarerNeiTilFullførtDato ->
            case model.aktivSamtale of
                SpørOmBrukerVilLeggeInnFullførtDato info ->
                    ( info
                        |> kursholderTilVarighet
                        |> RegistrerVarighetEnhet
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, det vil jeg ikke" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FullførtMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerFullførtMåned fullførtDatoInfo ->
                    ( måned
                        |> setFullførtMåned fullførtDatoInfo
                        |> RegistrerFullførtÅr
                        |> nesteSamtaleSteg model
                            (Melding.svar [ måned |> Dato.månedTilString ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereFullførtÅr ->
            case model.aktivSamtale of
                RegistrerFullførtÅr fullførtDatoInfo ->
                    case Dato.stringTilÅr fullførtDatoInfo.fullførtÅr of
                        Just fullførtÅr ->
                            ( { kursnavn = fullførtDatoInfo.kursnavn
                              , kursholder = fullførtDatoInfo.kursholder
                              , fullførtDato = Oppgitt fullførtDatoInfo.fullførtMåned fullførtÅr
                              }
                                |> fullførtDatoTilVarighet
                                |> RegistrerVarighetEnhet
                                |> nesteSamtaleSteg model (Melding.svar [ fullførtDatoInfo.fullførtÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { fullførtDatoInfo | tillatÅViseFeilmeldingÅr = True }
                                |> RegistrerFullførtÅr
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererFullførtÅr string ->
            case model.aktivSamtale of
                RegistrerFullførtÅr fullførtDatoInfo ->
                    ( { fullførtDatoInfo | fullførtÅr = string }
                        |> RegistrerFullførtÅr
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VarighetEnhetValgt enhet ->
            case model.aktivSamtale of
                RegistrerVarighetEnhet varighetInfo ->
                    ( { varighetInfo | varighetEnhet = enhet }
                        |> RegistrerVarighet
                        |> nesteSamtaleSteg model (Melding.svar [ Skjema.varighetEnhetTilString enhet ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereVarighet ->
            case model.aktivSamtale of
                RegistrerVarighet info ->
                    ( info
                        |> varighetTilSkjema
                        |> VisOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ info.varighet ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererVarighet varighet_ ->
            case model.aktivSamtale of
                RegistrerVarighet varighetInfo ->
                    ( { varighetInfo | varighet = varighet_ }
                        |> RegistrerVarighet
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FeltMisterFokus ->
            case model.aktivSamtale of
                RegistrerFullførtÅr fullførtDatoInfo ->
                    ( { fullførtDatoInfo | tillatÅViseFeilmeldingÅr = True }
                        |> RegistrerFullførtÅr
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerVarighetEnhet varighetInfo ->
                    ( { varighetInfo | tillatÅViseFeilmeldingVarighet = True }
                        |> RegistrerVarighetEnhet
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerKursnavn kursnavnInfo ->
                    ( { kursnavnInfo | tillatÅViseFeilmeldingKursnavn = True }
                        |> RegistrerKursnavn
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilEndreOpplysninger ->
            case model.aktivSamtale of
                VisOppsummering validertKursSkjema ->
                    updateEtterVilEndreSkjema model validertKursSkjema

                VisOppsummeringEtterEndring validertKursSkjema ->
                    updateEtterVilEndreSkjema model validertKursSkjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreOpplysninger kursSkjema ->
                    ( kursSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> EndreOpplysninger
                        |> oppdaterSamtaleSteg model
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
                                |> Skjema.tillatÅViseAlleFeilmeldinger
                                |> EndreOpplysninger
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreKurs ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                VisOppsummeringEtterEndring skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                LagringFeilet _ skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, prøv på nytt" ])

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        KursLagret result ->
            case model.aktivSamtale of
                LagrerSkjema kursSkjema ->
                    case result of
                        Ok kurser ->
                            ( Model
                                { model
                                    | aktivSamtale = VenterPåAnimasjonFørFullføring kurser
                                    , seksjonsMeldingsLogg =
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Bra. Nå har du lagt til denne erfaringen." ] ]
                                }
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( LagringFeilet error kursSkjema
                                |> nesteSamtaleStegUtenMelding model
                            , Cmd.batch
                                [ kursSkjema
                                    |> Skjema.encode
                                    |> Api.logErrorWithRequestBody ErrorLogget "Lagre annen erfaring" error
                                , lagtTilSpørsmålCmd model.debugStatus
                                ]
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FerdigMedKurs ->
            case model.aktivSamtale of
                LagringFeilet _ _ ->
                    ( model.kursListe
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


setFullførtMåned : FullførtDatoInfo -> Dato.Måned -> FullførtDatoInfo
setFullførtMåned fullførtDatoInfo måned =
    { fullførtDatoInfo | fullførtMåned = måned }


oppdaterSkjema : SkjemaEndring -> KursSkjema -> KursSkjema
oppdaterSkjema endring skjema =
    case endring of
        Tekst felt innhold ->
            Skjema.oppdaterTekstFelt felt innhold skjema

        FullførtMåned månedString ->
            månedString
                |> Dato.stringTilMaybeMåned
                |> Skjema.oppdaterFullførtMåned skjema

        FullførtÅrBlurred ->
            Skjema.tillatÅViseFeilmeldingFullførtÅr skjema

        KursnavnBlurred ->
            Skjema.tillatÅViseFeilmeldingKursnavn skjema

        VarighetEnhet string ->
            string
                |> Skjema.stringTilVarighetEnhet
                |> Skjema.oppdaterVarighetEnhet skjema


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring kursListe ->
                    Ferdig kursListe ferdigAnimertSamtale

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


updateEtterVilEndreSkjema : ModelInfo -> ValidertKursSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model skjema =
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> EndreOpplysninger
        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> ValidertKursSkjema -> Melding -> SamtaleStatus
updateEtterLagreKnappTrykket model skjema melding =
    ( skjema
        |> LagrerSkjema
        |> nesteSamtaleSteg model melding
    , Api.postKurs KursLagret skjema
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
samtaleTilMeldingsLogg kursSeksjon =
    case kursSeksjon of
        RegistrerKursnavn _ ->
            [ Melding.spørsmål [ "Hva heter kurset du vil legge inn?" ]
            , Melding.spørsmål
                [ "Har du førstehjelpskurs, kanskje et språkkurs, eller noe helt annet?" ]
            ]

        RegistrerKursholder _ ->
            [ Melding.spørsmål [ "Hvilket firma eller organisasjon arrangerte kurset?" ]
            ]

        SpørOmBrukerVilLeggeInnFullførtDato _ ->
            [ Melding.spørsmål [ "Vil du legge inn informasjon om når du fullførte kurset?" ]
            ]

        RegistrerFullførtMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned var du ferdig med kurset?" ]
            ]

        RegistrerFullførtÅr _ ->
            [ Melding.spørsmål [ "Hvilket år var du ferdig med kurset?" ]
            ]

        RegistrerVarighetEnhet _ ->
            [ Melding.spørsmål [ "Hvor lenge varte kurset? Var det timer, dager, uker eller måneder?" ] ]

        RegistrerVarighet info ->
            [ Melding.spørsmål [ "Hvor mange " ++ (String.toLower << Skjema.varighetEnhetTilString) info.varighetEnhet ++ " varte kurset?" ] ]

        VisOppsummering skjema ->
            [ [ [ "Du har lagt inn dette:"
                , Melding.tomLinje
                ]
              , validertSkjemaTilSetninger skjema
              , [ Melding.tomLinje
                , "Er informasjonen riktig?"
                ]
              ]
                |> List.concat
                |> Melding.spørsmål
            ]

        EndreOpplysninger _ ->
            []

        VisOppsummeringEtterEndring _ ->
            [ Melding.spørsmål [ "Er informasjonen riktig nå?" ] ]

        LagrerSkjema _ ->
            []

        LagringFeilet _ _ ->
            [ Melding.spørsmål
                [ "Oops... Jeg klarte ikke å lagre annen erfaring. Vil du prøve på nytt?" ]
            ]

        VenterPåAnimasjonFørFullføring _ ->
            []


validertSkjemaTilSetninger : ValidertKursSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    let
        skjema =
            Skjema.tilUvalidertSkjema validertSkjema
    in
    [ case Skjema.fullførtDatoValidert validertSkjema of
        Oppgitt måned_ år_ ->
            [ Dato.datoTilString måned_ år_ ]

        IkkeOppgitt ->
            []
    , [ "Kursnavn: " ++ Skjema.innholdTekstFelt Kursnavn skjema
      , "Kursholder: " ++ Skjema.innholdTekstFelt Kursholder skjema
      , "Varighet: " ++ Skjema.innholdTekstFelt Varighet skjema ++ " " ++ (skjema |> Skjema.varighetEnhet |> Skjema.varighetEnhetTilString |> String.toLower)
      ]
    ]
        |> List.concat


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        RegistrerKursnavn _ ->
            settFokusCmd KursnavnId

        RegistrerKursholder _ ->
            settFokusCmd KursholderId

        RegistrerFullførtÅr _ ->
            settFokusCmd FullførtÅrId

        RegistrerVarighetEnhet _ ->
            settFokusCmd VarighetId

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
    = KursnavnId
    | KursholderId
    | FullførtÅrId
    | VarighetId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        KursnavnId ->
            "kurs-kursnavn-id"

        KursholderId ->
            "kurs-kursholder-id"

        FullførtÅrId ->
            "kurs-fullførtÅr-id"

        VarighetId ->
            "kurs-varighet-id"


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                RegistrerKursnavn info ->
                    Containers.inputMedGåVidereKnapp VilRegistrereKursnavn
                        [ info.kursnavn
                            |> Input.input { label = "Kursets navn", msg = OppdatererKursnavn }
                            |> Input.withOnEnter VilRegistrereKursnavn
                            |> Input.withId (inputIdTilString KursnavnId)
                            |> Input.withOnBlur FeltMisterFokus
                            |> Input.withMaybeFeilmelding
                                (info.kursnavn
                                    |> Skjema.feilmeldingKursnavn
                                    |> maybeHvisTrue info.tillatÅViseFeilmeldingKursnavn
                                )
                            |> Input.toHtml
                        ]

                RegistrerKursholder info ->
                    Containers.inputMedGåVidereKnapp VilRegistrereKursholder
                        [ info.kursholder
                            |> Input.input { label = "Kursets arrangør", msg = OppdatererKursholder }
                            |> Input.withOnEnter VilRegistrereKursholder
                            |> Input.withMaybeFeilmelding (Skjema.feilmeldingKursholder info.kursholder)
                            |> Input.withId (inputIdTilString KursholderId)
                            |> Input.toHtml
                        ]

                SpørOmBrukerVilLeggeInnFullførtDato _ ->
                    Containers.knapper Flytende
                        [ "Ja, det vil jeg"
                            |> Knapp.knapp SvarerJaTilFullførtDato
                            |> Knapp.toHtml
                        , "Nei, det vil jeg ikke"
                            |> Knapp.knapp SvarerNeiTilFullførtDato
                            |> Knapp.toHtml
                        ]

                RegistrerFullførtMåned _ ->
                    MånedKnapper.månedKnapper FullførtMånedValgt

                RegistrerFullførtÅr fullførtDatoInfo ->
                    Containers.inputMedGåVidereKnapp VilRegistrereFullførtÅr
                        [ div [ class "år-wrapper" ]
                            [ fullførtDatoInfo.fullførtÅr
                                |> Input.input { label = "År", msg = OppdatererFullførtÅr }
                                |> Input.withClass "aar"
                                |> Input.withOnEnter VilRegistrereFullførtÅr
                                |> Input.withOnBlur FeltMisterFokus
                                |> Input.withId (inputIdTilString FullførtÅrId)
                                |> Input.withMaybeFeilmelding
                                    (fullførtDatoInfo.fullførtÅr
                                        |> Dato.feilmeldingÅr
                                        |> maybeHvisTrue fullførtDatoInfo.tillatÅViseFeilmeldingÅr
                                    )
                                |> Input.toHtml
                            ]
                        ]

                RegistrerVarighetEnhet _ ->
                    varighetEnhetKnapper VarighetEnhetValgt

                RegistrerVarighet info ->
                    Containers.inputMedGåVidereKnapp VilRegistrereVarighet
                        [ div [ class "år-wrapper" ]
                            [ info.varighet
                                |> Input.input { label = "Antall", msg = OppdatererVarighet }
                                |> Input.withOnEnter VilRegistrereVarighet
                                |> Input.withOnBlur FeltMisterFokus
                                |> Input.withId (inputIdTilString VarighetId)
                                |> Input.withMaybeFeilmelding
                                    (info.varighet
                                        |> Skjema.feilmeldingVarighet
                                        |> maybeHvisTrue info.tillatÅViseFeilmeldingVarighet
                                    )
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
                            |> Skjema.innholdTekstFelt Kursnavn
                            |> Input.input { label = "Kursnavn", msg = Tekst Kursnavn >> SkjemaEndret }
                            |> Input.withMaybeFeilmelding (Skjema.feilmeldingKursnavnHvisSynlig skjema)
                            |> Input.withOnBlur (SkjemaEndret KursnavnBlurred)
                            |> Input.toHtml
                        , skjema
                            |> Skjema.innholdTekstFelt Kursholder
                            |> Textarea.textarea { label = "Kursholder", msg = Tekst Kursholder >> SkjemaEndret }
                            |> Textarea.withMaybeFeilmelding (Skjema.innholdTekstFelt Kursholder skjema |> Skjema.feilmeldingKursholder)
                            |> Textarea.toHtml
                        , div [ class "DatoInput-fra-til-rad" ]
                            [ ValgfriDatoInput.datoInput
                                { label = "Fullført"
                                , onMånedChange = FullførtMåned >> SkjemaEndret
                                , måned = Skjema.fullførtMåned skjema
                                , onÅrChange = Tekst FullførtÅr >> SkjemaEndret
                                , år = Skjema.innholdTekstFelt FullførtÅr skjema
                                }
                                |> ValgfriDatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingFullførtÅr skjema)
                                |> ValgfriDatoInput.withMaybeFeilmeldingMåned (Skjema.feilmeldingFullførtMåned skjema)
                                |> ValgfriDatoInput.withOnBlurÅr (SkjemaEndret FullførtÅrBlurred)
                                |> ValgfriDatoInput.toHtml
                            , viewVarighet skjema
                            ]
                        ]

                LagrerSkjema _ ->
                    div [] []

                LagringFeilet _ _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp VilLagreKurs "Ja, prøv på nytt"
                            |> Knapp.toHtml
                        , Knapp.knapp FerdigMedKurs "Nei, gå videre"
                            |> Knapp.toHtml
                        ]

                VenterPåAnimasjonFørFullføring _ ->
                    div [] []

        MeldingerGjenstår ->
            text ""


varighetEnhetKnapper : (VarighetEnhet -> msg) -> Html msg
varighetEnhetKnapper onEnhetClick =
    div [ class "knapperad" ]
        [ div [ class "knapper--varighet" ]
            (List.map (varighetEnhetKnapp onEnhetClick)
                [ TIME
                , DAG
                , UKE
                , MND
                ]
            )
        ]


varighetEnhetKnapp : (VarighetEnhet -> msg) -> VarighetEnhet -> Html msg
varighetEnhetKnapp onEnhetClick enhet =
    enhet
        |> Skjema.varighetEnhetTilString
        |> Knapp.knapp (onEnhetClick enhet)
        |> Knapp.toHtml


viewBekreftOppsummering : Html Msg
viewBekreftOppsummering =
    Containers.knapper Flytende
        [ Knapp.knapp VilLagreKurs "Ja, informasjonen er riktig"
            |> Knapp.toHtml
        , Knapp.knapp VilEndreOpplysninger "Nei, jeg vil endre"
            |> Knapp.toHtml
        ]


viewVarighet : KursSkjema -> Html Msg
viewVarighet skjema =
    div [ class " Varighet-wrapper skjemaelement" ]
        [ label []
            [ span [ class "skjemaelement__label", id "varighet-label" ] [ text "Timer/dager/uker/måneder" ]
            , div [ class "Select-wrapper" ]
                [ Select.select
                    ""
                    (VarighetEnhet >> SkjemaEndret)
                    [ ( "Timer", "Timer" )
                    , ( "Dager", "Dager" )
                    , ( "Uker", "Uker" )
                    , ( "Måneder", "Måneder" )
                    ]
                    |> Select.withSelected
                        (Skjema.varighetEnhetTilString (Skjema.varighetEnhet skjema))
                    |> Select.withLabelId "varighet-label"
                    |> Select.withClass "Varighet-enhet"
                    |> Select.toHtml
                ]
            , Skjema.innholdTekstFelt Varighet skjema
                |> Input.input { label = "", msg = Tekst Varighet >> SkjemaEndret }
                |> Input.withoutLabel
                |> Input.withClass "Varighet-antall"
                |> Input.withMaybeFeilmelding (Skjema.feilmeldingVarighet (Skjema.innholdTekstFelt Varighet skjema))
                |> Input.toHtml
            ]
        ]


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing



--- INIT ---


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List Kurs -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg kursListe =
    let
        aktivSamtale =
            RegistrerKursnavn { kursnavn = "", tillatÅViseFeilmeldingKursnavn = False }
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (MeldingsLogg.tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , kursListe = kursListe
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )
