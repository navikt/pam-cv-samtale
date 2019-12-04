module Kurs.Seksjon exposing
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
import Cv.Kurs exposing (Kurs)
import Dato exposing (Måned(..), År)
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.ManedKnapper as MånedKnapper
import FrontendModuler.Select as Select
import FrontendModuler.ValgfriDatoInput as ValgfriDatoInput
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Kurs.Skjema as Skjema exposing (Felt(..), FullførtDato(..), KursSkjema, ValidertKursSkjema, VarighetEnhet(..))
import LagreStatus exposing (LagreStatus)
import Meldinger.Melding as Melding exposing (Melding(..))
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Process
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
    | LagrerSkjema ValidertKursSkjema LagreStatus
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
    , varighetEnhet = Time
    , tillatÅViseFeilmeldingVarighet = False
    }


fullførtDatoTilVarighet : ValidertFullførtDatoInfo -> VarighetInfo
fullførtDatoTilVarighet input =
    { kursnavn = input.kursnavn
    , kursholder = input.kursholder
    , fullførtDato = input.fullførtDato
    , varighet = ""
    , varighetEnhet = Time
    , tillatÅViseFeilmeldingVarighet = False
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
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())
    | FeltMisterFokus
    | ErrorLogget


type SkjemaEndring
    = Tekst Felt String
    | FullførtMåned String
    | VarighetEnhet String
    | FullførtÅrBlurred
    | KursnavnBlurred
    | VarighetBlurred


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
                    case Skjema.feilmeldingVarighet info.varighet of
                        Just _ ->
                            ( { info | tillatÅViseFeilmeldingVarighet = True }
                                |> RegistrerVarighet
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Nothing ->
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
                RegistrerKursnavn kursnavnInfo ->
                    ( { kursnavnInfo | tillatÅViseFeilmeldingKursnavn = True }
                        |> RegistrerKursnavn
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerFullførtÅr fullførtDatoInfo ->
                    ( { fullførtDatoInfo | tillatÅViseFeilmeldingÅr = True }
                        |> RegistrerFullførtÅr
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerVarighet varighetInfo ->
                    ( { varighetInfo | tillatÅViseFeilmeldingVarighet = True }
                        |> RegistrerVarighet
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

                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerSkjema skjema
                        |> nesteSamtaleSteg model (Melding.svar [ "Prøv igjen" ])
                    , Api.postKurs KursLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        KursLagret result ->
            case model.aktivSamtale of
                LagrerSkjema skjema lagreStatus ->
                    case result of
                        Ok kurs ->
                            let
                                oppdatertMeldingslogg =
                                    if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Bra. Nå har du lagt til et kurs 👍" ] ]

                                    else
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Bra. Nå har du lagt til et kurs 👍" ] ]
                            in
                            ( kurs
                                |> VenterPåAnimasjonFørFullføring
                                |> oppdaterSamtaleSteg { model | seksjonsMeldingsLogg = oppdatertMeldingslogg }
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtaleSteg model
                                    , Api.postKurs KursLagret skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtaleSteg model
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre kurs" error
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
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre kurs" error
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
                        |> nesteSamtaleSteg model (Melding.svar [ "Gå videre" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case model.aktivSamtale of
                        LagrerSkjema skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerSkjema skjema
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagringFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtaleSteg model
                                    , Api.postKurs KursLagret skjema
                                    )

                            else
                                IkkeFerdig ( Model model, Cmd.none )

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Hidden ->
                    IkkeFerdig ( Model model, Cmd.none )

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            SamtaleAnimasjon.update model.debugStatus samtaleAnimasjonMsg model.seksjonsMeldingsLogg
                |> updateEtterFullførtMelding model

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

        VarighetEnhet string ->
            string
                |> Skjema.stringTilVarighetEnhet
                |> Skjema.oppdaterVarighetEnhet skjema

        FullførtÅrBlurred ->
            Skjema.tillatÅViseFeilmeldingFullførtÅr skjema

        KursnavnBlurred ->
            Skjema.tillatÅViseFeilmeldingKursnavn skjema

        VarighetBlurred ->
            Skjema.tillatÅViseFeilmeldingVarighet skjema


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding model ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring kursListe ->
                    Ferdig kursListe ferdigAnimertSamtale

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
    ( LagreStatus.init
        |> LagrerSkjema skjema
        |> nesteSamtaleSteg model melding
    , Api.postKurs KursLagret skjema
    )
        |> IkkeFerdig


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


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
            [ Melding.spørsmål [ "Det kan være nyttig for en arbeidsgiver å vite når du fullførte kurset. Vil du legge inn det?" ]
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
            [ Melding.spørsmål [ "Hvor mange " ++ (Skjema.varighetEnhetTilString >> String.toLower) info.varighetEnhet ++ " varte kurset?" ] ]

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
            [ Melding.spørsmål [ "Gjør endringene du ønsker i feltene under." ] ]

        VisOppsummeringEtterEndring _ ->
            [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

        LagrerSkjema _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre kurs" } ]

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

        RegistrerVarighet _ ->
            settFokusCmd VarighetId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
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
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
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
                        |> Input.withErObligatorisk
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
                            |> Input.withErObligatorisk
                            |> Input.toHtml
                        ]
                    ]

            RegistrerVarighetEnhet _ ->
                varighetEnhetKnapper

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
                        |> Input.withErObligatorisk
                        |> Input.toHtml
                    , skjema
                        |> Skjema.innholdTekstFelt Kursholder
                        |> Input.input { label = "Kursholder", msg = Tekst Kursholder >> SkjemaEndret }
                        |> Input.withMaybeFeilmelding (Skjema.innholdTekstFelt Kursholder skjema |> Skjema.feilmeldingKursholder)
                        |> Input.toHtml
                    , div [ class "DatoInput-fra-til-rad" ]
                        [ ValgfriDatoInput.datoInput
                            { label = "Når var du ferdig med kurset?"
                            , onMånedChange = FullførtMåned >> SkjemaEndret
                            , måned = Skjema.fullførtMåned skjema
                            , onÅrChange = Tekst FullførtÅr >> SkjemaEndret
                            , år = Skjema.innholdTekstFelt FullførtÅr skjema
                            }
                            |> ValgfriDatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingValgfrittFullførtÅr skjema)
                            |> ValgfriDatoInput.withMaybeFeilmeldingPeriode (Skjema.feilmeldingPeriode skjema)
                            |> ValgfriDatoInput.withOnBlurÅr (SkjemaEndret FullførtÅrBlurred)
                            |> ValgfriDatoInput.toHtml
                        , viewVarighet skjema
                        ]
                    ]

            LagrerSkjema _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    text ""

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    ErrorHåndtering.GiOpp ->
                        Containers.knapper Flytende
                            [ Knapp.knapp FerdigMedKurs "Gå videre"
                                |> Knapp.toHtml
                            ]

                    ErrorHåndtering.PrøvPåNytt ->
                        Containers.knapper Flytende
                            [ Knapp.knapp FerdigMedKurs "Prøv igjen"
                                |> Knapp.toHtml
                            , Knapp.knapp FerdigMedKurs "Gå videre"
                                |> Knapp.toHtml
                            ]

                    ErrorHåndtering.LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            VenterPåAnimasjonFørFullføring _ ->
                div [] []

    else
        text ""


varighetEnhetKnapper : Html Msg
varighetEnhetKnapper =
    div [ class "knapperad" ]
        [ div [ class "knapper--varighet" ]
            (List.map varighetEnhetKnapp
                [ Time
                , Dag
                , Uke
                , Måned
                ]
            )
        ]


varighetEnhetKnapp : VarighetEnhet -> Html Msg
varighetEnhetKnapp enhet =
    enhet
        |> Skjema.varighetEnhetTilString
        |> Knapp.knapp (VarighetEnhetValgt enhet)
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
    fieldset [ class "DatoInput-fieldset" ]
        [ legend [ class "skjemaelement__label" ]
            [ text "Hvor lenge varte kurset?" ]
        , div [ class "Varighet-kolonne skjemaelement" ]
            [ div [ class "Inputs-wrapper" ]
                [ Skjema.innholdTekstFelt Varighet skjema
                    |> Input.input { label = "Antall", msg = Tekst Varighet >> SkjemaEndret }
                    |> Input.withClass "Varighet-antall"
                    |> Input.withOnBlur (SkjemaEndret VarighetBlurred)
                    |> Input.withMaybeFeilmelding (Skjema.feilmeldingVarighetHvisSynlig skjema)
                    |> Input.toHtml
                , div [ class "Select-wrapper" ]
                    [ Select.select
                        "Timer/dager/uker/måneder"
                        (VarighetEnhet >> SkjemaEndret)
                        [ ( "Timer", "Timer" )
                        , ( "Dager", "Dager" )
                        , ( "Uker", "Uker" )
                        , ( "Måneder", "Måneder" )
                        ]
                        |> Select.withSelected
                            (Skjema.varighetEnhetTilString (Skjema.varighetEnhet skjema))
                        |> Select.withClass "Varighet-enhet"
                        |> Select.toHtml
                    ]
                ]
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


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Browser.Events.onVisibilityChange WindowEndrerVisibility
        , model.seksjonsMeldingsLogg
            |> SamtaleAnimasjon.subscriptions
            |> Sub.map SamtaleAnimasjonMsg
        ]
