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
import Dato exposing (År)
import Dato.Maned as Måned exposing (Måned(..))
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp exposing (Knapp)
import FrontendModuler.LoggInnLenke as LoggInnLenke
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
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
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


type AvsluttetGrunn
    = SlettetPåbegynt
    | AnnenAvslutning


type OppsummeringsType
    = FørsteGang
    | EtterEndring
    | AvbrøtSletting


type Samtale
    = RegistrerKursnavn KursnavnInfo
    | RegistrerKursholder KursholderInfo
    | SpørOmBrukerVilLeggeInnFullførtDato KursholderInfo
    | RegistrerFullførtMåned FullførtDatoInfo
    | RegistrerFullførtÅr FullførtDatoInfo
    | RegistrerVarighetEnhet VarighetInfo
    | RegistrerVarighet VarighetInfo
    | VisOppsummering OppsummeringsType ValidertKursSkjema
    | EndreOpplysninger KursSkjema
    | BekreftSlettingAvPåbegynt ValidertKursSkjema
    | LagrerSkjema ValidertKursSkjema LagreStatus
    | LagringFeilet Http.Error ValidertKursSkjema
    | VenterPåAnimasjonFørFullføring (List Kurs) AvsluttetGrunn


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
    | FullførtMånedValgt Måned
    | VilRegistrereFullførtÅr
    | OppdatererFullførtÅr String
    | VarighetEnhetValgt VarighetEnhet
    | VilRegistrereVarighet
    | OppdatererVarighet String
    | VilLagreKurs
    | VilEndreOpplysninger
    | SkjemaEndret SkjemaEndring
    | VilSlettePåbegynt
    | BekrefterSlettPåbegynt
    | AngrerSlettPåbegynt
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
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( info.kursnavn
                                |> kursnavnTilKursholder
                                |> RegistrerKursholder
                                |> oppdaterSamtale model (SvarFraMsg msg)
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
                        |> oppdaterSamtale model IngenNyeMeldinger
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
                                |> oppdaterSamtale model (SvarFraMsg msg)
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
                        |> oppdaterSamtale model IngenNyeMeldinger
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
                        |> oppdaterSamtale model (SvarFraMsg msg)
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
                        |> oppdaterSamtale model (SvarFraMsg msg)
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
                        |> oppdaterSamtale model (SvarFraMsg msg)
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
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { fullførtDatoInfo | tillatÅViseFeilmeldingÅr = True }
                                |> RegistrerFullførtÅr
                                |> oppdaterSamtale model IngenNyeMeldinger
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
                        |> oppdaterSamtale model IngenNyeMeldinger
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
                        |> oppdaterSamtale model (SvarFraMsg msg)
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
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( info
                                |> varighetTilSkjema
                                |> VisOppsummering FørsteGang
                                |> oppdaterSamtale model (SvarFraMsg msg)
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
                        |> oppdaterSamtale model IngenNyeMeldinger
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
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerFullførtÅr fullførtDatoInfo ->
                    ( { fullførtDatoInfo | tillatÅViseFeilmeldingÅr = True }
                        |> RegistrerFullførtÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerVarighet varighetInfo ->
                    ( { varighetInfo | tillatÅViseFeilmeldingVarighet = True }
                        |> RegistrerVarighet
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilEndreOpplysninger ->
            case model.aktivSamtale of
                VisOppsummering _ validertKursSkjema ->
                    updateEtterVilEndreSkjema model msg validertKursSkjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreOpplysninger kursSkjema ->
                    ( kursSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> EndreOpplysninger
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilSlettePåbegynt ->
            case model.aktivSamtale of
                VisOppsummering _ skjema ->
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
                    ( VenterPåAnimasjonFørFullføring model.kursListe SlettetPåbegynt
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        AngrerSlettPåbegynt ->
            case model.aktivSamtale of
                BekreftSlettingAvPåbegynt skjema ->
                    ( VisOppsummering AvbrøtSletting skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
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
                                |> VisOppsummering EtterEndring
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar (validertSkjemaTilSetninger validertSkjema)))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> Skjema.tillatÅViseAlleFeilmeldinger
                                |> EndreOpplysninger
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreKurs ->
            case model.aktivSamtale of
                VisOppsummering _ skjema ->
                    updateEtterLagreKnappTrykket model msg skjema

                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerSkjema skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
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
                            ( VenterPåAnimasjonFørFullføring kurs AnnenAvslutning
                                |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg } UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , Api.postKurs KursLagret skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre kurs" error
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
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre kurs" error
                                    ]
                                )
                                    |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FerdigMedKurs ->
            case model.aktivSamtale of
                LagringFeilet _ _ ->
                    ( VenterPåAnimasjonFørFullføring model.kursListe AnnenAvslutning
                        |> oppdaterSamtale model (SvarFraMsg msg)
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
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagringFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
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


setFullførtMåned : FullførtDatoInfo -> Måned -> FullførtDatoInfo
setFullførtMåned fullførtDatoInfo måned =
    { fullførtDatoInfo | fullførtMåned = måned }


oppdaterSkjema : SkjemaEndring -> KursSkjema -> KursSkjema
oppdaterSkjema endring skjema =
    case endring of
        Tekst felt innhold ->
            Skjema.oppdaterTekstFelt felt innhold skjema

        FullførtMåned månedString ->
            månedString
                |> Måned.fraString
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
                VenterPåAnimasjonFørFullføring kursListe _ ->
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


updateEtterVilEndreSkjema : ModelInfo -> Msg -> ValidertKursSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model msg skjema =
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> EndreOpplysninger
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> Msg -> ValidertKursSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model msg skjema =
    ( LagreStatus.init
        |> LagrerSkjema skjema
        |> oppdaterSamtale model (SvarFraMsg msg)
    , Api.postKurs KursLagret skjema
    )
        |> IkkeFerdig


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


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

        VisOppsummering oppsummeringsType validertSkjema ->
            case oppsummeringsType of
                AvbrøtSletting ->
                    [ Melding.spørsmål [ "Ok, da lar jeg kurset stå." ]
                    , oppsummeringsSpørsmål validertSkjema
                    ]

                EtterEndring ->
                    [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

                FørsteGang ->
                    [ oppsummeringsSpørsmål validertSkjema
                    ]

        EndreOpplysninger _ ->
            [ Melding.spørsmål [ "Gjør endringene du ønsker i feltene under." ] ]

        BekreftSlettingAvPåbegynt _ ->
            [ Melding.spørsmål [ "Er du sikker på at du vil slette dette kurset?" ] ]

        LagrerSkjema _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre kurs" } ]

        VenterPåAnimasjonFørFullføring _ avsluttetGrunn ->
            case avsluttetGrunn of
                SlettetPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg slettet kurset. Vil du legge inn flere kategorier?" ] ]

                _ ->
                    [ Melding.spørsmål [ "Vil du legge inn flere kategorier?" ] ]


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


oppsummeringsSpørsmål : ValidertKursSkjema -> Melding
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
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput model =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            RegistrerKursnavn info ->
                BrukerInput.inputMedGåVidereKnapp VilRegistrereKursnavn
                    (info.kursnavn
                        |> Input.input { label = "Kursets navn", msg = OppdatererKursnavn }
                        |> Input.withOnEnter VilRegistrereKursnavn
                        |> Input.withId (inputIdTilString KursnavnId)
                        |> Input.withOnBlur FeltMisterFokus
                        |> Input.withFeilmelding
                            (info.kursnavn
                                |> Skjema.feilmeldingKursnavn
                                |> maybeHvisTrue info.tillatÅViseFeilmeldingKursnavn
                            )
                        |> Input.withErObligatorisk
                    )

            RegistrerKursholder info ->
                BrukerInput.inputMedGåVidereKnapp VilRegistrereKursholder
                    (info.kursholder
                        |> Input.input { label = "Kursets arrangør", msg = OppdatererKursholder }
                        |> Input.withOnEnter VilRegistrereKursholder
                        |> Input.withFeilmelding (Skjema.feilmeldingKursholder info.kursholder)
                        |> Input.withId (inputIdTilString KursholderId)
                    )

            SpørOmBrukerVilLeggeInnFullførtDato _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp SvarerJaTilFullførtDato "Ja, det vil jeg"
                    , Knapp.knapp SvarerNeiTilFullførtDato "Nei, det vil jeg ikke"
                    ]

            RegistrerFullførtMåned _ ->
                BrukerInput.månedKnapper FullførtMånedValgt

            RegistrerFullførtÅr fullførtDatoInfo ->
                BrukerInput.inputMedGåVidereKnapp VilRegistrereFullførtÅr
                    (fullførtDatoInfo.fullførtÅr
                        |> Input.input { label = "År", msg = OppdatererFullførtÅr }
                        |> Input.withClass "aar"
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withOnEnter VilRegistrereFullførtÅr
                        |> Input.withOnBlur FeltMisterFokus
                        |> Input.withId (inputIdTilString FullførtÅrId)
                        |> Input.withFeilmelding
                            (fullførtDatoInfo.fullførtÅr
                                |> Dato.feilmeldingÅr
                                |> maybeHvisTrue fullførtDatoInfo.tillatÅViseFeilmeldingÅr
                            )
                        |> Input.withErObligatorisk
                    )

            RegistrerVarighetEnhet _ ->
                varighetEnhetKnapper

            RegistrerVarighet info ->
                BrukerInput.inputMedGåVidereKnapp VilRegistrereVarighet
                    (info.varighet
                        |> Input.input { label = "Antall", msg = OppdatererVarighet }
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withOnEnter VilRegistrereVarighet
                        |> Input.withOnBlur FeltMisterFokus
                        |> Input.withId (inputIdTilString VarighetId)
                        |> Input.withFeilmelding
                            (info.varighet
                                |> Skjema.feilmeldingVarighet
                                |> maybeHvisTrue info.tillatÅViseFeilmeldingVarighet
                            )
                    )

            VisOppsummering _ _ ->
                viewBekreftOppsummering

            EndreOpplysninger skjema ->
                BrukerInput.skjema { lagreMsg = VilLagreEndretSkjema, lagreKnappTekst = "Lagre endringer" }
                    [ skjema
                        |> Skjema.innholdTekstFelt Kursnavn
                        |> Input.input { label = "Kursnavn", msg = Tekst Kursnavn >> SkjemaEndret }
                        |> Input.withFeilmelding (Skjema.feilmeldingKursnavnHvisSynlig skjema)
                        |> Input.withOnBlur (SkjemaEndret KursnavnBlurred)
                        |> Input.withErObligatorisk
                        |> Input.toHtml
                    , skjema
                        |> Skjema.innholdTekstFelt Kursholder
                        |> Input.input { label = "Kursholder", msg = Tekst Kursholder >> SkjemaEndret }
                        |> Input.withFeilmelding (Skjema.innholdTekstFelt Kursholder skjema |> Skjema.feilmeldingKursholder)
                        |> Input.toHtml
                    , div [ class "DatoInput-fra-til-rad" ]
                        [ ValgfriDatoInput.datoInput
                            { label = "Når avsluttet du kurset?"
                            , onMånedChange = FullførtMåned >> SkjemaEndret
                            , måned = Skjema.fullførtMåned skjema
                            , onÅrChange = Tekst FullførtÅr >> SkjemaEndret
                            , år = Skjema.innholdTekstFelt FullførtÅr skjema
                            }
                            |> ValgfriDatoInput.withFeilmeldingÅr (Skjema.feilmeldingValgfrittFullførtÅr skjema)
                            |> ValgfriDatoInput.withFeilmeldingPeriode (Skjema.feilmeldingPeriode skjema)
                            |> ValgfriDatoInput.withOnBlurÅr (SkjemaEndret FullførtÅrBlurred)
                            |> ValgfriDatoInput.toHtml
                        , viewVarighet skjema
                        ]
                    ]

            BekreftSlettingAvPåbegynt _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BekrefterSlettPåbegynt "Ja, jeg vil slette"
                    , Knapp.knapp AngrerSlettPåbegynt "Nei, jeg vil ikke slette"
                    ]

            LagrerSkjema _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    BrukerInput.utenInnhold

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    ErrorHåndtering.GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp FerdigMedKurs "Gå videre" ]

                    ErrorHåndtering.PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp VilLagreKurs "Prøv igjen"
                            , Knapp.knapp FerdigMedKurs "Gå videre"
                            ]

                    ErrorHåndtering.LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            VenterPåAnimasjonFørFullføring _ _ ->
                BrukerInput.utenInnhold

    else
        BrukerInput.utenInnhold


varighetEnhetKnapper : BrukerInput Msg
varighetEnhetKnapper =
    BrukerInput.knapper VarighetGrid
        (List.map varighetEnhetKnapp
            [ Time
            , Dag
            , Uke
            , Måned
            ]
        )


varighetEnhetKnapp : VarighetEnhet -> Knapp Msg
varighetEnhetKnapp enhet =
    enhet
        |> Skjema.varighetEnhetTilString
        |> Knapp.knapp (VarighetEnhetValgt enhet)


viewBekreftOppsummering : BrukerInput Msg
viewBekreftOppsummering =
    BrukerInput.knapper Kolonne
        [ Knapp.knapp VilLagreKurs "Ja, det er riktig"
        , Knapp.knapp VilEndreOpplysninger "Nei, jeg vil endre"
        , Knapp.knapp VilSlettePåbegynt "Nei, jeg vil slette"
        ]


viewVarighet : KursSkjema -> Html Msg
viewVarighet skjema =
    fieldset [ class "DatoInput-fieldset" ]
        [ legend [ class "skjemaelement__label" ]
            [ text "Hvor lenge varte kurset?" ]
        , div [ class "Varighet-wrapper skjemaelement" ]
            [ Skjema.innholdTekstFelt Varighet skjema
                |> Input.input { label = "Antall", msg = Tekst Varighet >> SkjemaEndret }
                |> Input.withClass "Varighet-antall"
                |> Input.withOnBlur (SkjemaEndret VarighetBlurred)
                |> Input.withFeilmelding (Skjema.feilmeldingVarighetHvisSynlig skjema)
                |> Input.toHtml
            , Select.select
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
