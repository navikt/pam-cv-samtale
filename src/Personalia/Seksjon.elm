module Personalia.Seksjon exposing
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
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Feilmelding
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onInput)
import Http exposing (Error(..))
import Json.Encode
import LagreStatus exposing (LagreStatus)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Personalia exposing (Personalia)
import Personalia.Skjema as Skjema exposing (PersonaliaSkjema, ValidertPersonaliaSkjema)
import Poststed exposing (Poststed)
import Process
import SamtaleAnimasjon
import Task



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , personalia : Personalia
    , debugStatus : DebugStatus
    }


type Samtale
    = BekreftOriginal Personalia
    | EndreOriginal PersonaliaSkjema
    | LagrerEndring ValidertPersonaliaSkjema LagreStatus
    | LagringFeilet Http.Error ValidertPersonaliaSkjema
    | VenterPåAnimasjonFørFullføring Personalia FullføringStatus


type FullføringStatus
    = BekreftetOriginal
    | LagringLyktesFørsteGang
    | LagringLyktesEtterFlereForsøk
    | BrukerGikkVidere


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Personalia FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



--- UPDATE ---


type Msg
    = OriginalPersonaliaBekreftet
    | BrukerVilEndreOriginalPersonalia
    | PersonaliaSkjemaEndret Skjema.Felt String
    | PersonaliaSkjemaFeltMistetFokus Skjema.Felt
    | PoststedHentet String (Result Http.Error Poststed)
    | PoststedfeltEndretSelvOmDetErDisabled String
    | PersonaliaskjemaLagreknappTrykket
    | PersonaliaOppdatert (Result Http.Error Personalia)
    | BrukerVilGåVidereUtenÅLagre
    | BrukerVilPrøveÅLagrePåNytt
    | WindowEndrerVisibility Visibility
    | ViewportSatt (Result Dom.Error ())
    | StartÅSkrive
    | FullførMelding
    | ErrorLogget


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        OriginalPersonaliaBekreftet ->
            BekreftetOriginal
                |> VenterPåAnimasjonFørFullføring model.personalia
                |> nesteSamtaleSteg model (Melding.svar [ "Ja, informasjonen er riktig" ])
                |> fullførSeksjonHvisMeldingsloggErFerdig model.personalia

        BrukerVilEndreOriginalPersonalia ->
            ( model.personalia
                |> Skjema.init
                |> EndreOriginal
                |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        PersonaliaSkjemaEndret felt string ->
            case model.aktivSamtale of
                EndreOriginal skjema ->
                    ( Skjema.oppdaterFelt felt skjema string
                        |> EndreOriginal
                        |> oppdaterSamtaleSteg model
                    , case felt of
                        Skjema.Postnummer ->
                            if String.length string == 4 && String.toInt string /= Nothing then
                                Api.hentPoststed (PoststedHentet string) string

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        PersonaliaSkjemaFeltMistetFokus felt ->
            case model.aktivSamtale of
                EndreOriginal skjema ->
                    ( felt
                        |> Skjema.gjørFeilmeldingSynligForFelt skjema
                        |> EndreOriginal
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        PoststedHentet postnummer result ->
            case result of
                Ok poststed ->
                    case model.aktivSamtale of
                        EndreOriginal skjema ->
                            ( skjema
                                |> Skjema.oppdaterPoststed poststed
                                |> EndreOriginal
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Err error ->
                    case error of
                        Http.BadStatus 404 ->
                            IkkeFerdig ( Model model, Cmd.none )

                        _ ->
                            IkkeFerdig
                                ( Model model
                                , error
                                    |> Feilmelding.feilmelding ("Hent poststed for postnummer: \"" ++ postnummer ++ "\"")
                                    |> Maybe.map (Api.logError (always ErrorLogget))
                                    |> Maybe.withDefault Cmd.none
                                )

        PoststedfeltEndretSelvOmDetErDisabled _ ->
            IkkeFerdig ( Model model, Cmd.none )

        PersonaliaskjemaLagreknappTrykket ->
            case model.aktivSamtale of
                EndreOriginal skjema ->
                    case Skjema.validerSkjema skjema of
                        Just validertSkjema ->
                            ( LagreStatus.init
                                |> LagrerEndring validertSkjema
                                |> nesteSamtaleSteg model
                                    (skjema
                                        |> personaliaSkjemaOppsummering
                                        |> Melding.svar
                                    )
                            , Cmd.batch
                                [ model.personalia
                                    |> Personalia.id
                                    |> Api.putPersonalia PersonaliaOppdatert validertSkjema
                                , lagtTilSpørsmålCmd model.debugStatus
                                ]
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> Skjema.gjørAlleFeilmeldingerSynlig
                                |> EndreOriginal
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        PersonaliaOppdatert result ->
            case model.aktivSamtale of
                LagrerEndring skjema lagreStatus ->
                    case result of
                        Ok personalia ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                lagreStatus
                                    |> fullførtStatusEtterOkLagring
                                    |> VenterPåAnimasjonFørFullføring personalia
                                    |> nesteSamtaleSteg model (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])
                                    |> fullførSeksjonHvisMeldingsloggErFerdig personalia

                            else
                                lagreStatus
                                    |> fullførtStatusEtterOkLagring
                                    |> VenterPåAnimasjonFørFullføring personalia
                                    |> nesteSamtaleStegUtenSvar model
                                    |> fullførSeksjonHvisMeldingsloggErFerdig personalia

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerEndring skjema
                                        |> oppdaterSamtaleSteg model
                                    , model.personalia
                                        |> Personalia.id
                                        |> Api.putPersonalia PersonaliaOppdatert skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtaleSteg model
                                    , Cmd.none
                                    )
                                        |> IkkeFerdig

                            else
                                ( skjema
                                    |> LagringFeilet error
                                    |> nesteSamtaleStegUtenSvar model
                                , Cmd.batch
                                    [ lagtTilSpørsmålCmd model.debugStatus
                                    , model.personalia
                                        |> Personalia.id
                                        |> Json.Encode.string
                                        |> Tuple.pair "id"
                                        |> List.singleton
                                        |> Json.Encode.object
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre personalia" error
                                    ]
                                )
                                    |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilPrøveÅLagrePåNytt ->
            case model.aktivSamtale of
                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerEndring skjema
                        |> nesteSamtaleSteg model (Melding.svar [ "Prøv på nytt" ])
                    , Cmd.batch
                        [ model.personalia
                            |> Personalia.id
                            |> Api.putPersonalia PersonaliaOppdatert skjema
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilGåVidereUtenÅLagre ->
            VenterPåAnimasjonFørFullføring model.personalia BrukerGikkVidere
                |> nesteSamtaleSteg model (Melding.svar [ "Gå videre" ])
                |> fullførSeksjonHvisMeldingsloggErFerdig model.personalia

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case model.aktivSamtale of
                        LagringFeilet error skjema ->
                            ( error
                                |> LagreStatus.fraError
                                |> LagrerEndring skjema
                                |> oppdaterSamtaleSteg model
                            , model.personalia
                                |> Personalia.id
                                |> Api.putPersonalia PersonaliaOppdatert skjema
                            )
                                |> IkkeFerdig

                        LagrerEndring skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerEndring skjema
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Hidden ->
                    IkkeFerdig ( Model model, Cmd.none )

        ViewportSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )

        StartÅSkrive ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        MeldingsLogg.startÅSkrive model.seksjonsMeldingsLogg
                }
            , Cmd.batch
                [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                , MeldingsLogg.nesteMeldingToString model.seksjonsMeldingsLogg
                    * 1000.0
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

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )


fullførtStatusEtterOkLagring : LagreStatus -> FullføringStatus
fullførtStatusEtterOkLagring lagreStatus =
    if LagreStatus.lagrerPåFørsteForsøk lagreStatus then
        LagringLyktesFørsteGang

    else
        LagringLyktesEtterFlereForsøk


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring personalia _ ->
                    Ferdig personalia ferdigAnimertSamtale

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
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig


fullførSeksjonHvisMeldingsloggErFerdig : Personalia -> Model -> SamtaleStatus
fullførSeksjonHvisMeldingsloggErFerdig personalia (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            Ferdig personalia ferdigAnimertMeldingsLogg

        MeldingerGjenstår ->
            ( Model model, lagtTilSpørsmålCmd model.debugStatus )
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
nesteSamtaleSteg model melding samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


nesteSamtaleStegUtenSvar : ModelInfo -> Samtale -> Model
nesteSamtaleStegUtenSvar model samtaleSeksjon =
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
samtaleTilMeldingsLogg personaliaSeksjon =
    case personaliaSeksjon of
        BekreftOriginal personalia ->
            [ Melding.spørsmål [ "Da setter vi i gang 😊" ]
            , Melding.spørsmål [ "Jeg har hentet inn kontaktinformasjonen din, den vises på CV-en. Sjekk at den er riktig, slik at arbeidsgivere kan kontakte deg." ]
            , Melding.spørsmål
                (List.concat
                    [ personalia
                        |> Skjema.init
                        |> personaliaSkjemaOppsummering
                    , [ Melding.tomLinje
                      , "Er kontaktinformasjonen riktig?"
                      ]
                    ]
                )
            ]

        EndreOriginal _ ->
            [ Melding.spørsmål [ "Ok! Skriv inn riktig informasjon i feltene under." ] ]

        LagrerEndring _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { operasjon = "lagre kontaktinformasjonen", error = error } ]

        VenterPåAnimasjonFørFullføring _ fullføringStatus ->
            case fullføringStatus of
                BekreftetOriginal ->
                    [ Melding.spørsmål [ "Så bra! 😊 Nå kan arbeidsgivere kontakte deg." ]
                    , Melding.spørsmål [ "Da går vi videre til utdanning." ]
                    ]

                LagringLyktesFørsteGang ->
                    [ Melding.spørsmål [ "Da har du endret👍 Er det riktig nå?" ] ]

                LagringLyktesEtterFlereForsøk ->
                    [ Melding.spørsmål [ "Supert! Nå fikk jeg det til. Kontaktinformasjonen er lagret. La oss fortsette 😊" ] ]

                BrukerGikkVidere ->
                    [ Melding.spørsmål [ "Da går vi videre" ] ]


personaliaSkjemaOppsummering : PersonaliaSkjema -> List String
personaliaSkjemaOppsummering skjema =
    [ "Navn: " ++ Skjema.fornavn skjema ++ " " ++ Skjema.etternavn skjema
    , "Fødselsdato: " ++ (skjema |> Skjema.fodselsdato |> viewDatoString)
    , "E-post: " ++ Skjema.epost skjema
    , "Telefonnummer: " ++ Skjema.telefon skjema
    , "Adresse: "
        ++ Skjema.gateadresse skjema
        ++ " "
        ++ Skjema.postnummer skjema
        ++ " "
        ++ Skjema.poststed skjema
    ]


viewDatoString : String -> String
viewDatoString datoString =
    case String.split "-" datoString of
        år :: måned :: dag :: [] ->
            dag ++ "." ++ måned ++ "." ++ år

        _ ->
            datoString



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model { aktivSamtale, seksjonsMeldingsLogg }) =
    case MeldingsLogg.ferdigAnimert seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case aktivSamtale of
                BekreftOriginal _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp OriginalPersonaliaBekreftet "Ja, informasjonen er riktig"
                            |> Knapp.toHtml
                        , Knapp.knapp BrukerVilEndreOriginalPersonalia "Nei, jeg vil endre"
                            |> Knapp.toHtml
                        ]

                EndreOriginal personaliaSkjema ->
                    Containers.skjema { lagreMsg = PersonaliaskjemaLagreknappTrykket, lagreKnappTekst = "Lagre endringer" }
                        [ personaliaSkjema
                            |> Skjema.fornavn
                            |> Input.input { label = "Fornavn*", msg = PersonaliaSkjemaEndret Skjema.Fornavn }
                            |> Input.withMaybeFeilmelding (Skjema.fornavnFeilmelding personaliaSkjema)
                            |> Input.withOnBlur (PersonaliaSkjemaFeltMistetFokus Skjema.Fornavn)
                            |> Input.toHtml
                        , personaliaSkjema
                            |> Skjema.etternavn
                            |> Input.input { label = "Etternavn*", msg = PersonaliaSkjemaEndret Skjema.Etternavn }
                            |> Input.withMaybeFeilmelding (Skjema.etternavnFeilmelding personaliaSkjema)
                            |> Input.withOnBlur (PersonaliaSkjemaFeltMistetFokus Skjema.Etternavn)
                            |> Input.toHtml
                        , personaliaSkjema
                            |> Skjema.epost
                            |> Input.input { label = "E-post*", msg = PersonaliaSkjemaEndret Skjema.Epost }
                            |> Input.withMaybeFeilmelding (Skjema.epostFeilmelding personaliaSkjema)
                            |> Input.withOnBlur (PersonaliaSkjemaFeltMistetFokus Skjema.Epost)
                            |> Input.toHtml
                        , viewTelefonISkjema personaliaSkjema
                        , personaliaSkjema
                            |> Skjema.gateadresse
                            |> Input.input { label = "Gateadresse", msg = PersonaliaSkjemaEndret Skjema.Gateadresse }
                            |> Input.toHtml
                        , div [ class "PersonaliaSeksjon-poststed" ]
                            [ personaliaSkjema
                                |> Skjema.postnummer
                                |> Input.input { label = "Postnummer", msg = PersonaliaSkjemaEndret Skjema.Postnummer }
                                |> Input.withMaybeFeilmelding (Skjema.postnummerFeilmelding personaliaSkjema)
                                |> Input.withOnBlur (PersonaliaSkjemaFeltMistetFokus Skjema.Postnummer)
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.poststed
                                |> Input.input { label = "Poststed", msg = PoststedfeltEndretSelvOmDetErDisabled }
                                |> Input.withEnabled Input.Disabled
                                |> Input.toHtml
                            ]
                        ]

                -- Lenken for å logge seg inn skal alltid være synlig hvis man har blitt utlogget, selv under lagring
                LagrerEndring _ lagreStatus ->
                    if LagreStatus.lagrerEtterUtlogging lagreStatus then
                        LoggInnLenke.viewLoggInnLenke

                    else
                        text ""

                LagringFeilet error _ ->
                    case ErrorHåndtering.operasjonEtterError error of
                        GiOpp ->
                            Containers.knapper Flytende
                                [ Knapp.knapp BrukerVilGåVidereUtenÅLagre "Gå videre"
                                    |> Knapp.toHtml
                                ]

                        PrøvPåNytt ->
                            Containers.knapper Flytende
                                [ Knapp.knapp BrukerVilPrøveÅLagrePåNytt "Prøv på nytt"
                                    |> Knapp.toHtml
                                , Knapp.knapp BrukerVilGåVidereUtenÅLagre "Gå videre"
                                    |> Knapp.toHtml
                                ]

                        LoggInn ->
                            LoggInnLenke.viewLoggInnLenke

                VenterPåAnimasjonFørFullføring _ _ ->
                    text ""

        MeldingerGjenstår ->
            text ""


viewTelefonISkjema : PersonaliaSkjema -> Html Msg
viewTelefonISkjema personaliaSkjema =
    div [ class "skjemaelement" ]
        [ label []
            [ span [ class "skjemaelement__label" ] [ text "Telefon*" ]
            , div [ class "PersonaliaSeksjon--telefonnummer" ]
                [ p [ class "PersonaliaSeksjon--telefonnummer--country-code" ] [ text "+47" ]
                , input
                    [ value (Skjema.telefon personaliaSkjema)
                    , onInput (PersonaliaSkjemaEndret Skjema.Telefon)
                    , onBlur (PersonaliaSkjemaFeltMistetFokus Skjema.Telefon)
                    , classList
                        [ ( "skjemaelement__input", True )
                        , ( "PersonaliaSeksjon--telefonnummer--input", True )
                        , ( "skjemaelement__input--harFeil", Skjema.telefonFeilmelding personaliaSkjema /= Nothing )
                        ]
                    ]
                    []
                ]
            , case Skjema.telefonFeilmelding personaliaSkjema of
                Just feilmelding ->
                    div [ class "skjemaelement__feilmelding" ] [ text feilmelding ]

                Nothing ->
                    text ""
            ]
        ]



--- INIT ---


init : DebugStatus -> Personalia -> MeldingsLogg -> ( Model, Cmd Msg )
init debugStatus personalia gammelMeldingsLogg =
    let
        aktivSamtale =
            BekreftOriginal personalia
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål
                (samtaleTilMeldingsLogg aktivSamtale)
                gammelMeldingsLogg
        , aktivSamtale = aktivSamtale
        , personalia = personalia
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onVisibilityChange WindowEndrerVisibility
