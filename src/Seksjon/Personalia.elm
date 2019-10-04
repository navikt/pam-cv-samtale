module Seksjon.Personalia exposing
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
import DebugStatus exposing (DebugStatus)
import Feilmelding
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Encode
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Personalia exposing (Personalia)
import Poststed exposing (Poststed)
import Process
import SamtaleAnimasjon
import Skjema.Personalia exposing (PersonaliaSkjema, ValidertPersonaliaSkjema)
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
    | LagrerEndring PersonaliaSkjema
    | LagringFeilet Http.Error PersonaliaSkjema
    | VenterPåAnimasjonFørFullføring Personalia


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
    | PersonaliaSkjemaEndret Skjema.Personalia.Felt String
    | PoststedHentet String (Result Http.Error Poststed)
    | PoststedfeltEndretSelvOmDetErDisabled String
    | PersonaliaskjemaLagreknappTrykket ValidertPersonaliaSkjema
    | PersonaliaskjemaLagreknappTrykketSelvOmDenErDisabled
    | PersonaliaOppdatert (Result Http.Error Personalia)
    | ViewportSatt (Result Dom.Error ())
    | StartÅSkrive
    | FullførMelding
    | ErrorLogget


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        OriginalPersonaliaBekreftet ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (Melding.svar [ "Ja, informasjonen er riktig" ])
                            |> MeldingsLogg.leggTilSpørsmål
                                [ Melding.spørsmål [ "Så bra! 😊 Nå kan arbeidsgivere kontakte deg." ]
                                , Melding.spørsmål [ "Da kan vi gå videre til utfylling av CV-en." ]
                                ]
                    , aktivSamtale = VenterPåAnimasjonFørFullføring model.personalia
                }
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilEndreOriginalPersonalia ->
            ( model.personalia
                |> Skjema.Personalia.init
                |> EndreOriginal
                |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        PersonaliaSkjemaEndret felt string ->
            case model.aktivSamtale of
                EndreOriginal skjema ->
                    ( Skjema.Personalia.oppdaterFelt felt skjema string
                        |> EndreOriginal
                        |> oppdaterSamtaleSteg model
                    , case felt of
                        Skjema.Personalia.Postnummer ->
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

        PoststedHentet postnummer result ->
            case result of
                Ok poststed ->
                    case model.aktivSamtale of
                        EndreOriginal skjema ->
                            ( skjema
                                |> Skjema.Personalia.oppdaterPoststed poststed
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

        PersonaliaskjemaLagreknappTrykket validertSkjema ->
            case model.aktivSamtale of
                EndreOriginal skjema ->
                    ( skjema
                        |> LagrerEndring
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

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        PersonaliaskjemaLagreknappTrykketSelvOmDenErDisabled ->
            IkkeFerdig ( Model model, Cmd.none )

        PersonaliaOppdatert result ->
            case model.aktivSamtale of
                LagrerEndring skjema ->
                    case result of
                        Ok personalia ->
                            fullførSeksjonHvisMeldingsloggErFerdig model personalia

                        Err error ->
                            ( LagringFeilet error skjema
                                |> nesteSamtaleSteg model (Melding.spørsmål [ "Noe gikk galt" ])
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
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        ViewportSatt _ ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

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


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring personalia ->
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


fullførSeksjonHvisMeldingsloggErFerdig : ModelInfo -> Personalia -> SamtaleStatus
fullførSeksjonHvisMeldingsloggErFerdig modelInfo personalia =
    case MeldingsLogg.ferdigAnimert modelInfo.seksjonsMeldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            Ferdig personalia ferdigAnimertMeldingsLogg

        MeldingerGjenstår ->
            ( Model { modelInfo | aktivSamtale = VenterPåAnimasjonFørFullføring personalia }, Cmd.none )
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
            , Melding.spørsmål [ "Jeg har hentet inn kontaktinformasjonen din, den vises på CV-en. Det er viktig at informasjonen er riktig, slik at arbeidsgivere kan kontakte deg." ]
            , Melding.spørsmål
                (List.concat
                    [ personalia
                        |> Skjema.Personalia.init
                        |> personaliaSkjemaOppsummering
                    , [ Melding.tomLinje
                      , "Er kontaktinformasjonen riktig?"
                      ]
                    ]
                )
            ]

        EndreOriginal _ ->
            [ Melding.spørsmål
                [ "Ok! Vennligst skriv inn riktig informasjon i feltene under:" ]
            ]

        LagrerEndring _ ->
            [ Melding.spørsmål
                [ "Godt jobbet! Da tar jeg vare på den nye infoen!" ]
            ]

        LagringFeilet _ _ ->
            [ Melding.spørsmål
                [ "Oops.. Noe gikk galt!" ]
            ]

        VenterPåAnimasjonFørFullføring _ ->
            []


personaliaSkjemaOppsummering : PersonaliaSkjema -> List String
personaliaSkjemaOppsummering skjema =
    [ "Navn: " ++ Skjema.Personalia.fornavn skjema ++ " " ++ Skjema.Personalia.etternavn skjema
    , "Fødselsdato: " ++ (skjema |> Skjema.Personalia.fodselsdato |> viewDatoString)
    , "E-post: " ++ Skjema.Personalia.epost skjema
    , "Telefonnummer: " ++ Skjema.Personalia.telefon skjema
    , "Adresse: "
        ++ Skjema.Personalia.gateadresse skjema
        ++ " "
        ++ Skjema.Personalia.postnummer skjema
        ++ " "
        ++ Skjema.Personalia.poststed skjema
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
                    -- TODO: Endre til og bruke Container og til at feilmeldinger kun kommer etter blur/lagring
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ personaliaSkjema
                                |> Skjema.Personalia.fornavn
                                |> Input.input { label = "Fornavn*", msg = PersonaliaSkjemaEndret Skjema.Personalia.Fornavn }
                                |> Input.withMaybeFeilmelding (Skjema.Personalia.fornavnFeilmelding personaliaSkjema)
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.Personalia.etternavn
                                |> Input.input { label = "Etternavn*", msg = PersonaliaSkjemaEndret Skjema.Personalia.Etternavn }
                                |> Input.withMaybeFeilmelding (Skjema.Personalia.etternavnFeilmelding personaliaSkjema)
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.Personalia.epost
                                |> Input.input { label = "E-post*", msg = PersonaliaSkjemaEndret Skjema.Personalia.Epost }
                                |> Input.withMaybeFeilmelding (Skjema.Personalia.epostFeilmelding personaliaSkjema)
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.Personalia.telefon
                                |> Input.input { label = "Telefon*", msg = PersonaliaSkjemaEndret Skjema.Personalia.Telefon }
                                |> Input.withMaybeFeilmelding (Skjema.Personalia.telefonFeilmelding personaliaSkjema)
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.Personalia.gateadresse
                                |> Input.input { label = "Gateadresse", msg = PersonaliaSkjemaEndret Skjema.Personalia.Gateadresse }
                                |> Input.toHtml
                            , div [ class "PersonaliaSeksjon-poststed" ]
                                [ personaliaSkjema
                                    |> Skjema.Personalia.postnummer
                                    |> Input.input { label = "Postnummer", msg = PersonaliaSkjemaEndret Skjema.Personalia.Postnummer }
                                    |> Input.withMaybeFeilmelding (Skjema.Personalia.postnummerFeilmelding personaliaSkjema)
                                    |> Input.toHtml
                                , personaliaSkjema
                                    |> Skjema.Personalia.poststed
                                    |> Input.input { label = "Poststed", msg = PoststedfeltEndretSelvOmDetErDisabled }
                                    |> Input.withEnabled Input.Disabled
                                    |> Input.toHtml
                                ]
                            , case Skjema.Personalia.validerSkjema personaliaSkjema of
                                Just validertSkjema ->
                                    div []
                                        [ Knapp.knapp (PersonaliaskjemaLagreknappTrykket validertSkjema) "Lagre endringer"
                                            |> Knapp.toHtml
                                        ]

                                Nothing ->
                                    div []
                                        [ Knapp.knapp PersonaliaskjemaLagreknappTrykketSelvOmDenErDisabled "Lagre endringer"
                                            |> Knapp.withEnabled Knapp.Disabled
                                            |> Knapp.toHtml
                                        ]
                            ]
                        ]

                LagrerEndring _ ->
                    text ""

                LagringFeilet _ _ ->
                    text ""

                VenterPåAnimasjonFørFullføring _ ->
                    text ""

        MeldingerGjenstår ->
            text ""



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
