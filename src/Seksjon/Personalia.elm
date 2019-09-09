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
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Encode
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Personalia exposing (Personalia)
import Process
import SamtaleAnimasjon
import Skjema.Personalia exposing (PersonaliaSkjema)
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
    | PersonaliaskjemaLagreknappTrykket
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
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        PersonaliaskjemaLagreknappTrykket ->
            case model.aktivSamtale of
                EndreOriginal skjema ->
                    ( skjema
                        |> LagrerEndring
                        |> nesteSamtaleSteg model
                            (Melding.svar
                                [ "Navn: " ++ Skjema.Personalia.fornavn skjema ++ " " ++ Skjema.Personalia.etternavn skjema
                                , "Fødselsdato " ++ Skjema.Personalia.fodselsdato skjema
                                , "E-post: " ++ Skjema.Personalia.epost skjema
                                , "Telefonnummer: " ++ Skjema.Personalia.telefon skjema
                                , "Adresse: "
                                    ++ Skjema.Personalia.gateadresse skjema
                                    ++ " "
                                    ++ Skjema.Personalia.postnummer skjema
                                    ++ " "
                                    ++ Skjema.Personalia.poststed skjema
                                ]
                            )
                    , Cmd.batch
                        [ model.personalia
                            |> Personalia.id
                            |> Api.putPersonalia PersonaliaOppdatert skjema
                        , lagtTilSpørsmålCmd model.debugStatus
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

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

        ViewportSatt result ->
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
                [ "Navn: " ++ (Personalia.fornavn personalia |> Maybe.withDefault "-") ++ " " ++ (Personalia.etternavn personalia |> Maybe.withDefault "-")
                , "Fødselsdato: " ++ (Personalia.fodselsdato personalia |> Maybe.withDefault "-")
                , "E-post: " ++ (Personalia.epost personalia |> Maybe.withDefault "-")
                , "Telefonnummer: " ++ (Personalia.telefon personalia |> Maybe.withDefault "-")
                , "Adresse: "
                    ++ (Personalia.gateadresse personalia |> Maybe.withDefault "-")
                    ++ " "
                    ++ (Personalia.poststed personalia |> Maybe.withDefault "-")
                    ++ " "
                    ++ (Personalia.postnummer personalia |> Maybe.withDefault "-")
                , Melding.tomLinje
                , "Er kontaktinformasjonen riktig?"
                ]
            ]

        EndreOriginal personaliaSkjema ->
            [ Melding.spørsmål
                [ "Ok! Vennligst skriv inn riktig informasjon i feltene under:" ]
            ]

        LagrerEndring personaliaSkjema ->
            [ Melding.spørsmål
                [ "Godt jobbet! Da tar jeg vare på den nye infoen!" ]
            ]

        LagringFeilet error personaliaSkjema ->
            [ Melding.spørsmål
                [ "Oops.. Noe gikk galt!" ]
            ]

        VenterPåAnimasjonFørFullføring _ ->
            []



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model { aktivSamtale, seksjonsMeldingsLogg }) =
    case MeldingsLogg.ferdigAnimert seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case aktivSamtale of
                BekreftOriginal personalia ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp OriginalPersonaliaBekreftet "Ja, informasjonen er riktig"
                                    |> Knapp.toHtml
                                , Knapp.knapp BrukerVilEndreOriginalPersonalia "Nei, jeg vil endre"
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                EndreOriginal personaliaSkjema ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ personaliaSkjema
                                |> Skjema.Personalia.fornavn
                                |> Input.input { label = "Fornavn", msg = PersonaliaSkjemaEndret Skjema.Personalia.Fornavn }
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.Personalia.etternavn
                                |> Input.input { label = "Etternavn", msg = PersonaliaSkjemaEndret Skjema.Personalia.Etternavn }
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.Personalia.fodselsdato
                                |> Input.input { label = "Fødselsdato", msg = PersonaliaSkjemaEndret Skjema.Personalia.Fodelsdato }
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.Personalia.epost
                                |> Input.input { label = "E-post", msg = PersonaliaSkjemaEndret Skjema.Personalia.Epost }
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.Personalia.telefon
                                |> Input.input { label = "Telefon", msg = PersonaliaSkjemaEndret Skjema.Personalia.Telefon }
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.Personalia.gateadresse
                                |> Input.input { label = "Gateadresse", msg = PersonaliaSkjemaEndret Skjema.Personalia.Gateadresse }
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.Personalia.postnummer
                                |> Input.input { label = "Postnummer", msg = PersonaliaSkjemaEndret Skjema.Personalia.Postnummer }
                                |> Input.toHtml
                            , personaliaSkjema
                                |> Skjema.Personalia.poststed
                                |> Input.input { label = "Poststed", msg = PersonaliaSkjemaEndret Skjema.Personalia.Poststed }
                                |> Input.toHtml
                            , Knapp.knapp PersonaliaskjemaLagreknappTrykket "Lagre endringer"
                                |> Knapp.toHtml
                            ]
                        ]

                LagrerEndring personaliaSkjema ->
                    text ""

                LagringFeilet error personaliaSkjema ->
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
