module Seksjon.Personalia exposing (Model, Msg, SamtaleStatus(..), init, meldingsLogg, update, viewBrukerInput)

import Api
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Melding exposing (Melding(..))
import MeldingsLogg exposing (MeldingsLogg)
import Personalia exposing (Personalia)
import Skjema.Personalia exposing (PersonaliaSkjema)



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg, aktivSamtale : Samtale, personalia : Personalia }


type Samtale
    = BekreftOriginal Personalia
    | EndreOriginal PersonaliaSkjema
    | LagrerEndring PersonaliaSkjema
    | LagringFeilet Http.Error PersonaliaSkjema


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Personalia MeldingsLogg


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


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        OriginalPersonaliaBekreftet ->
            Ferdig model.personalia model.seksjonsMeldingsLogg

        BrukerVilEndreOriginalPersonalia ->
            ( model.personalia
                |> Skjema.Personalia.init
                |> EndreOriginal
                |> nesteSamtaleSteg model "Endre"
            , Cmd.none
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
                        |> nesteSamtaleSteg model "Det jeg fylte ut"
                    , model.personalia
                        |> Personalia.id
                        |> Api.oppdaterPersonalia PersonaliaOppdatert skjema
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
                            Ferdig personalia model.seksjonsMeldingsLogg

                        Err error ->
                            ( LagringFeilet error skjema
                                |> nesteSamtaleSteg model "Ne gikk galt"
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig


nesteSamtaleSteg : ModelInfo -> String -> Samtale -> Model
nesteSamtaleSteg model tekst samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
                    |> MeldingsLogg.leggTilSvar (Melding.svar [ tekst ])
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
            [ Melding.spørsmål
                [ "Da har du bekreftet personaliaen din"
                ]
            ]

        EndreOriginal personaliaSkjema ->
            [ Melding.spørsmål
                [ "Du har valgt å endre personaliaskjema" ]
            ]

        LagrerEndring personaliaSkjema ->
            [ Melding.spørsmål
                [ "Melding lagret: " ++ Skjema.Personalia.fornavn personaliaSkjema ]
            ]

        LagringFeilet error personaliaSkjema ->
            [ Melding.spørsmål
                [ "Oops.. Noe gikk galt!" ]
            ]



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model { aktivSamtale }) =
    case aktivSamtale of
        BekreftOriginal personalia ->
            div []
                [ button [ onClick BrukerVilEndreOriginalPersonalia ] [ text "Endre" ]
                , button [ onClick OriginalPersonaliaBekreftet ] [ text "Bekreft" ]
                ]

        EndreOriginal personaliaSkjema ->
            div []
                [ label [] [ text "Fornavn" ]
                , input
                    [ Skjema.Personalia.fornavn personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Fornavn)
                    ]
                    []
                , label [] [ text "Etternavn" ]
                , input
                    [ Skjema.Personalia.etternavn personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Etternavn)
                    ]
                    []
                , label [] [ text "Fødselsdato" ]
                , input
                    [ Skjema.Personalia.fodselsdato personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Fodelsdato)
                    ]
                    []
                , label [] [ text "Epost" ]
                , input
                    [ Skjema.Personalia.epost personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Epost)
                    ]
                    []
                , label [] [ text "Telefon" ]
                , input
                    [ Skjema.Personalia.telefon personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Telefon)
                    ]
                    []
                , label [] [ text "Gateadresse" ]
                , input
                    [ Skjema.Personalia.gateadresse personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Gateadresse)
                    ]
                    []
                , label [] [ text "Postnummer" ]
                , input
                    [ Skjema.Personalia.postnummer personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Postnummer)
                    ]
                    []
                , label [] [ text "Poststed" ]
                , input
                    [ Skjema.Personalia.poststed personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Poststed)
                    ]
                    []
                , button [ onClick PersonaliaskjemaLagreknappTrykket ] [ text "Lagre" ]
                ]

        LagrerEndring personaliaSkjema ->
            text ""

        LagringFeilet error personaliaSkjema ->
            text ""



--- INIT ---


init : MeldingsLogg -> Personalia -> Model
init gammelMeldingsLogg personalia =
    Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål
                [ Melding.spørsmål
                    [ "Jeg har hentet inn kontaktinformasjonen din. Den vil vises på CV-en."
                    , "Det er viktig at informasjonen er riktig, slik at arbeidsgivere kan kontakte deg. "
                    , "Fornavn: "
                    , Personalia.fornavn personalia |> Maybe.withDefault "-"
                    , " Etternavn: "
                    , Personalia.etternavn personalia |> Maybe.withDefault "-"
                    ]
                ]
                gammelMeldingsLogg
        , aktivSamtale = BekreftOriginal personalia
        , personalia = personalia
        }
