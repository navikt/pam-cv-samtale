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
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.leggTilSvar (Melding.svar [ "Bekreft" ])
                |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Så bra! Da ruller vi videre!" ] ]
                |> Ferdig model.personalia

        BrukerVilEndreOriginalPersonalia ->
            ( model.personalia
                |> Skjema.Personalia.init
                |> EndreOriginal
                |> nesteSamtaleSteg model (Melding.svar [ "Endre" ])
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
                        |> nesteSamtaleSteg model
                            (Melding.svar
                                [ "Fornavn: " ++ Skjema.Personalia.fornavn skjema
                                , "Etternavn: " ++ Skjema.Personalia.etternavn skjema
                                , "Fødselsdato " ++ Skjema.Personalia.fodselsdato skjema
                                , "Epost: " ++ Skjema.Personalia.epost skjema
                                , "Telefonnummer: " ++ Skjema.Personalia.telefon skjema
                                , "Adresse: "
                                    ++ Skjema.Personalia.gateadresse skjema
                                    ++ " "
                                    ++ Skjema.Personalia.postnummer skjema
                                    ++ " "
                                    ++ Skjema.Personalia.poststed skjema
                                ]
                            )
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
                                |> nesteSamtaleSteg model (Melding.spørsmål [ "Noe gikk galt" ])
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig


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
            [ Melding.spørsmål [ "Da settter vi i gang :)" ]
            , Melding.spørsmål
                [ "Jeg har hentet inn kontaktinformasjonen din. Den vil vises på CV-en."
                , "Det er viktig at informasjonen er riktig, slik at arbeidsgivere kan kontakte deg. "
                ]
            , Melding.spørsmål
                [ "Fornavn: " ++ (Personalia.fornavn personalia |> Maybe.withDefault "-")
                , "Etternavn: " ++ (Personalia.etternavn personalia |> Maybe.withDefault "-")
                , "Fødselsdato: " ++ (Personalia.fodselsdato personalia |> Maybe.withDefault "-")
                , "Epost: " ++ (Personalia.epost personalia |> Maybe.withDefault "-")
                , "Telefonnummer: " ++ (Personalia.telefon personalia |> Maybe.withDefault "-")
                , "Adresse: "
                    ++ (Personalia.gateadresse personalia |> Maybe.withDefault "-")
                    ++ " "
                    ++ (Personalia.poststed personalia |> Maybe.withDefault "-")
                    ++ " "
                    ++ (Personalia.postnummer personalia |> Maybe.withDefault "-")
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
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
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
                    ]
                , button [ onClick PersonaliaskjemaLagreknappTrykket ] [ text "Lagre" ]
                ]

        LagrerEndring personaliaSkjema ->
            text ""

        LagringFeilet error personaliaSkjema ->
            text ""



--- INIT ---


init : Personalia -> MeldingsLogg -> Model
init personalia gammelMeldingsLogg =
    let
        aktivSamtale =
            BekreftOriginal personalia
    in
    Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål
                (samtaleTilMeldingsLogg aktivSamtale)
                gammelMeldingsLogg
        , aktivSamtale = aktivSamtale
        , personalia = personalia
        }
