module Seksjon.Personalia exposing (Model, Msg, SamtaleStatus(..), historikk, init, update, viewBrukerInput)

import Api
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Personalia exposing (Personalia)
import Skjema.Personalia exposing (PersonaliaSkjema)
import Snakkeboble exposing (Snakkeboble(..))



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { historikk : List Snakkeboble, aktivSamtale : Samtale, personalia : Personalia }


type Samtale
    = BekreftOriginal Personalia
    | EndreOriginal PersonaliaSkjema
    | LagrerEndring PersonaliaSkjema
    | LagringFeilet Http.Error PersonaliaSkjema


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Personalia (List Snakkeboble)


historikk : Model -> List Snakkeboble
historikk (Model model) =
    model.historikk ++ [ samtaleTilBoble model.aktivSamtale ]



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
            Ferdig model.personalia model.historikk

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
                            Ferdig personalia model.historikk

                        Err error ->
                            ( LagringFeilet error skjema
                                |> nesteSamtaleSteg model "Olø noe gikk galt ass brusjan"
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
            , historikk = model.historikk ++ [ Bruker tekst ]
        }


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }


samtaleTilBoble : Samtale -> Snakkeboble
samtaleTilBoble personaliaSeksjon =
    case personaliaSeksjon of
        BekreftOriginal personalia ->
            Robot
                ("Jeg har hentet inn kontaktinformasjonen din. Den vil vises på CV-en."
                    ++ "Det er viktig at informasjonen er riktig, slik at arbeidsgivere kan kontakte deg. "
                    ++ "Fornavn: "
                    ++ (Personalia.fornavn personalia |> Maybe.withDefault "-")
                    ++ " Etternavn: "
                    ++ (Personalia.etternavn personalia |> Maybe.withDefault "-")
                )

        EndreOriginal personaliaSkjema ->
            Robot "Endre personalia"

        LagrerEndring personaliaSkjema ->
            Robot "Spinner"

        LagringFeilet error personaliaSkjema ->
            Robot "Lagring feilet"



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


init : Personalia -> Model
init personalia =
    Model { historikk = [], aktivSamtale = BekreftOriginal personalia, personalia = personalia }
