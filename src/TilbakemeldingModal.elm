module TilbakemeldingModal exposing (ModalStatus(..), Model, Msg, init, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import FrontendModuler.Lenke as Lenke
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (onClick, onFocus)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline
import Task



--- MODEL ---


type Model
    = Model { sisteElementMedFokus : InputId }



--- UPDATE ---


type Msg
    = TrykketLukkeKnapp
    | ElementFikkFokus InputId
    | TabTrykket
    | ShiftTabTrykket
    | SattFocus (Result Browser.Dom.Error ())


type ModalStatus
    = Open Model (Cmd Msg)
    | Closed


update : Msg -> Model -> ModalStatus
update msg ((Model { sisteElementMedFokus }) as model) =
    case msg of
        TrykketLukkeKnapp ->
            Closed

        ElementFikkFokus inputId ->
            Open (Model { sisteElementMedFokus = inputId }) Cmd.none

        TabTrykket ->
            if sisteElementMedFokus == AvsluttLenke then
                LukkeKnapp
                    |> inputIdTilString
                    |> Browser.Dom.focus
                    |> Task.attempt SattFocus
                    |> Open model

            else
                Open model Cmd.none

        ShiftTabTrykket ->
            if sisteElementMedFokus == LukkeKnapp then
                AvsluttLenke
                    |> inputIdTilString
                    |> Browser.Dom.focus
                    |> Task.attempt SattFocus
                    |> Open model

            else
                Open model Cmd.none

        SattFocus _ ->
            Open model Cmd.none



--- VIEW ---


view : Model -> Html Msg
view _ =
    div [ class "modal__overlay", Html.Attributes.attribute "aria-modal" "true" ]
        [ div [ class "modal__overlay gjennomsiktig", onClick TrykketLukkeKnapp ] []
        , div [ id modalId, class "modal tilbakemelding-modal", tabindex -1, role "dialog", ariaLabel "Modal - Gi tilbakemelding" ]
            [ button
                [ class "lukknapp lukknapp--overstHjorne modal__lukknapp--shake"
                , onClick TrykketLukkeKnapp
                , onFocus (ElementFikkFokus LukkeKnapp)
                , id (inputIdTilString LukkeKnapp)
                ]
                [ text "Lukk modal" ]
            , section []
                [ h2 [] [ text "Hjelp oss med å gjøre roboten bedre" ]
                , p [] [ text "Vil du gi tilbakemelding?" ]
                , Lenke.lenke { tekst = "Gi tilbakemelding", url = "https://surveys.hotjar.com/s?siteId=118350&surveyId=144585" }
                    |> Lenke.withTargetBlank
                    |> Lenke.withId (inputIdTilString GiTilbakemeldingLenke)
                    |> Lenke.withOnFocus (ElementFikkFokus GiTilbakemeldingLenke)
                    |> Lenke.toHtml
                , Lenke.lenke { tekst = "Avslutt CV-registreringen", url = "/cv" }
                    |> Lenke.withId (inputIdTilString AvsluttLenke)
                    |> Lenke.withOnFocus (ElementFikkFokus AvsluttLenke)
                    |> Lenke.toHtml
                ]
            , div [ tabindex 0 ]
                [-- Denne er her sånn at nettleseren (spesifikt Firefox) skal ha noe å fokusere på etter Avslutt-lenken,
                 -- for default-oppførselen er at den fokuserer på selve fanen over nettsiden, og da kan vi ikke sette fokus lenger
                ]
            ]
        ]


type InputId
    = LukkeKnapp
    | GiTilbakemeldingLenke
    | AvsluttLenke


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        LukkeKnapp ->
            "tilbakemelding-modal--lukke-knapp"

        GiTilbakemeldingLenke ->
            "tilbakemelding-modal--gi-tilbakemelding-lenke"

        AvsluttLenke ->
            "tilbakemelding-modal--avslutt-lenke"


modalId : String
modalId =
    "tilbakemeldings-modal-id"



--- SUBSCRIPTIONS ---


decodeTab : Decoder Msg
decodeTab =
    decodeKeypressInfo
        |> Json.Decode.andThen keyPressInfoTilMsg


keyPressInfoTilMsg : KeyPressInfo -> Decoder Msg
keyPressInfoTilMsg keyPressInfo =
    if keyPressInfo.key == "Tab" && not keyPressInfo.shiftPressed then
        Json.Decode.succeed TabTrykket

    else if keyPressInfo.key == "Tab" && keyPressInfo.shiftPressed then
        Json.Decode.succeed ShiftTabTrykket

    else
        Json.Decode.fail ""


decodeKeypressInfo : Decoder KeyPressInfo
decodeKeypressInfo =
    Json.Decode.succeed KeyPressInfo
        |> Json.Decode.Pipeline.required "shiftKey" Json.Decode.bool
        |> Json.Decode.Pipeline.required "key" Json.Decode.string


type alias KeyPressInfo =
    { shiftPressed : Bool
    , key : String
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown decodeTab



--- INIT ---


init : ( Model, Cmd Msg )
init =
    ( Model { sisteElementMedFokus = LukkeKnapp }
    , modalId
        |> Browser.Dom.focus
        |> Task.attempt SattFocus
    )
