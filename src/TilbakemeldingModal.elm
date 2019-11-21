module TilbakemeldingModal exposing (ModalStatus(..), Model, Msg, init, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import FrontendModuler.Lenke as Lenke
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (onClick, onFocus, preventDefaultOn)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline
import Task



--- MODEL ---


type Model
    = Model { sisteElementMedFokus : InputId }



--- UPDATE ---


type Msg
    = TrykketLukkeKnapp
    | EscapeTrykket
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

        EscapeTrykket ->
            Closed

        ElementFikkFokus inputId ->
            Open (Model { sisteElementMedFokus = inputId }) Cmd.none

        TabTrykket ->
            sisteElementMedFokus
                |> nesteInput
                |> inputIdTilString
                |> Browser.Dom.focus
                |> Task.attempt SattFocus
                |> Open model

        ShiftTabTrykket ->
            sisteElementMedFokus
                |> forrigeInput
                |> inputIdTilString
                |> Browser.Dom.focus
                |> Task.attempt SattFocus
                |> Open model

        SattFocus _ ->
            Open model Cmd.none



--- VIEW ---


view : Model -> Html Msg
view _ =
    div [ class "modal__overlay", Html.Attributes.attribute "aria-modal" "true" ]
        [ div [ class "modal__overlay gjennomsiktig", onClick TrykketLukkeKnapp ] []
        , div [ id (inputIdTilString SelveModalen), class "modal tilbakemelding-modal", tabindex -1, role "dialog", ariaLabel "Modal - Gi tilbakemelding", onTab ]
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
                    |> Lenke.withClass "gi-tilbakemelding-lenke"
                    |> Lenke.withOnFocus (ElementFikkFokus GiTilbakemeldingLenke)
                    |> Lenke.toHtml
                , Lenke.lenke { tekst = "Avslutt CV-registreringen", url = "/cv/forhandsvis" }
                    |> Lenke.withId (inputIdTilString AvsluttLenke)
                    |> Lenke.withOnFocus (ElementFikkFokus AvsluttLenke)
                    |> Lenke.toHtml
                ]
            ]
        ]


type InputId
    = SelveModalen
    | GiTilbakemeldingLenke
    | AvsluttLenke
    | LukkeKnapp


nesteInput : InputId -> InputId
nesteInput inputId =
    case inputId of
        SelveModalen ->
            GiTilbakemeldingLenke

        GiTilbakemeldingLenke ->
            AvsluttLenke

        AvsluttLenke ->
            LukkeKnapp

        LukkeKnapp ->
            GiTilbakemeldingLenke


forrigeInput : InputId -> InputId
forrigeInput inputId =
    case inputId of
        SelveModalen ->
            LukkeKnapp

        GiTilbakemeldingLenke ->
            LukkeKnapp

        AvsluttLenke ->
            GiTilbakemeldingLenke

        LukkeKnapp ->
            AvsluttLenke


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        SelveModalen ->
            "tilbakemeldings-modal-id"

        GiTilbakemeldingLenke ->
            "tilbakemelding-modal--gi-tilbakemelding-lenke"

        AvsluttLenke ->
            "tilbakemelding-modal--avslutt-lenke"

        LukkeKnapp ->
            "tilbakemelding-modal--lukke-knapp"


onTab : Html.Attribute Msg
onTab =
    decodeTab
        |> preventDefaultOn "keydown"


decodeTab : Decoder ( Msg, Bool )
decodeTab =
    decodeKeypressInfo
        |> Json.Decode.andThen keyPressInfoTilMsg


keyPressInfoTilMsg : KeyPressInfo -> Decoder ( Msg, Bool )
keyPressInfoTilMsg keyPressInfo =
    if keyPressInfo.key == "Tab" && not keyPressInfo.shiftPressed then
        Json.Decode.succeed ( TabTrykket, True )

    else if keyPressInfo.key == "Tab" && keyPressInfo.shiftPressed then
        Json.Decode.succeed ( ShiftTabTrykket, True )

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



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown decodeEscape


decodeEscape : Decoder Msg
decodeEscape =
    decodeKeypressInfo
        |> Json.Decode.andThen keyPressInfoTilEscapeMsg


keyPressInfoTilEscapeMsg : KeyPressInfo -> Decoder Msg
keyPressInfoTilEscapeMsg keyPressInfo =
    if keyPressInfo.key == "Escape" then
        Json.Decode.succeed EscapeTrykket

    else
        Json.Decode.fail ""



--- INIT ---


init : ( Model, Cmd Msg )
init =
    ( Model { sisteElementMedFokus = LukkeKnapp }
    , SelveModalen
        |> inputIdTilString
        |> Browser.Dom.focus
        |> Task.attempt SattFocus
    )
