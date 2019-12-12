module FrontendModuler.BrukerInput exposing
    ( BrukerInput
    , KnapperLayout(..)
    , brukerInputMedGåVidereKnapp
    , datoInputMedGåVidereKnapp
    , inputMedGåVidereKnapp
    , knapper
    , lenke
    , månedKnapper
    , selectMedGåVidereKnapp
    , skjema
    , textareaMedGåVidereKnapp
    , tilSvarMelding
    , toHtml
    , typeaheadMedGåVidereKnapp
    , utenInnhold
    )

import Dato exposing (Måned)
import FrontendModuler.BrukerInputMedGaVidereKnapp as BrukerInputMedGåVidereKnapp exposing (BrukerInputMedGåVidereKnapp)
import FrontendModuler.DatoInputMedDag as DatoInputMedDag exposing (DatoInputMedDag)
import FrontendModuler.Input as Input exposing (Input)
import FrontendModuler.Knapp as Knapp exposing (Knapp)
import FrontendModuler.Lenke as Lenke exposing (Lenke)
import FrontendModuler.ManedKnapper as MånedKnapper
import FrontendModuler.Select as Select exposing (Select)
import FrontendModuler.Textarea as Textarea exposing (Textarea)
import FrontendModuler.Typeahead as Typeahead exposing (Typeahead)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Meldinger.Melding as Melding exposing (Melding)


type BrukerInput msg
    = Knapper KnapperLayout (List (Knapp msg))
    | BrukerInputMedGåVidereKnapp (BrukerInputMedGåVidereKnapp msg)
    | MånedKnapper (Måned -> msg)
    | Skjema { lagreMsg : msg, lagreKnappTekst : String } (List (Html msg))
    | Lenke (Lenke msg)
    | UtenInnhold



--- KONSTRUKTØRER ---


type KnapperLayout
    = Flytende
    | Kolonne
    | VarighetGrid


knapper : KnapperLayout -> List (Knapp msg) -> BrukerInput msg
knapper =
    Knapper


månedKnapper : (Måned -> msg) -> BrukerInput msg
månedKnapper =
    MånedKnapper


inputMedGåVidereKnapp : msg -> Input msg -> BrukerInput msg
inputMedGåVidereKnapp gåVidereMsg inputElement =
    BrukerInputMedGåVidereKnapp.input gåVidereMsg inputElement
        |> brukerInputMedGåVidereKnapp


brukerInputMedGåVidereKnapp : BrukerInputMedGåVidereKnapp msg -> BrukerInput msg
brukerInputMedGåVidereKnapp =
    BrukerInputMedGåVidereKnapp


textareaMedGåVidereKnapp : msg -> Textarea msg -> BrukerInput msg
textareaMedGåVidereKnapp gåVidereMsg textareaElement =
    BrukerInputMedGåVidereKnapp.textarea gåVidereMsg textareaElement
        |> brukerInputMedGåVidereKnapp


typeaheadMedGåVidereKnapp : msg -> Typeahead msg -> BrukerInput msg
typeaheadMedGåVidereKnapp gåVidereMsg typeaheadElement =
    BrukerInputMedGåVidereKnapp.typeahead gåVidereMsg typeaheadElement
        |> brukerInputMedGåVidereKnapp


selectMedGåVidereKnapp : msg -> Select msg -> BrukerInput msg
selectMedGåVidereKnapp gåVidereMsg selectElement =
    BrukerInputMedGåVidereKnapp.select gåVidereMsg selectElement
        |> brukerInputMedGåVidereKnapp


datoInputMedGåVidereKnapp : msg -> DatoInputMedDag msg -> BrukerInput msg
datoInputMedGåVidereKnapp gåVidereMsg datoInputElement =
    BrukerInputMedGåVidereKnapp.datoInput gåVidereMsg datoInputElement
        |> brukerInputMedGåVidereKnapp


skjema : { lagreMsg : msg, lagreKnappTekst : String } -> List (Html msg) -> BrukerInput msg
skjema =
    Skjema


lenke : Lenke msg -> BrukerInput msg
lenke =
    Lenke


utenInnhold : BrukerInput msg
utenInnhold =
    UtenInnhold



--- TIL HTML ---


toHtml : BrukerInput msg -> Html msg
toHtml brukerInput =
    case brukerInput of
        Knapper layout knappeElementer ->
            case layout of
                Flytende ->
                    div [ class "knapperad" ]
                        [ div [ class "knapper--flytende" ]
                            (List.map Knapp.toHtml knappeElementer)
                        ]

                Kolonne ->
                    div [ class "knapperad" ]
                        [ div [ class "knapper--kolonne" ]
                            (List.map Knapp.toHtml knappeElementer)
                        ]

                VarighetGrid ->
                    div [ class "knapperad" ]
                        [ div [ class "knapper--varighet" ]
                            (List.map Knapp.toHtml knappeElementer)
                        ]

        Skjema { lagreMsg, lagreKnappTekst } skjemaelementer ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    (List.concat
                        [ skjemaelementer
                        , [ div []
                                [ Knapp.knapp lagreMsg lagreKnappTekst
                                    |> Knapp.toHtml
                                ]
                          ]
                        ]
                    )
                ]

        Lenke lenkeElement ->
            div [ class "knapperad" ]
                [ div [ class "knapper--flytende" ]
                    [ lenkeElement
                        |> Lenke.toHtml
                    ]
                ]

        MånedKnapper onMånedClick ->
            MånedKnapper.månedKnapper onMånedClick

        UtenInnhold ->
            text ""

        BrukerInputMedGåVidereKnapp brukerInputMedGåVidereKnapp_ ->
            BrukerInputMedGåVidereKnapp.toHtml brukerInputMedGåVidereKnapp_



--- TIL MELDING ---


tilSvarMelding : msg -> BrukerInput msg -> Melding
tilSvarMelding msg brukerInput =
    tilString msg brukerInput
        |> List.singleton
        |> Melding.svar


tilString : msg -> BrukerInput msg -> String
tilString msg brukerInput =
    case brukerInput of
        Knapper _ knappeElementer ->
            knappeElementer
                |> List.find (\knapp -> Knapp.msg knapp == msg)
                |> Maybe.map Knapp.innhold
                |> Maybe.withDefault ""

        BrukerInputMedGåVidereKnapp brukerInputMedGåVidereKnapp_ ->
            BrukerInputMedGåVidereKnapp.tilString brukerInputMedGåVidereKnapp_

        Skjema _ _ ->
            ""

        Lenke lenke_ ->
            Lenke.tekst_ lenke_

        UtenInnhold ->
            ""

        MånedKnapper månedMsg ->
            Dato.måneder
                |> List.find (\måned -> månedMsg måned == msg)
                |> Maybe.map Dato.månedTilString
                |> Maybe.withDefault ""
