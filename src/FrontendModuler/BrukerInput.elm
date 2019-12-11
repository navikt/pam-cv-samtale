module FrontendModuler.BrukerInput exposing
    ( BrukerInput
    , KnapperLayout(..)
    , datoInputMedGåVidereKnapp
    , inputMedGåVidereKnapp
    , knapper
    , lenke
    , månedKnapper
    , selectMedGåVidereKnapp
    , skjema
    , textareaMedGåVidereKnapp
    , textareaSkjema
    , tilSvarMelding
    , toHtml
    , typeaheadMedGåVidereKnapp
    , utenInnhold
    )

import Dato exposing (Måned)
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
    | InputMedGåVidereKnapp msg (Input msg)
    | TextareaMedGåVidereKnapp msg (Textarea msg)
    | TextareaSkjema { lagreMsg : msg, lagreKnappTekst : String } (Textarea msg)
    | SelectMedGåVidereKnapp msg (Select msg)
    | TypeaheadMedGåVidereKnapp msg (Typeahead msg)
    | MånedKnapper (Måned -> msg)
    | DatoInputMedGåVidereKnapp msg (DatoInputMedDag msg)
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
inputMedGåVidereKnapp =
    InputMedGåVidereKnapp


textareaMedGåVidereKnapp : msg -> Textarea msg -> BrukerInput msg
textareaMedGåVidereKnapp =
    TextareaMedGåVidereKnapp


typeaheadMedGåVidereKnapp : msg -> Typeahead msg -> BrukerInput msg
typeaheadMedGåVidereKnapp =
    TypeaheadMedGåVidereKnapp


selectMedGåVidereKnapp : msg -> Select msg -> BrukerInput msg
selectMedGåVidereKnapp =
    SelectMedGåVidereKnapp


datoInputMedGåVidereKnapp : msg -> DatoInputMedDag msg -> BrukerInput msg
datoInputMedGåVidereKnapp =
    DatoInputMedGåVidereKnapp


textareaSkjema : { lagreMsg : msg, lagreKnappTekst : String } -> Textarea msg -> BrukerInput msg
textareaSkjema =
    TextareaSkjema


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

        InputMedGåVidereKnapp gåVidereMsg inputElement ->
            inputElement
                |> Input.toHtml
                |> gåVidereHtml gåVidereMsg

        SelectMedGåVidereKnapp gåVidereMsg selectElement ->
            selectElement
                |> Select.toHtml
                |> gåVidereHtml gåVidereMsg

        DatoInputMedGåVidereKnapp gåVidereMsg datoInputElement ->
            datoInputElement
                |> DatoInputMedDag.toHtml
                |> gåVidereHtml gåVidereMsg

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

        UtenInnhold ->
            text ""

        TypeaheadMedGåVidereKnapp gåVidereMsg typeahead ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema typeahead-skjema-height-wrapper" ]
                    [ typeahead
                        |> Typeahead.toHtml
                    , div [ class "gå-videre-knapp" ]
                        [ Knapp.knapp gåVidereMsg "Gå videre"
                            |> Knapp.toHtml
                        ]
                    ]
                ]

        TextareaMedGåVidereKnapp gåVidereMsg textareaElement ->
            textareaElement
                |> Textarea.toHtml
                |> gåVidereHtml gåVidereMsg

        TextareaSkjema { lagreMsg, lagreKnappTekst } textareaElement ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ textareaElement
                        |> Textarea.toHtml
                    , div [ class "gå-videre-knapp" ]
                        [ Knapp.knapp lagreMsg lagreKnappTekst
                            |> Knapp.toHtml
                        ]
                    ]
                ]

        MånedKnapper onMånedClick ->
            MånedKnapper.månedKnapper onMånedClick


gåVidereHtml : msg -> Html msg -> Html msg
gåVidereHtml gåVidereMsg inputElement =
    div [ class "skjema-wrapper" ]
        [ div [ class "skjema" ]
            [ inputElement
            , div [ class "gå-videre-knapp" ]
                [ Knapp.knapp gåVidereMsg "Gå videre"
                    |> Knapp.toHtml
                ]
            ]
        ]



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

        InputMedGåVidereKnapp _ inputElement ->
            if (Input.innhold >> String.trim >> String.isEmpty) inputElement then
                "Gå videre"

            else
                Input.innhold inputElement

        SelectMedGåVidereKnapp _ selectElement ->
            --- TODO: Fiks dette
            ""

        Skjema record list ->
            ""

        Lenke lenke_ ->
            Lenke.tekst_ lenke_

        UtenInnhold ->
            ""

        TypeaheadMedGåVidereKnapp _ typeaheadElement ->
            if (Typeahead.innhold >> String.trim >> String.isEmpty) typeaheadElement then
                "Gå videre"

            else
                Typeahead.innhold typeaheadElement

        TextareaMedGåVidereKnapp _ textareaElement ->
            if (Textarea.innhold >> String.trim >> String.isEmpty) textareaElement then
                "Gå videre"

            else
                Textarea.innhold textareaElement

        TextareaSkjema { lagreKnappTekst } textareaElement ->
            if (Textarea.innhold >> String.trim >> String.isEmpty) textareaElement then
                lagreKnappTekst

            else
                Textarea.innhold textareaElement

        MånedKnapper månedMsg ->
            Dato.måneder
                |> List.find (\måned -> månedMsg måned == msg)
                |> Maybe.map Dato.månedTilString
                |> Maybe.withDefault ""

        DatoInputMedGåVidereKnapp _ _ ->
            ""
