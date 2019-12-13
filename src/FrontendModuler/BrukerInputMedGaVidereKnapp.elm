module FrontendModuler.BrukerInputMedGaVidereKnapp exposing
    ( BrukerInputMedGåVidereKnapp
    , datoInput
    , input
    , select
    , textarea
    , tilString
    , toHtml
    , typeahead
    , withAlternativKnappetekst
    , withVisEksempelKnapp
    )

import FrontendModuler.DatoInputMedDag as DatoInputMedDag exposing (DatoInputMedDag)
import FrontendModuler.Input as Input exposing (Input)
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Select as Select exposing (Select)
import FrontendModuler.Textarea as Textarea exposing (Textarea)
import FrontendModuler.Typeahead as Typeahead exposing (Typeahead)
import Html exposing (..)
import Html.Attributes exposing (class)


type BrukerInputMedGåVidereKnapp msg
    = BrukerInputMedGåVidereKnapp (Info msg)


type alias Info msg =
    { inputElement : InputElement msg
    , gåVidereMsg : msg
    , alternativKnappetekst : Maybe String
    , visEksempelMsg : Maybe msg
    }


type InputElement msg
    = InputElement (Input msg)
    | TextareaElement (Textarea msg)
    | TypeaheadElement (Typeahead msg)
    | SelectElement (Select msg)
    | DatoInputMedDagElement (DatoInputMedDag msg)


input : msg -> Input msg -> BrukerInputMedGåVidereKnapp msg
input gåVidereMsg inputElement =
    InputElement inputElement
        |> init gåVidereMsg


textarea : msg -> Textarea msg -> BrukerInputMedGåVidereKnapp msg
textarea gåVidereMsg textareaElement =
    TextareaElement textareaElement
        |> init gåVidereMsg


typeahead : msg -> Typeahead msg -> BrukerInputMedGåVidereKnapp msg
typeahead gåVidereMsg typeaheadElement =
    TypeaheadElement typeaheadElement
        |> init gåVidereMsg


select : msg -> Select msg -> BrukerInputMedGåVidereKnapp msg
select gåVidereMsg selectElement =
    SelectElement selectElement
        |> init gåVidereMsg


datoInput : msg -> DatoInputMedDag msg -> BrukerInputMedGåVidereKnapp msg
datoInput gåVidereMsg datoInputMedDagElement =
    DatoInputMedDagElement datoInputMedDagElement
        |> init gåVidereMsg


init : msg -> InputElement msg -> BrukerInputMedGåVidereKnapp msg
init gåVidereMsg inputElement =
    BrukerInputMedGåVidereKnapp
        { inputElement = inputElement
        , gåVidereMsg = gåVidereMsg
        , alternativKnappetekst = Nothing
        , visEksempelMsg = Nothing
        }



--- OPTIONS ---


withAlternativKnappetekst : String -> BrukerInputMedGåVidereKnapp msg -> BrukerInputMedGåVidereKnapp msg
withAlternativKnappetekst alternativKnappetekst (BrukerInputMedGåVidereKnapp info) =
    BrukerInputMedGåVidereKnapp { info | alternativKnappetekst = Just alternativKnappetekst }


withVisEksempelKnapp : Bool -> msg -> BrukerInputMedGåVidereKnapp msg -> BrukerInputMedGåVidereKnapp msg
withVisEksempelKnapp eksempelKnappSkalVises visEksempelMsg (BrukerInputMedGåVidereKnapp info) =
    if eksempelKnappSkalVises then
        BrukerInputMedGåVidereKnapp { info | visEksempelMsg = Just visEksempelMsg }

    else
        BrukerInputMedGåVidereKnapp info



--- TIL HTML ---


toHtml : BrukerInputMedGåVidereKnapp msg -> Html msg
toHtml (BrukerInputMedGåVidereKnapp info) =
    case info.inputElement of
        InputElement inputElement ->
            inputElement
                |> Input.toHtml
                |> gåVidereHtml info

        SelectElement selectElement ->
            gåVidereHtml info
                (div [ class "select-i-samtaleflyt-wrapper" ]
                    [ selectElement
                        |> Select.toHtml
                    ]
                )

        DatoInputMedDagElement datoInputElement ->
            datoInputElement
                |> DatoInputMedDag.toHtml
                |> gåVidereHtml info

        TextareaElement textareaElement ->
            textareaElement
                |> Textarea.toHtml
                |> gåVidereHtml info

        TypeaheadElement typeaheadElement ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema typeahead-skjema-height-wrapper" ]
                    [ Typeahead.toHtml typeaheadElement
                    , div [ class "gå-videre-knapp" ]
                        [ gåVidereKnapp info ]
                    ]
                ]


gåVidereHtml : Info msg -> Html msg -> Html msg
gåVidereHtml info inputElement =
    div [ class "skjema-wrapper" ]
        [ div [ class "skjema" ]
            [ inputElement
            , div [ class "gå-videre-knapp" ]
                [ eksempelKnapp info.visEksempelMsg
                , gåVidereKnapp info
                ]
            ]
        ]


eksempelKnapp : Maybe msg -> Html msg
eksempelKnapp maybeEksempelMsg =
    case maybeEksempelMsg of
        Just visEksempelMsg ->
            Knapp.knapp visEksempelMsg visEksempelKnappTekst
                |> Knapp.toHtml

        Nothing ->
            text ""


visEksempelKnappTekst : String
visEksempelKnappTekst =
    "Jeg vil se eksempel"


gåVidereKnapp : Info msg -> Html msg
gåVidereKnapp info =
    info.alternativKnappetekst
        |> Maybe.withDefault "Gå videre"
        |> Knapp.knapp info.gåVidereMsg
        |> Knapp.toHtml



--- TIL STRING ---


tilString : msg -> BrukerInputMedGåVidereKnapp msg -> String
tilString msg (BrukerInputMedGåVidereKnapp info) =
    if visEksempelKnappTrykket msg info.visEksempelMsg then
        visEksempelKnappTekst

    else if (inputElementInnhold >> String.trim >> String.isEmpty) info.inputElement then
        "Gå videre"

    else
        inputElementInnhold info.inputElement


visEksempelKnappTrykket : msg -> Maybe msg -> Bool
visEksempelKnappTrykket msgSendt visEksempelMsg =
    case visEksempelMsg of
        Just msg ->
            msg == msgSendt

        Nothing ->
            False


inputElementInnhold : InputElement msg -> String
inputElementInnhold inputElement_ =
    case inputElement_ of
        InputElement inputElement ->
            Input.innhold inputElement

        TextareaElement textareaElement ->
            Textarea.innhold textareaElement

        TypeaheadElement typeaheadElement ->
            Typeahead.innhold typeaheadElement

        SelectElement selectElement ->
            --- TODO: Fiks dette
            ""

        DatoInputMedDagElement datoInputMedDag ->
            --- TODO: Fiks dette
            ""