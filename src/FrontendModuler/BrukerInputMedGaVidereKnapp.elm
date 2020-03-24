module FrontendModuler.BrukerInputMedGaVidereKnapp exposing
    ( BrukerInputMedGåVidereKnapp
    , checkboxGruppe
    , datoInput
    , datoMånedÅr
    , input
    , radioGruppe
    , select
    , textarea
    , tilString
    , toHtml
    , typeahead
    , typeaheadMedMerkelapper
    , withAlternativKnappetekst
    , withAvbrytKnapp
    , withFeilmelding
    , withVisEksempelKnapp
    )

import Dato.Dato as Dato
import FrontendModuler.Checkbox as Checkbox exposing (Checkbox)
import FrontendModuler.DatoInput as DatoInput exposing (DatoInput)
import FrontendModuler.DatoInputEttFelt as DatoInputEttFelt exposing (DatoInputEttFelt)
import FrontendModuler.Input as Input exposing (Input)
import FrontendModuler.Knapp as Knapp exposing (Type(..))
import FrontendModuler.Merkelapp as Merkelapp exposing (Merkelapp)
import FrontendModuler.MerkelappGruppe as MerkelappGruppe exposing (MerkelappGruppe)
import FrontendModuler.Radio as Radio exposing (Radio)
import FrontendModuler.RadioGruppe as RadioGruppe exposing (RadioGruppe)
import FrontendModuler.Select as Select exposing (Select)
import FrontendModuler.Textarea as Textarea exposing (Textarea)
import FrontendModuler.Typeahead as Typeahead exposing (Typeahead)
import Html exposing (..)
import Html.Attributes exposing (class)


type BrukerInputMedGåVidereKnapp msg
    = BrukerInputMedGåVidereKnapp (Options msg)


type alias Options msg =
    { inputElement : InputElement msg
    , gåVidereMsg : msg
    , alternativKnappetekst : Maybe String
    , visEksempelMsg : Maybe msg
    , onAvbrytMsg : Maybe msg
    , feilmelding : Maybe String
    }


type InputElement msg
    = InputElement (Input msg)
    | TextareaElement (Textarea msg)
    | TypeaheadElement (Typeahead msg)
    | TypeaheadMedMerkelapper (Typeahead msg) (MerkelappGruppe msg)
    | SelectElement (Select msg)
    | DatoInputEttFeltElement (DatoInputEttFelt msg)
    | DatoMånedÅr (DatoInput msg)
    | CheckboxGruppe (List (Checkbox msg))
    | RadioGruppe (RadioGruppe msg)


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


typeaheadMedMerkelapper : msg -> Typeahead msg -> MerkelappGruppe msg -> BrukerInputMedGåVidereKnapp msg
typeaheadMedMerkelapper gåVidereMsg typeaheadElement merkelapper =
    TypeaheadMedMerkelapper typeaheadElement merkelapper
        |> init gåVidereMsg


select : msg -> Select msg -> BrukerInputMedGåVidereKnapp msg
select gåVidereMsg selectElement =
    SelectElement selectElement
        |> init gåVidereMsg


checkboxGruppe : msg -> List (Checkbox msg) -> BrukerInputMedGåVidereKnapp msg
checkboxGruppe gåVidereMsg checkboxer =
    CheckboxGruppe checkboxer
        |> init gåVidereMsg


radioGruppe : msg -> RadioGruppe msg -> BrukerInputMedGåVidereKnapp msg
radioGruppe gåVidereMsg radioGruppeElement =
    RadioGruppe radioGruppeElement
        |> init gåVidereMsg


datoInput : msg -> DatoInputEttFelt msg -> BrukerInputMedGåVidereKnapp msg
datoInput gåVidereMsg datoInputEttFeltElement =
    DatoInputEttFeltElement datoInputEttFeltElement
        |> init gåVidereMsg


datoMånedÅr : msg -> DatoInput msg -> BrukerInputMedGåVidereKnapp msg
datoMånedÅr gåVidereMsg datoInputElement =
    DatoMånedÅr datoInputElement
        |> init gåVidereMsg


init : msg -> InputElement msg -> BrukerInputMedGåVidereKnapp msg
init gåVidereMsg inputElement =
    BrukerInputMedGåVidereKnapp
        { inputElement = inputElement
        , gåVidereMsg = gåVidereMsg
        , alternativKnappetekst = Nothing
        , visEksempelMsg = Nothing
        , onAvbrytMsg = Nothing
        , feilmelding = Nothing
        }



--- OPTIONS ---


withAlternativKnappetekst : String -> BrukerInputMedGåVidereKnapp msg -> BrukerInputMedGåVidereKnapp msg
withAlternativKnappetekst alternativKnappetekst (BrukerInputMedGåVidereKnapp options) =
    BrukerInputMedGåVidereKnapp { options | alternativKnappetekst = Just alternativKnappetekst }


withVisEksempelKnapp : Bool -> msg -> BrukerInputMedGåVidereKnapp msg -> BrukerInputMedGåVidereKnapp msg
withVisEksempelKnapp eksempelKnappSkalVises visEksempelMsg (BrukerInputMedGåVidereKnapp options) =
    if eksempelKnappSkalVises then
        BrukerInputMedGåVidereKnapp { options | visEksempelMsg = Just visEksempelMsg }

    else
        BrukerInputMedGåVidereKnapp options


withAvbrytKnapp : msg -> BrukerInputMedGåVidereKnapp msg -> BrukerInputMedGåVidereKnapp msg
withAvbrytKnapp onAvbrytMsg (BrukerInputMedGåVidereKnapp options) =
    BrukerInputMedGåVidereKnapp { options | onAvbrytMsg = Just onAvbrytMsg }


withFeilmelding : Maybe String -> BrukerInputMedGåVidereKnapp msg -> BrukerInputMedGåVidereKnapp msg
withFeilmelding feilmelding (BrukerInputMedGåVidereKnapp options) =
    BrukerInputMedGåVidereKnapp { options | feilmelding = feilmelding }



--- TIL HTML ---


toHtml : BrukerInputMedGåVidereKnapp msg -> Html msg
toHtml (BrukerInputMedGåVidereKnapp options) =
    case options.inputElement of
        InputElement inputElement ->
            inputElement
                |> Input.toHtml
                |> gåVidereHtml options

        SelectElement selectElement ->
            gåVidereHtml options
                (div [ class "select-i-samtaleflyt-wrapper" ]
                    [ selectElement
                        |> Select.toHtml
                    ]
                )

        DatoInputEttFeltElement datoInputElement ->
            datoInputElement
                |> DatoInputEttFelt.toHtml
                |> gåVidereHtml options

        DatoMånedÅr datoInputElement ->
            (datoInputElement
                |> DatoInput.withWrapperClass "datoMånedÅr-samtale-wrapper"
                |> DatoInput.månedÅrToHtml
            )
                |> gåVidereHtml options

        TextareaElement textareaElement ->
            textareaElement
                |> Textarea.toHtml
                |> gåVidereHtml options

        TypeaheadElement typeaheadElement ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema typeahead-skjema-height-wrapper" ]
                    [ Typeahead.toHtml typeaheadElement
                    , knapper options
                    ]
                ]

        TypeaheadMedMerkelapper typeaheadElement merkelapper ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema typeahead-skjema-height-wrapper" ]
                    [ Typeahead.toHtml typeaheadElement
                    , MerkelappGruppe.toHtml merkelapper
                    , knapper options
                    ]
                ]

        CheckboxGruppe checkboxer ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema auto-width" ]
                    [ div [ class "radio-checkbox-gruppe-wrapper" ]
                        (List.map Checkbox.toHtml checkboxer)
                    , knapper options
                    ]
                ]

        RadioGruppe radiogruppe ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema auto-width" ]
                    [ RadioGruppe.toHtml radiogruppe
                    , knapper options
                    ]
                ]


gåVidereHtml : Options msg -> Html msg -> Html msg
gåVidereHtml options inputElement =
    div [ class "skjema-wrapper" ]
        [ div [ class "skjema" ]
            [ inputElement
            , knapper options
            ]
        ]


knapper : Options msg -> Html msg
knapper options =
    case ( options.visEksempelMsg, options.onAvbrytMsg ) of
        ( Just visEksempelMsg, Just avbrytMsg ) ->
            div []
                [ div [ class "gå-videre-knapp" ]
                    [ eksempelKnapp visEksempelMsg
                    , gåVidereKnapp options
                    ]
                , div [ class "avbryt-rad" ]
                    [ avbrytKnapp avbrytMsg
                    ]
                ]

        ( Just visEksempelMsg, Nothing ) ->
            div [ class "gå-videre-knapp" ]
                [ eksempelKnapp visEksempelMsg
                , gåVidereKnapp options
                ]

        ( Nothing, _ ) ->
            div [ class "gå-videre-knapp" ]
                [ options.onAvbrytMsg
                    |> Maybe.map avbrytKnapp
                    |> Maybe.withDefault (text "")
                , gåVidereKnapp options
                ]


avbrytKnapp : msg -> Html msg
avbrytKnapp avbrytMsg =
    Knapp.knapp avbrytMsg "Avbryt"
        |> Knapp.withType Flat
        |> Knapp.withMouseDown avbrytMsg
        |> Knapp.toHtml


eksempelKnapp : msg -> Html msg
eksempelKnapp visEksempelMsg =
    Knapp.knapp visEksempelMsg visEksempelKnappTekst
        |> Knapp.toHtml


visEksempelKnappTekst : String
visEksempelKnappTekst =
    "Jeg vil se eksempel"


gåVidereKnapp : Options msg -> Html msg
gåVidereKnapp options =
    options.alternativKnappetekst
        |> Maybe.withDefault "Gå videre"
        |> Knapp.knapp options.gåVidereMsg
        |> Knapp.toHtml



--- TIL STRING ---


tilString : msg -> BrukerInputMedGåVidereKnapp msg -> String
tilString msg (BrukerInputMedGåVidereKnapp info) =
    if maybeKnappTrykket msg info.visEksempelMsg then
        visEksempelKnappTekst

    else if maybeKnappTrykket msg info.onAvbrytMsg then
        "Avbryt"

    else if (inputElementInnhold >> String.trim >> String.isEmpty) info.inputElement then
        "Gå videre"

    else
        inputElementInnhold info.inputElement


maybeKnappTrykket : msg -> Maybe msg -> Bool
maybeKnappTrykket msgSendt visEksempelMsg =
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

        TypeaheadMedMerkelapper _ merkelapper ->
            Merkelapp.listeTilString (MerkelappGruppe.merkelappListe merkelapper)

        SelectElement selectElement ->
            --- TODO: Fiks dette
            ""

        DatoInputEttFeltElement datoInputEttFelt ->
            --- TODO: Fiks dette
            ""

        DatoMånedÅr datoInputElement ->
            DatoInput.tilString datoInputElement

        CheckboxGruppe checkboxer ->
            Checkbox.toStringOfChecked checkboxer

        RadioGruppe radiogruppeElement ->
            Radio.checkedRadioToString (RadioGruppe.radioKnapper radiogruppeElement)



--
