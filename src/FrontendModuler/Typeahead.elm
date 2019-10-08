module FrontendModuler.Typeahead exposing
    ( Operation(..)
    , Suggestion
    , Typeahead
    , TypeaheadOptions
    , toHtml
    , typeahead
    , withFeilmelding
    , withOnBlur
    , withOnFocus
    , withSuggestions
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List.Extra as List


type Typeahead msg
    = Typeahead (TypeaheadInfo msg)


type alias TypeaheadInfo msg =
    { label : String
    , onInput : String -> msg
    , onTypeaheadChange : Operation -> msg
    , innhold : String
    , suggestions : List (Suggestion msg)
    , inputId : String
    , feilmelding : Maybe String
    , onFocus : Maybe msg
    , onBlur : Maybe msg
    }


type Operation
    = ArrowUp
    | ArrowDown
    | Enter
    | MouseLeaveSuggestions


type alias TypeaheadOptions msg =
    { onInput : String -> msg
    , onTypeaheadChange : Operation -> msg
    , label : String
    , inputId : String
    }


typeahead : TypeaheadOptions msg -> String -> Typeahead msg
typeahead options innhold =
    Typeahead
        { label = options.label
        , onInput = options.onInput
        , onTypeaheadChange = options.onTypeaheadChange
        , innhold = innhold
        , suggestions = []
        , inputId = options.inputId
        , feilmelding = Nothing
        , onFocus = Nothing
        , onBlur = Nothing
        }


type alias Suggestion msg =
    { innhold : String
    , onActive : msg
    , onClick : msg
    , active : Bool
    }


withSuggestions : List (Suggestion msg) -> Typeahead msg -> Typeahead msg
withSuggestions suggestions (Typeahead options) =
    Typeahead { options | suggestions = suggestions }


withFeilmelding : Maybe String -> Typeahead msg -> Typeahead msg
withFeilmelding feilmelding (Typeahead options) =
    Typeahead { options | feilmelding = feilmelding }


withOnFocus : msg -> Typeahead msg -> Typeahead msg
withOnFocus onFocus (Typeahead options) =
    Typeahead { options | onFocus = Just onFocus }


withOnBlur : msg -> Typeahead msg -> Typeahead msg
withOnBlur onBlur (Typeahead options) =
    Typeahead { options | onBlur = Just onBlur }


onKeyUp : (Operation -> msg) -> Html.Attribute msg
onKeyUp onTypeaheadChange =
    Html.Events.preventDefaultOn
        "keydown"
        (Json.Decode.andThen (typeaheadKeys onTypeaheadChange) Html.Events.keyCode)


typeaheadKeys : (Operation -> msg) -> Int -> Json.Decode.Decoder ( msg, Bool )
typeaheadKeys onTypeaheadChange i =
    if i == 40 then
        Json.Decode.succeed ( onTypeaheadChange ArrowDown, True )

    else if i == 38 then
        Json.Decode.succeed ( onTypeaheadChange ArrowUp, True )

    else if i == 13 then
        Json.Decode.succeed ( onTypeaheadChange Enter, True )

    else
        Json.Decode.fail ""


toHtml : Typeahead msg -> Html msg
toHtml (Typeahead options) =
    div [ class "typeahead-wrapper" ]
        [ div [ class "typeahead" ]
            [ label
                [ class "skjemaelement__label", for options.inputId ]
                [ text options.label ]
            , input
                [ onInput options.onInput
                , value options.innhold
                , class "skjemaelement__input input--fullbredde"
                , classList [ ( "skjemaelement__input--harFeil", options.feilmelding /= Nothing ) ]
                , onKeyUp options.onTypeaheadChange
                , type_ "text"
                , id options.inputId
                , autocomplete False
                , role "combobox"
                , ariaAutocompleteList
                , ariaOwns (suggestionsId options.inputId)
                , ariaControls (suggestionsId options.inputId)
                , activeDescendant options.inputId options.suggestions
                , options.onFocus
                    |> Maybe.map onFocus
                    |> Maybe.withDefault noAttribute
                , options.onBlur
                    |> Maybe.map onBlur
                    |> Maybe.withDefault noAttribute
                ]
                []
            , if List.isEmpty options.suggestions then
                text ""

              else
                options.suggestions
                    |> List.map (viewSuggestion options.inputId)
                    |> ul
                        [ onMouseLeave (options.onTypeaheadChange MouseLeaveSuggestions)
                        , id (suggestionsId options.inputId)
                        , ariaExpanded "true"
                        , role "listbox"
                        , tabindex -1
                        , options.onFocus
                            |> Maybe.map onFocus
                            |> Maybe.withDefault noAttribute
                        ]
            ]
        , case options.feilmelding of
            Just feilmelding ->
                div [ role "alert", ariaLive "assertive" ]
                    [ div [ class "skjemaelement__feilmelding" ]
                        [ text feilmelding ]
                    ]

            Nothing ->
                text ""
        ]


activeDescendant : String -> List (Suggestion msg) -> Html.Attribute msg
activeDescendant inputFeltId suggestions =
    suggestions
        |> List.find .active
        |> Maybe.map (suggestionId inputFeltId)
        |> Maybe.map ariaActiveDescendant
        |> Maybe.withDefault noAttribute


viewSuggestion : String -> Suggestion msg -> Html msg
viewSuggestion inputFeltId suggestion =
    li
        [ onClick suggestion.onClick
        , onMouseEnter suggestion.onActive
        , role "option"
        , id (suggestionId inputFeltId suggestion)
        , if suggestion.active then
            ariaSelected "true"

          else
            ariaSelected "false"
        ]
        [ span [ classList [ ( "typetext", True ), ( "active", suggestion.active ) ] ]
            [ text suggestion.innhold ]
        ]


suggestionId : String -> Suggestion msg -> String
suggestionId inputId { innhold } =
    inputId ++ "-suggestion-" ++ String.replace " " "-" innhold


suggestionsId : String -> String
suggestionsId inputId =
    inputId ++ "-suggestions"


ariaOwns : String -> Html.Attribute msg
ariaOwns id =
    Html.Attributes.attribute "aria-owns" id


ariaAutocompleteList : Html.Attribute msg
ariaAutocompleteList =
    Html.Attributes.attribute "aria-autocomplete" "list"


noAttribute : Html.Attribute msg
noAttribute =
    classList []
