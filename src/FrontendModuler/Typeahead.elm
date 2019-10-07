module FrontendModuler.Typeahead exposing
    ( Operation(..)
    , Suggestion
    , Typeahead
    , TypeaheadOptions
    , toHtml
    , typeahead
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
    div [ class "typeahead" ]
        [ label
            [ class "skjemaelement__label", for options.inputId ]
            [ text options.label ]
        , input
            [ onInput options.onInput
            , value options.innhold
            , class "skjemaelement__input input--fullbredde"
            , onKeyUp options.onTypeaheadChange
            , type_ "text"
            , id options.inputId
            , autocomplete False
            , role "combobox"
            , ariaAutocompleteList
            , ariaOwns (suggestionsId options.inputId)
            , ariaControls (suggestionsId options.inputId)
            , activeDescendant options.inputId options.suggestions
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
                    ]
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
