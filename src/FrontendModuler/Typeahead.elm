module FrontendModuler.Typeahead exposing
    ( Operation(..)
    , Suggestion
    , Typeahead
    , TypeaheadOptions
    , toHtml
    , typeahead
    , withInputId
    , withSuggestions
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode


type Typeahead msg
    = Typeahead (TypeaheadInfo msg)


type alias TypeaheadInfo msg =
    { label : String
    , onInput : String -> msg
    , onTypeaheadChange : Operation -> msg
    , innhold : String
    , suggestions : List (Suggestion msg)
    , inputId : Maybe String
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
    }


typeahead : TypeaheadOptions msg -> String -> Typeahead msg
typeahead options innhold =
    Typeahead
        { label = options.label
        , onInput = options.onInput
        , onTypeaheadChange = options.onTypeaheadChange
        , innhold = innhold
        , suggestions = []
        , inputId = Nothing
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


withInputId : String -> Typeahead msg -> Typeahead msg
withInputId id (Typeahead options) =
    Typeahead { options | inputId = Just id }


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
            --- TODO: htmlFor={inputId}
            [ class "skjemaelement__label" ]
            [ text options.label ]
        , input
            [ onInput options.onInput
            , value options.innhold
            , class "skjemaelement__input input--fullbredde"
            , onKeyUp options.onTypeaheadChange
            , type_ "text"
            , options.inputId
                |> Maybe.map id
                |> Maybe.withDefault noAttribute
            ]
            []
        , if List.isEmpty options.suggestions then
            text ""

          else
            options.suggestions
                |> List.map viewSuggestion
                |> ul [ onMouseLeave (options.onTypeaheadChange MouseLeaveSuggestions) ]
        ]


viewSuggestion : Suggestion msg -> Html msg
viewSuggestion suggestion =
    li
        [ onClick suggestion.onClick
        , onMouseEnter suggestion.onActive
        ]
        [ span [ classList [ ( "typetext", True ), ( "active", suggestion.active ) ] ]
            [ text suggestion.innhold ]
        ]


noAttribute : Html.Attribute msg
noAttribute =
    classList []
