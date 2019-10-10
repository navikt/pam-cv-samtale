module Typeahead.Typeahead exposing
    ( GetSuggestionStatus(..)
    , Model
    , Msg
    , ProceedStatus(..)
    , TypeaheadInitInfo
    , TypeaheadInitWithSelectedInfo
    , init
    , initWithSelected
    , inputValue
    , selected
    , update
    , updateSuggestions
    , view
    )

import FrontendModuler.Typeahead as Typeahead
import Html exposing (..)
import Typeahead.TypeaheadState as TypeaheadState exposing (TypeaheadState)



--- MODEL ---


type Model a
    = Model (ModelInfo a)


type alias ModelInfo a =
    { selected : Maybe a
    , typeaheadState : TypeaheadState a
    , id : String
    , label : String
    }


inputValue : Model a -> String
inputValue (Model model) =
    TypeaheadState.value model.typeaheadState


selected : Model a -> Maybe a
selected (Model model) =
    model.selected



--- UPDATE ---


type Msg a
    = BrukerOppdatererInput String
    | BrukerTrykkerTypeaheadTast Typeahead.Operation
    | BrukerHovrerOverTypeaheadSuggestion a
    | BrukerVelgerElement a
    | TypeaheadFikkFokus
    | TypeaheadMistetFokus


type GetSuggestionStatus
    = DoNothing
    | GetSuggestionsForInput String


type ProceedStatus a
    = UserWantsToProceed a
    | UserDoesNotWantToProceed


update : (a -> String) -> Msg a -> Model a -> ( Model a, GetSuggestionStatus, ProceedStatus a )
update toString msg (Model model) =
    case msg of
        BrukerOppdatererInput string ->
            ( model.typeaheadState
                |> TypeaheadState.updateValue string
                |> updateTypeaheadState toString model
            , GetSuggestionsForInput string
            , UserDoesNotWantToProceed
            )

        BrukerTrykkerTypeaheadTast operation ->
            case operation of
                Typeahead.ArrowUp ->
                    ( model.typeaheadState
                        |> TypeaheadState.arrowUp
                        |> updateTypeaheadState toString model
                    , DoNothing
                    , UserDoesNotWantToProceed
                    )

                Typeahead.ArrowDown ->
                    ( model.typeaheadState
                        |> TypeaheadState.arrowDown
                        |> updateTypeaheadState toString model
                    , DoNothing
                    , UserDoesNotWantToProceed
                    )

                Typeahead.Enter ->
                    case TypeaheadState.getActive model.typeaheadState of
                        Just active ->
                            updateAfterEnter toString active model

                        Nothing ->
                            case model.selected of
                                Just selected_ ->
                                    updateAfterEnter toString selected_ model

                                Nothing ->
                                    ( Model model, DoNothing, UserDoesNotWantToProceed )

                Typeahead.MouseLeaveSuggestions ->
                    ( model.typeaheadState
                        |> TypeaheadState.removeActive
                        |> updateTypeaheadState toString model
                    , DoNothing
                    , UserDoesNotWantToProceed
                    )

        BrukerHovrerOverTypeaheadSuggestion active ->
            ( model.typeaheadState
                |> TypeaheadState.updateActive active
                |> updateTypeaheadState toString model
            , DoNothing
            , UserDoesNotWantToProceed
            )

        BrukerVelgerElement selected_ ->
            updateAfterSelect toString selected_ model

        TypeaheadFikkFokus ->
            ( model.typeaheadState
                |> TypeaheadState.showSuggestions
                |> updateTypeaheadState toString model
            , DoNothing
            , UserDoesNotWantToProceed
            )

        TypeaheadMistetFokus ->
            ( model.typeaheadState
                |> TypeaheadState.hideSuggestions
                |> updateTypeaheadState toString model
            , DoNothing
            , UserDoesNotWantToProceed
            )


updateTypeaheadState : (a -> String) -> ModelInfo a -> TypeaheadState a -> Model a
updateTypeaheadState toString model typeaheadState =
    Model
        { model
            | typeaheadState = typeaheadState
            , selected =
                case model.selected of
                    Just selected_ ->
                        if toString selected_ == TypeaheadState.value model.typeaheadState then
                            Just selected_

                        else
                            TypeaheadState.findSuggestionMatchingInputValue toString typeaheadState

                    Nothing ->
                        TypeaheadState.findSuggestionMatchingInputValue toString typeaheadState
        }


updateAfterEnter : (a -> String) -> a -> ModelInfo a -> ( Model a, GetSuggestionStatus, ProceedStatus a )
updateAfterEnter toString selected_ model =
    let
        ( newModel, getSuggestionStatus, proceedStatus ) =
            updateAfterSelect toString selected_ model
    in
    if toString selected_ == TypeaheadState.value model.typeaheadState && not (TypeaheadState.suggestionsAreShown model.typeaheadState) then
        ( newModel, getSuggestionStatus, UserWantsToProceed selected_ )

    else
        ( newModel, getSuggestionStatus, proceedStatus )


updateAfterSelect : (a -> String) -> a -> ModelInfo a -> ( Model a, GetSuggestionStatus, ProceedStatus a )
updateAfterSelect toString selected_ model =
    ( Model
        { model
            | selected = Just selected_
            , typeaheadState =
                model.typeaheadState
                    |> TypeaheadState.updateValue (toString selected_)
                    |> TypeaheadState.hideSuggestions
        }
    , selected_
        |> toString
        |> GetSuggestionsForInput
    , UserDoesNotWantToProceed
    )


updateSuggestions : (a -> String) -> Model a -> List a -> Model a
updateSuggestions toString (Model model) suggestions =
    model.typeaheadState
        |> TypeaheadState.updateSuggestions "" suggestions
        |> updateTypeaheadState toString model



--- VIEW ---


view : (a -> String) -> Maybe String -> Model a -> Html (Msg a)
view toString feilmelding (Model model) =
    model.typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = model.label, onInput = BrukerOppdatererInput, onTypeaheadChange = BrukerTrykkerTypeaheadTast, inputId = model.id }
        |> Typeahead.withSuggestions (typeaheadStateSuggestionsTilViewSuggestionRegistrerYrke toString model.typeaheadState)
        |> Typeahead.withFeilmelding feilmelding
        |> Typeahead.withOnFocus TypeaheadFikkFokus
        |> Typeahead.withOnBlur TypeaheadMistetFokus
        |> Typeahead.toHtml


typeaheadStateSuggestionsTilViewSuggestionRegistrerYrke : (a -> String) -> TypeaheadState a -> List (Typeahead.Suggestion (Msg a))
typeaheadStateSuggestionsTilViewSuggestionRegistrerYrke toString typeaheadState =
    typeaheadState
        |> TypeaheadState.mapSuggestions
            (\activeState suggestion ->
                { innhold = toString suggestion
                , onClick = BrukerVelgerElement suggestion
                , onActive = BrukerHovrerOverTypeaheadSuggestion suggestion
                , active =
                    case activeState of
                        TypeaheadState.Active ->
                            True

                        TypeaheadState.NotActive ->
                            False
                }
            )



--- INIT ---


type alias TypeaheadInitInfo a =
    { value : String
    , label : String
    , id : String
    , toString : a -> String
    }


init : TypeaheadInitInfo a -> Model a
init input =
    Model
        { selected = Nothing
        , typeaheadState = TypeaheadState.init input.value
        , id = input.id
        , label = input.label
        }


type alias TypeaheadInitWithSelectedInfo a =
    { selected : a
    , label : String
    , id : String
    , toString : a -> String
    }


initWithSelected : TypeaheadInitWithSelectedInfo a -> Model a
initWithSelected input =
    Model
        { selected = Just input.selected
        , id = input.id
        , label = input.label
        , typeaheadState =
            input.selected
                |> input.toString
                |> TypeaheadState.init
                |> TypeaheadState.hideSuggestions
        }
