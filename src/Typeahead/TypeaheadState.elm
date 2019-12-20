module Typeahead.TypeaheadState exposing
    ( ActiveState(..)
    , TypeaheadState
    , active
    , arrowDown
    , arrowUp
    , error
    , findSuggestionMatchingInputValue
    , hideSuggestions
    , init
    , mapSuggestions
    , removeActive
    , showSuggestions
    , updateActive
    , updateSuggestions
    , updateValue
    , value
    )

import Http
import List.Extra as List


type TypeaheadState a
    = TypeaheadState
        { value : String
        , suggestions : SuggestionList a
        }


type SuggestionList a
    = HideSuggestions (List a)
    | NoneActive (List a)
    | SuggestionActive
        { before : List a
        , active_ : a
        , after : List a
        }
    | SuggestionError Bool Http.Error


init : String -> TypeaheadState a
init value_ =
    TypeaheadState
        { value = value_
        , suggestions = NoneActive []
        }


value : TypeaheadState a -> String
value (TypeaheadState info) =
    info.value


active : TypeaheadState a -> Maybe a
active (TypeaheadState info) =
    case info.suggestions of
        HideSuggestions _ ->
            Nothing

        NoneActive _ ->
            Nothing

        SuggestionActive { active_ } ->
            Just active_

        SuggestionError _ _ ->
            Nothing


updateValue : String -> TypeaheadState a -> TypeaheadState a
updateValue value_ (TypeaheadState info) =
    TypeaheadState { info | value = value_ }


updateSuggestions : String -> Result Http.Error (List a) -> TypeaheadState a -> TypeaheadState a
updateSuggestions value_ suggestionResult (TypeaheadState info) =
    TypeaheadState
        { info
            | suggestions =
                case suggestionResult of
                    Ok suggestions ->
                        case info.suggestions of
                            HideSuggestions _ ->
                                HideSuggestions suggestions

                            NoneActive _ ->
                                NoneActive suggestions

                            SuggestionActive record ->
                                -- TODO: Denne burde ta vare på aktivt element
                                NoneActive suggestions

                            SuggestionError _ _ ->
                                NoneActive suggestions

                    Err error_ ->
                        SuggestionError True error_
        }


updateActive : a -> TypeaheadState a -> TypeaheadState a
updateActive newActive (TypeaheadState info) =
    TypeaheadState { info | suggestions = updateActiveSuggestion newActive info.suggestions }


updateActiveSuggestion : a -> SuggestionList a -> SuggestionList a
updateActiveSuggestion newActive suggestionList =
    case suggestionList of
        HideSuggestions list ->
            makeActive newActive list

        NoneActive list ->
            makeActive newActive list

        SuggestionActive { before, active_, after } ->
            [ before, [ active_ ], after ]
                |> List.concat
                |> makeActive newActive

        SuggestionError _ _ ->
            suggestionList


makeActive : a -> List a -> SuggestionList a
makeActive newActive suggestions =
    let
        findActive : a -> { before : List a, maybeActive : Maybe a, after : List a } -> { before : List a, maybeActive : Maybe a, after : List a }
        findActive elem { before, maybeActive, after } =
            case maybeActive of
                Just a ->
                    { before = before
                    , maybeActive = Just a
                    , after = after ++ [ elem ]
                    }

                Nothing ->
                    if elem == newActive then
                        { before = before
                        , maybeActive = Just elem
                        , after = after
                        }

                    else
                        { before = before ++ [ elem ]
                        , maybeActive = Nothing
                        , after = after
                        }

        maybeMarkertState =
            List.foldl findActive { before = [], maybeActive = Nothing, after = [] } suggestions
    in
    case maybeMarkertState.maybeActive of
        Just elem ->
            SuggestionActive
                { before = maybeMarkertState.before
                , active_ = elem
                , after = maybeMarkertState.after
                }

        Nothing ->
            NoneActive maybeMarkertState.before


arrowDown : TypeaheadState a -> TypeaheadState a
arrowDown (TypeaheadState info) =
    TypeaheadState
        { info
            | suggestions =
                case info.suggestions of
                    HideSuggestions list ->
                        arrowDownOnNoneActive list

                    NoneActive list ->
                        arrowDownOnNoneActive list

                    SuggestionActive { before, active_, after } ->
                        case after of
                            first :: rest ->
                                SuggestionActive
                                    { before = before ++ [ active_ ]
                                    , active_ = first
                                    , after = rest
                                    }

                            [] ->
                                [ before, [ active_ ], after ]
                                    |> List.concat
                                    |> NoneActive

                    SuggestionError _ _ ->
                        info.suggestions
        }


arrowDownOnNoneActive : List a -> SuggestionList a
arrowDownOnNoneActive list =
    case list of
        first :: rest ->
            SuggestionActive
                { before = []
                , active_ = first
                , after = rest
                }

        [] ->
            NoneActive []


arrowUp : TypeaheadState a -> TypeaheadState a
arrowUp (TypeaheadState info) =
    TypeaheadState
        { info
            | suggestions =
                case info.suggestions of
                    HideSuggestions list ->
                        arrowUpOnNoneActive list

                    NoneActive list ->
                        arrowUpOnNoneActive list

                    SuggestionActive { before, active_, after } ->
                        case List.reverse before of
                            last :: rest ->
                                SuggestionActive
                                    { before = List.reverse rest
                                    , active_ = last
                                    , after = active_ :: after
                                    }

                            [] ->
                                [ before, [ active_ ], after ]
                                    |> List.concat
                                    |> NoneActive

                    SuggestionError _ _ ->
                        info.suggestions
        }


arrowUpOnNoneActive : List a -> SuggestionList a
arrowUpOnNoneActive list =
    case List.reverse list of
        last :: rest ->
            SuggestionActive
                { before = List.reverse rest
                , active_ = last
                , after = []
                }

        [] ->
            NoneActive []


removeActive : TypeaheadState a -> TypeaheadState a
removeActive (TypeaheadState info) =
    TypeaheadState { info | suggestions = removeActiveFromSuggestions info.suggestions }


removeActiveFromSuggestions : SuggestionList a -> SuggestionList a
removeActiveFromSuggestions suggestionList =
    case suggestionList of
        HideSuggestions suggestions ->
            HideSuggestions suggestions

        NoneActive suggestions ->
            NoneActive suggestions

        SuggestionActive { before, active_, after } ->
            [ before, [ active_ ], after ]
                |> List.concat
                |> NoneActive

        SuggestionError _ _ ->
            suggestionList


showSuggestions : TypeaheadState a -> TypeaheadState a
showSuggestions (TypeaheadState info) =
    TypeaheadState
        { info
            | suggestions =
                case info.suggestions of
                    HideSuggestions list ->
                        NoneActive list

                    NoneActive list ->
                        NoneActive list

                    SuggestionActive record ->
                        SuggestionActive record

                    SuggestionError _ error_ ->
                        SuggestionError True error_
        }


hideSuggestions : TypeaheadState a -> TypeaheadState a
hideSuggestions (TypeaheadState info) =
    TypeaheadState
        { info
            | suggestions =
                case info.suggestions of
                    HideSuggestions list ->
                        HideSuggestions list

                    NoneActive list ->
                        HideSuggestions list

                    SuggestionActive { before, active_, after } ->
                        [ before, [ active_ ], after ]
                            |> List.concat
                            |> HideSuggestions

                    SuggestionError _ error_ ->
                        SuggestionError False error_
        }


findSuggestionMatchingInputValue : (a -> String) -> TypeaheadState a -> Maybe a
findSuggestionMatchingInputValue toString (TypeaheadState info) =
    let
        predicate suggestion =
            (toString >> String.trim >> String.toLower) suggestion == (String.trim >> String.toLower) info.value
    in
    case info.suggestions of
        HideSuggestions suggestions ->
            List.find predicate suggestions

        NoneActive suggestions ->
            List.find predicate suggestions

        SuggestionActive { before, active_, after } ->
            [ before
            , [ active_ ]
            , after
            ]
                |> List.concat
                |> List.find predicate

        SuggestionError _ _ ->
            Nothing


type ActiveState
    = Active
    | NotActive


mapSuggestions : (ActiveState -> a -> b) -> TypeaheadState a -> List b
mapSuggestions function (TypeaheadState info) =
    case info.suggestions of
        HideSuggestions _ ->
            []

        NoneActive suggestions ->
            List.map (function NotActive) suggestions

        SuggestionActive { before, active_, after } ->
            List.concat
                [ List.map (function NotActive) before
                , [ function Active active_ ]
                , List.map (function NotActive) after
                ]

        SuggestionError _ _ ->
            []


error : TypeaheadState a -> Maybe Http.Error
error (TypeaheadState info) =
    case info.suggestions of
        SuggestionError showError error_ ->
            if showError then
                Just error_

            else
                Nothing

        HideSuggestions list ->
            Nothing

        NoneActive list ->
            Nothing

        SuggestionActive record ->
            Nothing
