module FrontendModuler.Checkbox exposing
    ( Checkbox
    , checkbox
    , toHtml
    , toStringOfChecked
    , withClass
    , withId
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Checkbox msg
    = Checkbox
        { label : String
        , msg : msg
        , checked : Bool
        , class : Maybe String
        , id : Maybe String
        }


checkbox : String -> msg -> Bool -> Checkbox msg
checkbox label msg checkboxChecked =
    Checkbox
        { label = label
        , msg = msg
        , checked = checkboxChecked
        , class = Nothing
        , id = Nothing
        }


withClass : String -> Checkbox msg -> Checkbox msg
withClass class (Checkbox options) =
    Checkbox { options | class = Just class }


withId : String -> Checkbox msg -> Checkbox msg
withId id (Checkbox options) =
    Checkbox { options | id = Just id }


toHtml : Checkbox msg -> Html msg
toHtml (Checkbox options) =
    div
        [ class "skjemaelement skjemaelement--horisontal"
        , options.class
            |> Maybe.map class
            |> Maybe.withDefault noAttribute
        ]
        [ input
            [ type_ "checkbox"
            , class "skjemaelement__input checkboks"
            , checked options.checked
            , onClick options.msg
            , options.id
                |> Maybe.map id
                |> Maybe.withDefault noAttribute
            ]
            []

        --- TODO: htmlFor
        , label [ class "skjemaelement__label", class "skjemaelement--checkbox-label", onClick options.msg ] [ text options.label ]
        ]


noAttribute : Html.Attribute msg
noAttribute =
    classList []


innhold : Checkbox msg -> String
innhold (Checkbox options) =
    options.label


isChecked : Checkbox msg -> Bool
isChecked (Checkbox options) =
    options.checked


toStringOfChecked : List (Checkbox msg) -> String
toStringOfChecked checkboxListe =
    let
        listOfChecked =
            List.filter isChecked checkboxListe
    in
    case listOfChecked of
        [] ->
            ""

        x :: [] ->
            innhold x

        x :: y :: [] ->
            innhold x ++ " og " ++ innhold y

        x :: y :: z :: more ->
            toStringHelper (innhold x ++ ", " ++ innhold y) (z :: more)


toStringHelper : String -> List (Checkbox msg) -> String
toStringHelper foreløpigString merkelappListe =
    case merkelappListe of
        [] ->
            foreløpigString

        x :: [] ->
            foreløpigString ++ " og " ++ innhold x

        x :: xs ->
            toStringHelper (foreløpigString ++ ", " ++ innhold x) xs
