module FrontendModuler.Merkelapp exposing (Merkelapp, listeTilString, merkelapp, toHtml)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)


type Merkelapp msg
    = Merkelapp (Options msg)


type alias Options msg =
    { msg : msg
    , tekst : String
    }


merkelapp : msg -> String -> Merkelapp msg
merkelapp msg_ tekst_ =
    Merkelapp
        { msg = msg_
        , tekst = tekst_
        }


toHtml : List (Merkelapp msg) -> Html msg
toHtml merkelappListe =
    div [ class "Merkelapp__wrapper" ]
        (List.map merkelappToHtml merkelappListe)


listeTilString : List (Merkelapp msg) -> String
listeTilString merkelappListe =
    case merkelappListe of
        [] ->
            ""

        x :: [] ->
            innhold x

        x :: y :: [] ->
            innhold x ++ " og " ++ innhold y

        x :: y :: z :: more ->
            toStringHelper (innhold x ++ ", " ++ innhold y) (z :: more)


toStringHelper : String -> List (Merkelapp msg) -> String
toStringHelper foreløpigString merkelappListe =
    case merkelappListe of
        [] ->
            foreløpigString

        x :: [] ->
            foreløpigString ++ " og " ++ innhold x

        x :: xs ->
            toStringHelper (foreløpigString ++ ", " ++ innhold x) xs


innhold : Merkelapp msg -> String
innhold (Merkelapp options) =
    options.tekst


merkelappToHtml : Merkelapp msg -> Html msg
merkelappToHtml (Merkelapp options) =
    button
        [ class "Merkelapp typo-element"
        , onClick options.msg
        , ariaLabel "slett"
        ]
        [ div [ class "Merkelapp__text" ] [ text options.tekst ]
        , i [ class "Merkelapp__slett__icon" ] []
        ]
