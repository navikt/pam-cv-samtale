module FrontendModuler.Knapp exposing
    ( Enabled(..)
    , Knapp
    , Type(..)
    , knapp
    , toHtml
    , withEnabled
    , withType
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Knapp msg
    = Knapp (Options msg)


type alias Options msg =
    { msg : msg
    , innhold : String
    , enabled : Enabled
    , knappeType : Type
    }


type Enabled
    = Enabled
    | Disabled


type Type
    = Normal
    | Hoved


knapp : msg -> String -> Knapp msg
knapp msg innhold =
    Knapp
        { msg = msg
        , innhold = innhold
        , enabled = Enabled
        , knappeType = Normal
        }


withEnabled : Enabled -> Knapp msg -> Knapp msg
withEnabled enabled (Knapp options) =
    Knapp { options | enabled = enabled }


withType : Type -> Knapp msg -> Knapp msg
withType knappeType (Knapp options) =
    Knapp { options | knappeType = knappeType }


toHtml : Knapp msg -> Html msg
toHtml (Knapp options) =
    case options.enabled of
        Enabled ->
            button
                [ classList [ ( "Knapp", True ), ( "Knapp--hoved", options.knappeType == Hoved ) ]
                , onClick options.msg
                ]
                [ text options.innhold ]

        Disabled ->
            button
                [ classList [ ( "Knapp", True ), ( "Knapp--disabled", True ), ( "Knapp--hoved", options.knappeType == Hoved ) ]
                , disabled True
                ]
                [ text options.innhold ]
