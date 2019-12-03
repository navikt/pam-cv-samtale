module FrontendModuler.Lenkepanel exposing (Lenkepanel, lenkepanel, toHtml, withClass)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onFocus)


type Lenkepanel msg
    = Lenkepanel
        { tekst : String
        , url : String
        , class : Maybe String
        }


lenkepanel : { tekst : String, url : String } -> Lenkepanel msg
lenkepanel { tekst, url } =
    Lenkepanel
        { tekst = tekst
        , url = url
        , class = Nothing
        }


withClass : String -> Lenkepanel msg -> Lenkepanel msg
withClass class (Lenkepanel options) =
    Lenkepanel { options | class = Just class }


toHtml : Lenkepanel msg -> Html msg
toHtml (Lenkepanel options) =
    a
        [ href options.url
        , class "lenkepanel lenkepanel--border"
        , options.class
            |> Maybe.map class
            |> Maybe.withDefault noAttribute
        , tabindex 0
        ]
        [ span [ class "typo-undertittel lenkepanel__heading" ]
            [ text options.tekst ]
        , span [ class "lenkepanel__indikator" ] []
        ]


noAttribute : Html.Attribute msg
noAttribute =
    classList []
