module FrontendModuler.Lenke exposing (Lenke, lenke, tekst_, toHtml, withClass, withId, withOnFocus, withTargetBlank)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onFocus)


type Lenke msg
    = Lenke
        { tekst : String
        , url : String
        , åpneINyFane : Bool
        , id : Maybe String
        , class : Maybe String
        , onFocus : Maybe msg
        }


lenke : { tekst : String, url : String } -> Lenke msg
lenke { tekst, url } =
    Lenke
        { tekst = tekst
        , url = url
        , åpneINyFane = False
        , id = Nothing
        , class = Nothing
        , onFocus = Nothing
        }


withTargetBlank : Lenke msg -> Lenke msg
withTargetBlank (Lenke options) =
    Lenke { options | åpneINyFane = True }


withId : String -> Lenke msg -> Lenke msg
withId id (Lenke options) =
    Lenke { options | id = Just id }


withClass : String -> Lenke msg -> Lenke msg
withClass class (Lenke options) =
    Lenke { options | class = Just class }


withOnFocus : msg -> Lenke msg -> Lenke msg
withOnFocus onFocus (Lenke options) =
    Lenke { options | onFocus = Just onFocus }


toHtml : Lenke msg -> Html msg
toHtml (Lenke options) =
    if options.åpneINyFane then
        span [ class "ForlateSiden" ]
            [ a
                [ href options.url
                , class "lenke"
                , target "_blank"
                , rel "noopener noreferrer"
                , tabindex 0
                , options.id
                    |> Maybe.map id
                    |> Maybe.withDefault noAttribute
                , options.class
                    |> Maybe.map class
                    |> Maybe.withDefault noAttribute
                , options.onFocus
                    |> Maybe.map onFocus
                    |> Maybe.withDefault noAttribute
                ]
                [ span [] [ text options.tekst ]
                , i [ class "ForlateSiden__icon" ] []
                ]
            ]

    else
        a
            [ href options.url
            , class "lenke"
            , options.id
                |> Maybe.map id
                |> Maybe.withDefault noAttribute
            , options.class
                |> Maybe.map class
                |> Maybe.withDefault noAttribute
            , options.onFocus
                |> Maybe.map onFocus
                |> Maybe.withDefault noAttribute
            , tabindex 0
            ]
            [ text options.tekst ]


noAttribute : Html.Attribute msg
noAttribute =
    classList []


tekst_ : Lenke msg -> String
tekst_ (Lenke options) =
    options.tekst
