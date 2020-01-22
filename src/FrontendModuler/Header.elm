module FrontendModuler.Header exposing (Header, HeaderInput, arbeidsplassenLogo, header, toHtml)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import Metrikker
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, viewBox)
import Time exposing (Posix, utc)


type Header msg
    = Header (HeaderInput msg)


type alias HeaderInput msg =
    { windowWidth : Int
    , onAvsluttClick : msg
    , aktivSeksjon : Metrikker.Seksjon
    , sistLagret : Maybe String
    }


header : HeaderInput msg -> Header msg
header input =
    Header input


buttonFullScreen : msg -> Html msg
buttonFullScreen onAvsluttClick =
    button
        [ class "Knapp Knapp--flat avslutt-knapp-med-ikon"
        , onClick onAvsluttClick
        ]
        [ text "Avslutt CV-registreringen"
        , i [ class "avslutt-ikon" ] []
        ]


sistLagretToHtml : String -> Html msg
sistLagretToHtml sistLagret =
    if String.contains ":" sistLagret then
        span [] [ text "Sist lagret: ", time [ datetime "00:00" ] [ text sistLagret ] ]

    else
        span [] [ text ("Sist lagret: " ++ sistLagret) ]


toHtml : Header msg -> Html msg
toHtml (Header options) =
    div [ class "header" ]
        [ div [ class "Header__logo" ]
            [ a [ ariaLabel "Logo Arbeidsplassen", href ("/cv-samtale/goto/forsiden?seksjon=" ++ Metrikker.seksjonTilString options.aktivSeksjon) ]
                [ arbeidsplassenLogo ]
            ]
        , if options.windowWidth > 460 then
            case options.sistLagret of
                Just time ->
                    div [ class "header__rightSection" ]
                        [ sistLagretToHtml time
                        , buttonFullScreen options.onAvsluttClick
                        ]

                Nothing ->
                    buttonFullScreen options.onAvsluttClick

          else
            button
                [ class "Knapp Knapp--flat avslutt-knapp-med-ikon"
                , onClick options.onAvsluttClick
                ]
                [ i [ class "avslutt-ikon" ] [], text "Avslutt" ]
        ]


arbeidsplassenLogo : Html msg
arbeidsplassenLogo =
    svg [ viewBox "0 0 190 40" ]
        [ path [ fill "#40c1ac", d "M15.9 0L0 40h174.1L190 0z" ] []
        , path
            [ fill "#062040"
            , d
                ("M24.4 15.2c1.7 0 2.8.8 3.5 1.9v-1.4c0-.1.1-.2.2-.2h1.8c.1 0 .2.1.2.2v7.9h.8c.2 0 "
                    ++ ".3.2.2.4l-.5 1.4c0 .1-.1.1-.2.1h-2.2c-.1 0-.2-.1-.2-.2v-1.4c-.7 1.2-1.9 1.9-3.5 "
                    ++ "1.9-2.6 0-4.8-2.4-4.8-5.3-.1-3 2.1-5.3 4.7-5.3zm.5 8.5c1.8 0 3-1.3 3-3.2 "
                    ++ "0-1.9-1.3-3.2-3-3.2-1.8 0-3 1.3-3 3.2 0 1.9 1.2 3.2 3 3.2zM33.5 "
                    ++ "25.2v-7.9h-.8c-.2 0-.3-.2-.2-.4l.5-1.4c0-.1.1-.1.2-.1h2.2c.1 0 .2.1.2.2v2c.8-1.4 "
                    ++ "2-2.4 3.8-2.5.1 0 .2.1.2.2v2c0 .1-.1.2-.2.2-2.6 0-3.8 1.4-3.8 3.2v4.4c0 "
                    ++ ".1-.1.2-.2.2h-1.8c0 .1-.1 0-.1-.1zM43.5 23.8v1.4c0 .1-.1.2-.2.2h-2.4c-.2 "
                    ++ "0-.3-.2-.2-.4l.7-1.7V11.7c0-.1.1-.2.2-.2h1.8c.1 0 .2.1.2.2v5.4c.7-1.2 1.9-1.9 "
                    ++ "3.5-1.9 2.6 0 4.8 2.3 4.8 5.3 0 2.9-2.2 5.3-4.8 "
                    ++ "5.3-1.7-.1-2.8-.8-3.6-2zm6.1-3.3c0-1.9-1.3-3.2-3-3.2-1.8 0-3 1.3-3 3.2 0 1.9 1.3 "
                    ++ "3.2 3 3.2 1.8 0 3-1.3 3-3.2zM58.6 15.2c2.5 0 4.8 1.9 4.8 5v.8c0 .1-.1.2-.2.2H56c.2 "
                    ++ "1.5 1.1 2.4 2.6 2.4 1 0 1.9-.1 2.3-.9 0-.1.1-.1.2-.1h1.8c.2 0 .3.1.2.3-.5 1.8-2.4 "
                    ++ "2.7-4.4 2.7-2.8 0-4.9-2.3-4.9-5.3 0-2.8 2-5.1 4.8-5.1zm2.6 "
                    ++ "4.2c0-1.4-1-2.2-2.5-2.2s-2.3.9-2.6 2.2h5.1zM65.1 12.2c0-.9.7-1.7 1.7-1.7.9 0 1.7.8 "
                    ++ "1.7 1.7 0 .9-.8 1.7-1.7 1.7-1 0-1.7-.7-1.7-1.7zm.6 13v-9.5c0-.1.1-.2.2-.2h1.8c.1 0 "
                    ++ ".2.1.2.2v9.5c0 .1-.1.2-.2.2h-1.8c-.1 0-.2-.1-.2-.2zM74.9 15.2c1.7 0 2.8.8 3.5 "
                    ++ "1.9v-5.4c0-.1.1-.2.2-.2h1.8c.1 0 .2.1.2.2v11.9h.8c.2 0 .3.2.2.4l-.5 1.4c0 "
                    ++ ".1-.1.1-.2.1h-2.2c-.1 0-.2-.1-.2-.2v-1.4c-.7 1.2-1.9 1.9-3.5 1.9-2.6 "
                    ++ "0-4.8-2.3-4.8-5.3s2.1-5.3 4.7-5.3zm.5 8.5c1.8 0 3-1.3 3-3.2 0-1.9-1.3-3.2-3-3.2-1.8 "
                    ++ "0-3 1.3-3 3.2 0 1.9 1.2 3.2 3 3.2zM83.3 22.9c0-.1.1-.2.2-.2h1.8c.1 0 .2.1.2.2.2.6 "
                    ++ "1.2 1 2.4 1 1.4 0 2.3-.4 2.3-1.2 0-2.3-6.8.1-6.8-4.2 0-2 1.6-3.2 4.3-3.2 2.4 0 4.3 "
                    ++ "1.1 4.4 2.9 0 .1-.1.2-.2.2H90c-.1 0-.2-.1-.2-.2-.2-.7-1.1-1.1-2.2-1.1-1.3 "
                    ++ "0-2.1.4-2.1 1.3 0 2.1 6.9-.2 6.9 4.2 0 1.9-1.7 3.1-4.5 3.1-2.5 "
                    ++ "0-4.5-1.1-4.6-2.8zM100.7 25.7c-1.7 0-2.8-.8-3.5-1.9v5.4c0 .1-.1.2-.2.2h-1.8c-.1 "
                    ++ "0-.2-.1-.2-.2V17.3h-.8c-.2 0-.3-.2-.2-.4l.5-1.4c0-.1.1-.1.2-.1H97c.1 0 "
                    ++ ".2.1.2.2V17c.7-1.2 1.9-1.9 3.5-1.9 2.6 0 4.8 2.3 4.8 5.3s-2.1 5.3-4.8 "
                    ++ "5.3zm-.5-8.5c-1.8 0-3 1.3-3 3.2 0 1.9 1.3 3.2 3 3.2 1.8 0 "
                    ++ "3-1.3 3-3.2.1-1.8-1.2-3.2-3-3.2zM107.8 25.2V11.7c0-.1.1-.2.2-.2h1.8c.1 0 "
                    ++ ".2.1.2.2v13.5c0 .1-.1.2-.2.2H108c-.1 0-.2-.1-.2-.2zM117 15.2c1.7 0 2.8.8 3.5 "
                    ++ "1.9v-1.4c0-.1.1-.2.2-.2h1.8c.1 0 .2.1.2.2v7.9h.8c.2 0 .3.2.2.4l-.5 1.4c0 "
                    ++ ".1-.1.1-.2.1h-2.2c-.1 0-.2-.1-.2-.2v-1.4c-.7 1.2-1.9 1.9-3.5 1.9-2.6 "
                    ++ "0-4.8-2.4-4.8-5.3-.1-3 2-5.3 4.7-5.3zm.5 8.5c1.8 0 3-1.3 3-3.2 "
                    ++ "0-1.9-1.3-3.2-3-3.2-1.8 0-3 1.3-3 3.2-.1 1.9 1.2 3.2 3 3.2zM125.3 "
                    ++ "22.9c0-.1.1-.2.2-.2h1.8c.1 0 .2.1.2.2.2.6 1.2 1 2.4 1 1.4 0 2.3-.4 "
                    ++ "2.3-1.2 0-2.3-6.8.1-6.8-4.2 0-2 1.6-3.2 4.3-3.2 2.4 0 4.3 1.1 4.4 2.9 0 "
                    ++ ".1-.1.2-.2.2H132c-.1 0-.2-.1-.2-.2-.2-.7-1.1-1.1-2.2-1.1-1.3 0-2.1.4-2.1 1.3 0 2.1 "
                    ++ "6.9-.2 6.9 4.2 0 1.9-1.7 3.1-4.5 3.1-2.4 0-4.5-1.1-4.6-2.8zM136.4 "
                    ++ "22.9c0-.1.1-.2.2-.2h1.8c.1 0 .2.1.2.2.2.6 1.2 1 2.4 1 1.4 0 2.3-.4 2.3-1.2 "
                    ++ "0-2.3-6.8.1-6.8-4.2 0-2 1.6-3.2 4.3-3.2 2.4 0 4.3 1.1 4.4 2.9 0 "
                    ++ ".1-.1.2-.2.2h-1.9c-.1 0-.2-.1-.2-.2-.2-.7-1.1-1.1-2.2-1.1-1.3 0-2.1.4-2.1 1.3 0 2.1 "
                    ++ "6.9-.2 6.9 4.2 0 1.9-1.7 3.1-4.5 3.1-2.4 0-4.4-1.1-4.6-2.8zM152.5 15.2c2.5 0 4.8 1.9 "
                    ++ "4.8 5v.8c0 .1-.1.2-.2.2H150c.2 1.5 1.1 2.4 2.6 2.4 1 0 "
                    ++ "1.9-.1 2.3-.9 0-.1.1-.1.2-.1h1.8c.2 0 .3.1.2.3-.5 1.8-2.4 2.7-4.4 2.7-2.8 "
                    ++ "0-4.9-2.3-4.9-5.3-.2-2.8 1.9-5.1 4.7-5.1zm2.5 4.2c0-1.4-1-2.2-2.5-2.2s-2.3.9-2.6 "
                    ++ "2.2h5.1zM159.1 15.6c0-.1.1-.1.2-.1h2.2c.1 0 .2.1.2.2v1.6c.6-1.3 1.7-2.1 3.4-2.1 "
                    ++ "2.6 0 3.6 1.9 3.6 4.9v5.2c0 .1-.1.2-.2.2h-1.8c-.1 "
                    ++ "0-.2-.1-.2-.2v-5.2c0-2.3-1-2.8-2.3-2.8-1.8 0-2.6 1.3-2.6 3.2v4.8c0 "
                    ++ ".1-.1.2-.2.2h-1.8c-.1 0-.2-.1-.2-.2v-7.9h-.8c-.2 0-.3-.2-.2-.4l.7-1.4z"
                )
            ]
            []
        ]
