module FrontendModuler.Header exposing (Header, header, toHtml)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
import Svg exposing (circle, defs, desc, g, path, rect, svg, use)
import Svg.Attributes exposing (cx, cy, d, fill, fillRule, mask, r, rx, stroke, strokeLinecap, strokeWidth, transform, version, viewBox, x, y)


type Header
    = Header
        { windowWidth : Int
        }


header : Int -> Header
header width =
    Header
        { windowWidth = width
        }


toHtml : Header -> Html msg
toHtml (Header options) =
    div [ class "header" ]
        [ div [ class "Header__logo" ]
            [ a [ href "/", ariaLabel "Logo Arbeidsplassen" ]
                [ arbeidsplassenLogo ]
            ]

        -- TODO: Endre til å vise burger-meny på mobil og evt. iPad
        , if options.windowWidth > 410 then
            a [ href "/cv" ]
                [ div [ class "header-hoyre" ]
                    [ text "Avslutt CV-registreringen"
                    ]
                ]

          else
            text ""
        ]


robotLogo : Html msg
robotLogo =
    svg [ Svg.Attributes.width "50px", Svg.Attributes.height "50px", viewBox "0 0 50 50" ]
        [ desc [] [ text "Created with Sketch." ]
        , defs []
            [ circle [ id "path-1", cx "25", cy "25", r "25" ] [] ]
        , g [ id "CV", stroke "none", strokeWidth "1", fill "none", fillRule "evenodd" ]
            [ g [ id "CV-med-knapper-og-input-felt-språk", transform "translate(-301.000000, -173.000000)" ]
                [ g [ id "Snakkeboble/-4.-Kontaktinfo-feedback", transform "translate(301.000000, 173.000000)" ]
                    [ g [ id "robot" ]
                        [ g [ id "veileder/spotlight" ]
                            [ Svg.mask [ id "mask-2", fill "white" ]
                                [ use [] [] ]
                            , use [ id "Spotlight", fill "#40C1AC" ] []
                            , rect [ id "Rectangle", stroke "#062140", strokeWidth "2", fill "#E6F4EF", mask "url(#mask-2)", x "10.375", y "37.71875", Svg.Attributes.width "28.46875", Svg.Attributes.height "15.1875", rx "7.59375" ] []
                            , g [ id "Group-2", mask "url(#mask-2)" ]
                                [ g [ transform "translate(7.812500, 6.250000)" ]
                                    [ rect [ id "Rectangle", stroke "#062140", strokeWidth "2", fill "#E6F4EF", fillRule "evenodd", x "4.125", y "8.8125", Svg.Attributes.width "25.34375", Svg.Attributes.height "19.09375", rx "7" ] []
                                    , rect [ id "Rectangle", stroke "#062140", strokeWidth "2", fill "#E6F4EF", fillRule "evenodd", x "1", y "15.84375", Svg.Attributes.width "2.6875", Svg.Attributes.height "5.03125", rx "1.34375" ] []
                                    , rect [ id "Rectangle-Copy", stroke "#062140", strokeWidth "2", fill "#E6F4EF", fillRule "evenodd", x "29.90625", y "15.84375", Svg.Attributes.width "2.6875", Svg.Attributes.height "5.03125", rx "1.34375" ] []
                                    , circle [ id "Oval", stroke "none", fill "#062140", fillRule "evenodd", cx "11.71875", cy "16.40625", r "1.5625" ] []
                                    , circle [ id "Oval-Copy", stroke "none", fill "#062140", fillRule "evenodd", cx "21.875", cy "16.40625", r "1.5625" ] []
                                    , Svg.path [ d "M14.0625,21.1833106 C14.8704105,22.4851255 15.7818688,23.136033 16.796875,23.136033 C17.8118812,23.136033 18.7233395,22.4851255 19.53125,21.1833106", id "Path-3", stroke "#062140", strokeWidth "2", fill "none", strokeLinecap "round" ] []
                                    , circle [ id "Oval", stroke "none", fill "#062140", fillRule "evenodd", cx "16.40625", cy "2.34375", r "2.34375" ] []
                                    , Svg.path [ d "M16.40625,3.90625 L16.40625,7.8125", id "Line-2", stroke "#062140", strokeWidth "2", fill "none", strokeLinecap "square" ] []
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
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
