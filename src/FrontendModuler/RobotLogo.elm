module FrontendModuler.RobotLogo exposing (robotLogo)

import Html exposing (Html)
import Html.Attributes.Aria exposing (ariaLabel, role)
import Svg exposing (svg)
import Svg.Attributes exposing (..)


robotLogo : Html msg
robotLogo =
    svg [ id "icon-vectorpaint", viewBox "0 0 32 32", role "img", ariaLabel "Roboten" ]
        [ Svg.path [ fill "#40c1ac", style "fill: var(--color1, #40c1ac)", d "M32 16c0 8.837-7.163 16-16 16s-16-7.163-16-16c0-8.837 7.163-16 16-16s16 7.163 16 16z" ] []
        , Svg.path [ fill "#e6f4ef", style "fill: var(--color2, #e6f4ef); stroke: var(--color3, #062140)", stroke "#062140", strokeLinejoin "miter", strokeLinecap "butt", strokeMiterlimit "4", strokeWidth "1.28", d "M11.5 24.14h8.5c2.684 0 4.86 2.176 4.86 4.86s-2.176 4.86-4.86 4.86h-8.5c-2.684 0-4.86-2.176-4.86-4.86s2.176-4.86 4.86-4.86z" ] []
        , Svg.path [ fill "#e6f4ef", style "fill: var(--color2, #e6f4ef); stroke: var(--color3, #062140)", stroke "#062140", strokeLinejoin "miter", strokeLinecap "butt", strokeMiterlimit "4", strokeWidth "1.28", d "M12.12 9.64h7.26c2.474 0 4.48 2.006 4.48 4.48v3.26c0 2.474-2.006 4.48-4.48 4.48h-7.26c-2.474 0-4.48-2.006-4.48-4.48v-3.26c0-2.474 2.006-4.48 4.48-4.48z" ] []
        , Svg.path [ fill "#e6f4ef", style "fill: var(--color2, #e6f4ef); stroke: var(--color3, #062140)", stroke "#062140", strokeLinejoin "miter", strokeLinecap "butt", strokeMiterlimit "4", strokeWidth "1.28", d "M6.5 14.14c0.475 0 0.86 0.385 0.86 0.86v1.5c0 0.475-0.385 0.86-0.86 0.86s-0.86-0.385-0.86-0.86v-1.5c0-0.475 0.385-0.86 0.86-0.86z" ] []
        , Svg.path [ fill "#e6f4ef", style "fill: var(--color2, #e6f4ef); stroke: var(--color3, #062140)", stroke "#062140", strokeLinejoin "miter", strokeLinecap "butt", strokeMiterlimit "4", strokeWidth "1.28", d "M25 14.14c0.475 0 0.86 0.385 0.86 0.86v1.5c0 0.475-0.385 0.86-0.86 0.86s-0.86-0.385-0.86-0.86v-1.5c0-0.475 0.385-0.86 0.86-0.86z" ] []
        , Svg.path [ fill "#062140", style "fill: var(--color3, #062140)", d "M13.5 14.5c0 0.552-0.448 1-1 1s-1-0.448-1-1c0-0.552 0.448-1 1-1s1 0.448 1 1z" ] []
        , Svg.path [ fill "#062140", style "fill: var(--color3, #062140)", d "M20 14.5c0 0.552-0.448 1-1 1s-1-0.448-1-1c0-0.552 0.448-1 1-1s1 0.448 1 1z" ] []
        , Svg.path [ fill "none", stroke "#062140", style "stroke: var(--color3, #062140)", strokeLinejoin "miter", strokeLinecap "round", strokeMiterlimit "4", strokeWidth "1.28", d "M14 17.557c0.517 0.833 1.1 1.25 1.75 1.25s1.233-0.417 1.75-1.25" ] []
        , Svg.path [ fill "#062140", style "fill: var(--color3, #062140)", d "M17 5.5c0 0.828-0.672 1.5-1.5 1.5s-1.5-0.672-1.5-1.5c0-0.828 0.672-1.5 1.5-1.5s1.5 0.672 1.5 1.5z" ] []
        , Svg.path [ fill "none", stroke "#062140", style "stroke: var(--color3, #062140)", strokeLinejoin "miter", strokeLinecap "square", strokeMiterlimit "4", strokeWidth "1.28", d "M15.5 6.5v2.5" ] []
        ]
