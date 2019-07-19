module FrontendModuler.Spinner exposing (Spinner, Størrelse(..), spinner, toHtml, withStørrelse)

import Html exposing (Html)
import Svg
import Svg.Attributes exposing (..)


type Spinner
    = Spinner Options


type alias Options =
    { størrelse : Størrelse
    }


type Størrelse
    = M
    | L
    | XL


spinner : Spinner
spinner =
    Spinner
        { størrelse = M
        }


withStørrelse : Størrelse -> Spinner -> Spinner
withStørrelse størrelse (Spinner options) =
    Spinner { options | størrelse = størrelse }


toHtml : Spinner -> Html msg
toHtml (Spinner options) =
    Svg.svg [ class ("spinner " ++ størrelseClass options.størrelse), height "32", width "32", viewBox "0 0 50 50", preserveAspectRatio "xMidYMid" ]
        [ Svg.title [] [ Svg.text "Venter..." ]
        , Svg.circle [ cx "25", cy "25", r "20", stroke "#bdbab7", fill "none", strokeWidth "5" ] []
        , Svg.circle [ cx "25", cy "25", r "20", stroke "#7f756c", fill "none", strokeWidth "5", strokeDasharray "50 155", strokeLinecap "round" ] []
        ]


størrelseClass : Størrelse -> String
størrelseClass størrelse =
    case størrelse of
        M ->
            "spinner--m"

        L ->
            "spinner--l"

        XL ->
            "spinner--xl"
