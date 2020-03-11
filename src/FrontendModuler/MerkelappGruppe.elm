module FrontendModuler.MerkelappGruppe exposing (MerkelappGruppe, merkelappGruppe, merkelappListe, toHtml)

import FrontendModuler.Merkelapp as Merkelapp exposing (Merkelapp)
import Html exposing (..)
import Html.Attributes exposing (..)


type MerkelappGruppe msg
    = MerkelappGruppe (Info msg)


type alias Info msg =
    { merkelapper : List (Merkelapp msg)
    }


merkelappGruppe : List (Merkelapp msg) -> MerkelappGruppe msg
merkelappGruppe merkelapper =
    MerkelappGruppe
        { merkelapper = merkelapper
        }


toHtml : MerkelappGruppe msg -> Html msg
toHtml (MerkelappGruppe options) =
    if List.length options.merkelapper > 0 then
        div
            [ class "skjemaelement" ]
            [ span [ class "skjemaelement__label" ]
                [ text "Dette har du valgt: "
                , span [ class "onlyScreenReader" ] [ text "Valgte elementer, kan slettes med enter" ]
                ]
            , div
                [ class "Merkelapp__wrapper" ]
                (List.map
                    Merkelapp.toHtml
                    options.merkelapper
                )
            ]

    else
        text ""


merkelappListe : MerkelappGruppe msg -> List (Merkelapp msg)
merkelappListe (MerkelappGruppe options) =
    options.merkelapper
