module FrontendModuler.RadioGruppe exposing (RadioGruppe, radioGruppe, radioKnapper, toHtml, withFeilmelding)

import FrontendModuler.Feilmelding exposing (htmlFeilmelding)
import FrontendModuler.Radio as Radio exposing (Radio)
import Html exposing (..)
import Html.Attributes exposing (..)


type RadioGruppe msg
    = RadioGruppe (Info msg)


type alias Info msg =
    { legend : String
    , fokusId : Maybe String
    , radioknapper : List (Radio msg)
    , feilmelding : Maybe String
    , obligatorisk : Bool
    }


type alias RadioGruppeInfo msg =
    { legend : String
    , radioknapper : List (Radio msg)
    }


radioGruppe : RadioGruppeInfo msg -> RadioGruppe msg
radioGruppe { legend, radioknapper } =
    RadioGruppe
        { legend = legend
        , fokusId = Nothing
        , radioknapper = radioknapper
        , feilmelding = Nothing

        -- alltid true for nå
        , obligatorisk = True
        }


withFeilmelding : Maybe String -> RadioGruppe msg -> RadioGruppe msg
withFeilmelding feilmelding (RadioGruppe info) =
    RadioGruppe { info | feilmelding = feilmelding }


withErObligatorisk : RadioGruppe msg -> RadioGruppe msg
withErObligatorisk (RadioGruppe options) =
    RadioGruppe { options | obligatorisk = True }


toHtml : RadioGruppe msg -> Html msg
toHtml (RadioGruppe options) =
    fieldset
        [ class "radio-checkbox-fieldset radio-checkbox-gruppe-wrapper"
        ]
        [ legend [ class "skjemaelement__label" ]
            (if options.obligatorisk then
                [ text options.legend
                , span [ class "skjemaelement__måFyllesUt" ] [ text " - må fylles ut" ]
                ]

             else
                [ text options.legend ]
            )
        , div
            [ classList
                [ ( "radio-gruppe-feilområde", options.feilmelding /= Nothing )
                ]
            ]
            (List.map
                Radio.toHtml
                options.radioknapper
            )
        , htmlFeilmelding options.feilmelding
        ]


radioKnapper : RadioGruppe msg -> List (Radio msg)
radioKnapper (RadioGruppe options) =
    options.radioknapper
