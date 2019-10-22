module FrontendModuler.Containers exposing
    ( KnapperLayout(..)
    , inputMedGåVidereKnapp
    , knapper
    , lenke
    , skjema
    , typeaheadMedGåVidereKnapp
    )

import FrontendModuler.Knapp as Knapp
import Html exposing (..)
import Html.Attributes exposing (..)


skjema : { lagreMsg : msg, lagreKnappTekst : String } -> List (Html msg) -> Html msg
skjema { lagreMsg, lagreKnappTekst } skjemaelementer =
    div [ class "skjema-wrapper" ]
        [ div [ class "skjema" ]
            (List.concat
                [ skjemaelementer
                , [ div []
                        [ Knapp.knapp lagreMsg lagreKnappTekst
                            |> Knapp.toHtml
                        ]
                  ]
                ]
            )
        ]


inputMedGåVidereKnapp : msg -> List (Html msg) -> Html msg
inputMedGåVidereKnapp gåVidereMsg inputelementer =
    div [ class "skjema-wrapper" ]
        [ div [ class "skjema" ]
            (List.concat
                [ inputelementer
                , [ div [ class "gå-videre-knapp" ]
                        [ Knapp.knapp gåVidereMsg "Gå videre"
                            |> Knapp.toHtml
                        ]
                  ]
                ]
            )
        ]


typeaheadMedGåVidereKnapp : msg -> List (Html msg) -> Html msg
typeaheadMedGåVidereKnapp gåVidereMsg inputelementer =
    div [ class "skjema-wrapper" ]
        [ div [ class "skjema typeahead-skjema-height-wrapper" ]
            (List.concat
                [ inputelementer
                , [ div [ class "gå-videre-knapp" ]
                        [ Knapp.knapp gåVidereMsg "Gå videre"
                            |> Knapp.toHtml
                        ]
                  ]
                ]
            )
        ]


type KnapperLayout
    = Flytende
    | Kolonne


knapper : KnapperLayout -> List (Html msg) -> Html msg
knapper layout knappeElementer =
    case layout of
        Flytende ->
            div [ class "knapperad" ]
                [ div [ class "knapper--flytende" ]
                    knappeElementer
                ]

        Kolonne ->
            div [ class "knapperad" ]
                [ div [ class "knapper--kolonne" ]
                    knappeElementer
                ]


lenke : Html msg -> Html msg
lenke lenkeElement =
    div [ class "knapperad" ]
        [ div [ class "knapper--flytende" ]
            [ lenkeElement ]
        ]
