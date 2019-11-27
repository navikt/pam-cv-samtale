module FrontendModuler.Containers exposing
    ( KnapperLayout(..)
    , inputMedEksempelOgGåVidereKnapp
    , inputMedEksempelOgLagreKnapp
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


inputMedEksempelOgGåVidereKnapp : msg -> msg -> List (Html msg) -> Html msg
inputMedEksempelOgGåVidereKnapp eksempelMsg gåVidereMsg inputelementer =
    inputMedToKnapper
        [ { action = eksempelMsg, tekst = "Jeg vil se eksempel" }
        , { action = gåVidereMsg, tekst = "Gå videre" }
        ]
        inputelementer


inputMedEksempelOgLagreKnapp : Maybe msg -> msg -> List (Html msg) -> Html msg
inputMedEksempelOgLagreKnapp eksempelMsg lagreMsg inputelementer =
    inputMedToKnapper
        (case eksempelMsg of
            Just eksempelMsg_ ->
                [ { action = eksempelMsg_, tekst = "Jeg vil se eksempel" }
                , { action = lagreMsg, tekst = "Lagre endringer" }
                ]

            Nothing ->
                [ { action = lagreMsg, tekst = "Lagre endringer" }
                ]
        )
        inputelementer


inputMedToKnapper : List { action : msg, tekst : String } -> List (Html msg) -> Html msg
inputMedToKnapper knappeListe inputelementer =
    div [ class "skjema-wrapper" ]
        [ div [ class "skjema" ]
            (List.concat
                [ inputelementer
                , [ div [ class "knappekolonne" ]
                        (List.map
                            (\{ action, tekst } ->
                                Knapp.knapp action tekst
                                    |> Knapp.toHtml
                            )
                            knappeListe
                        )
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
