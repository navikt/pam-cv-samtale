module FrontendModuler.ArbeidsplassenHeader exposing (..)

import FrontendModuler.Header as Header
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (onClick)


header : { navn : String, underOppfølging : Bool, open : Bool, onOpenToggle : Bool -> msg } -> ArbeidsplassenHeader msg
header { navn, underOppfølging, open, onOpenToggle } =
    ArbeidsplassenHeader
        { navn = navn
        , underOppfølging = underOppfølging
        , open = open
        , onOpenToggle = onOpenToggle
        }


type ArbeidsplassenHeader msg
    = ArbeidsplassenHeader (Options msg)


type alias Options msg =
    { navn : String
    , underOppfølging : Bool
    , open : Bool
    , onOpenToggle : Bool -> msg
    }



--- HTML ---


toHtml : ArbeidsplassenHeader msg -> Html msg
toHtml (ArbeidsplassenHeader options) =
    div [ class "Header__wrapper Header__wrapper__border" ]
        [ div [ class "Header" ]
            [ div [ class "Header__topp" ]
                [ div [ class "Header__logo" ]
                    [ a [ ariaLabel "Logo Arbeidsplassen", href "/" ]
                        [ Header.arbeidsplassenLogo ]
                    ]
                , div [ class "Header__Authentication" ]
                    [ div [ class "Header__Authentication__buttons" ]
                        [ div [ class "Header__Innstillinger__wrapper" ]
                            [ aktivitetsplanLenke options
                            , div []
                                [ a [ class "Header__Innstillinger typo-normal", href "/personinnstillinger" ]
                                    [ div [ class "Header__Innstillinger-inner" ]
                                        [ span [ class "Header__Innstillinger__text" ]
                                            [ text "Innstillinger" ]
                                        , span [ class "Header__Tannhjul" ]
                                            []
                                        ]
                                    ]
                                ]
                            , navnPåBruker options
                            , loggUt
                            ]
                        ]
                    ]
                , div [ class "Header__Authentication--mobile" ]
                    [ div
                        [ ariaControls "Mobilmeny"
                        , ariaExpandedAttribute options.open
                        , class "Mobilmeny__Button--toggle"
                        , id "Mobilmeny__Button--toggle"
                        , role "button"
                        , onClick (options.onOpenToggle (not options.open))
                        ]
                        (if options.open then
                            [ div [ class "Mobilmeny--lukk-wrapper" ]
                                [ div [ class "Mobilmeny--lukk" ] [] ]
                            , span [ class "Mobilmeny__Text--toggle" ]
                                [ text "Lukk" ]
                            ]

                         else
                            [ div [ class "Mobilmeny--apne" ] []
                            , span [ class "Mobilmeny__Text--toggle" ]
                                [ text "Meny" ]
                            ]
                        )
                    ]
                ]
            , if options.open then
                menyMobil options

              else
                text ""
            , div [ class "Menu__wrapper" ]
                [ menypunkter ]
            ]
        ]


ariaExpandedAttribute : Bool -> Html.Attribute msg
ariaExpandedAttribute open =
    ariaExpanded
        (if open then
            "true"

         else
            "false"
        )


menyMobil : Options msg -> Html msg
menyMobil options =
    div [ class "Headermenu__mobile" ]
        [ div []
            [ div [ class "Menu__wrapper__mobile" ]
                [ menypunkter ]
            , div [ class "Header__Authentication__logout" ]
                [ aktivitetsplanLenke options
                , div [ class "Header__name__wrapper" ]
                    [ navnPåBruker options
                    , loggUt
                    ]
                ]
            ]
        ]


menypunkter : Html msg
menypunkter =
    nav [ class "Personbrukermeny" ]
        [ menyLenke False MinSide
        , menyLenke False Stillinger
        , menyLenke False Favoritter
        , menyLenke False LagredeSøk
        , menyLenke True Cv
        , menyLenke False Jobbprofil
        , menyLenke False Innstillinger
        ]


type Menypunkt
    = MinSide
    | Stillinger
    | Favoritter
    | LagredeSøk
    | Cv
    | Jobbprofil
    | Innstillinger


menypunktUrl : Menypunkt -> String
menypunktUrl menypunkt =
    case menypunkt of
        MinSide ->
            "/minside"

        Stillinger ->
            "/stillinger"

        Favoritter ->
            "/stillinger/favoritter"

        LagredeSøk ->
            "/stillinger/lagrede-sok"

        Cv ->
            "/cv"

        Jobbprofil ->
            "/jobbprofil"

        Innstillinger ->
            "/personinnstillinger"


menypunktTekst : Menypunkt -> String
menypunktTekst menypunkt =
    case menypunkt of
        MinSide ->
            "Min side"

        Stillinger ->
            "Ledige stillinger"

        Favoritter ->
            "Favoritter"

        LagredeSøk ->
            "Lagrede søk"

        Cv ->
            "CV"

        Jobbprofil ->
            "Jobbprofil"

        Innstillinger ->
            "Innstillinger"


menyLenke : Bool -> Menypunkt -> Html msg
menyLenke aktiv menypunkt =
    div
        [ classList
            [ ( "Personbrukermeny--lenke-wrapper", menypunkt /= Cv )
            , ( "Personbrukermeny--lenke-wrapper-CV", menypunkt == Cv )
            , ( "Personbrukermeny__Innstillinger--mobile", menypunkt == Innstillinger )
            ]
        , role "list"
        ]
        [ a
            [ class "Personbrukermeny--lenke"
            , classList [ ( "Personbrukermeny--lenke-active", aktiv ) ]
            , href (menypunktUrl menypunkt)
            , if aktiv then
                attribute "aria-current" "page"

              else
                noAttribute
            ]
            [ div [ class "Personbrukermeny--lenke-inner" ]
                [ span []
                    [ text (menypunktTekst menypunkt) ]
                ]
            ]
        ]


aktivitetsplanLenke : Options msg -> Html msg
aktivitetsplanLenke options =
    if options.underOppfølging then
        a [ class "Header__AktivitetsplanLenke", href "https://aktivitetsplan.nav.no" ]
            [ div [ class "Header__AktivitetsplanLenke-inner" ]
                [ span [ class "Header__AktivitetsplanLenke__text" ]
                    [ text "Aktivitetsplan" ]
                , span [ class "Header__Lenkeikon" ]
                    []
                ]
            ]

    else
        text ""


navnPåBruker : Options msg -> Html msg
navnPåBruker options =
    div [ class "Header__name" ]
        [ text options.navn ]


loggUt : Html msg
loggUt =
    div [ class "Header__logout-name" ]
        [ button [ class "Header__Button Header__Button--mini" ]
            [ text "Logg ut" ]
        ]


noAttribute : Html.Attribute msg
noAttribute =
    classList []
