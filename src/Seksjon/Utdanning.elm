module Seksjon.Utdanning exposing (Model, Msg, SamtaleStatus(..), init, meldingsLogg, update, viewBrukerInput)

import Cv.Utdanning as Cv exposing (Utdanning)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (MeldingsLogg)



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , utdanningListe : List Utdanning
    }


type Samtale
    = Intro
    | RegistrerNivå
    | RegistrerRetning RetningInfo
    | RegistrerSkole SkoleInfo
    | RegistrerBeskrivelse BeskrivelseInfo
    | RegistrerPeriode PeriodeInfo
    | Oppsummering OppsummeringInfo


type Nivå
    = Grunnskole
    | VideregåendeYrkesskole
    | Fagskole
    | Folkehøyskole
    | HøyereUtdanning1til4
    | HøyereUtdanning4pluss
    | Phd


type alias RetningInfo =
    { forrige : Nivå, retning : String }


type alias SkoleInfo =
    { forrige : RetningInfo, skole : String }


type alias BeskrivelseInfo =
    { forrige : SkoleInfo, beskrivelse : String }


type alias PeriodeInfo =
    { forrige : BeskrivelseInfo, periode : String }


type alias OppsummeringInfo =
    { forrige : PeriodeInfo, oppsummering : String }


nivåToString : Nivå -> String
nivåToString nivåInfo =
    case nivåInfo of
        Grunnskole ->
            "Grunnskole"

        VideregåendeYrkesskole ->
            "Videregående/Yrkesskole"

        Fagskole ->
            "Fagskole"

        Folkehøyskole ->
            "Folkehøyskole"

        HøyereUtdanning1til4 ->
            "Høyere Utdanning (1-4 år)"

        HøyereUtdanning4pluss ->
            "Høyere Utdanning (mer enn 4 år)"

        Phd ->
            "PhD"


forrigeTilRetningInfo : Nivå -> RetningInfo
forrigeTilRetningInfo nivå =
    { forrige = nivå, retning = "" }


forrigeTilSkoleInfo : RetningInfo -> SkoleInfo
forrigeTilSkoleInfo retningInfo =
    { forrige = retningInfo, skole = "" }


forrigeTilBeskrivelseInfo : SkoleInfo -> BeskrivelseInfo
forrigeTilBeskrivelseInfo skoleInfo =
    { forrige = skoleInfo, beskrivelse = "" }


forrigeTilPeriodeInfo : BeskrivelseInfo -> PeriodeInfo
forrigeTilPeriodeInfo beskrivelseInfo =
    { forrige = beskrivelseInfo, periode = "" }


forrigeTilOppsummeringInfo : PeriodeInfo -> OppsummeringInfo
forrigeTilOppsummeringInfo periodeInfo =
    { forrige = periodeInfo, oppsummering = "" }


type Msg
    = BrukerVilRegistrereUtdanning
    | GåTilArbeidserfaring
    | BekreftAlleredeRegistrert
    | BrukerVilRegistrereNivå Nivå
    | BrukerVilRegistrereRetning
    | OppdaterRetning String
    | BrukerVilRegistrereSkole
    | OppdaterSkole String
    | BrukerVilRegistrereBeskrivelse
    | OppdaterBeskrivelse String
    | BrukerVilRegistrerePeriode
    | OppdaterPeriode String
    | BrukerVilRegistrereOppsummering
    | OppdaterOppsummering String


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Utdanning) MeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



--- UPDATE ---


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        BrukerVilRegistrereUtdanning ->
            case model.aktivSamtale of
                Intro ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Jeg vil registrere utdannning" ]) RegistrerNivå
                        , Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        GåTilArbeidserfaring ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.leggTilSvar (Melding.svar [ "Jeg har ingen utdanning" ])
                |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Da fortsetter vi med arbeidserfaringen din" ] ]
                |> Ferdig model.utdanningListe

        BrukerVilRegistrereNivå nivå ->
            case model.aktivSamtale of
                RegistrerNivå ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Jeg valgte nivået: " ++ nivåToString nivå ]) (RegistrerRetning (forrigeTilRetningInfo nivå)), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereRetning ->
            case model.aktivSamtale of
                RegistrerRetning retninginfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Studieretningen min var: " ++ retninginfo.retning ]) (RegistrerSkole (forrigeTilSkoleInfo retninginfo)), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereSkole ->
            case model.aktivSamtale of
                RegistrerSkole skoleinfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Skolen jeg gikk på het: " ++ skoleinfo.skole ]) (RegistrerBeskrivelse (forrigeTilBeskrivelseInfo skoleinfo)), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereBeskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse beskrivelseinfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Dette føler jeg beskriver utdannelsen: " ++ beskrivelseinfo.beskrivelse ]) (RegistrerPeriode (forrigeTilPeriodeInfo beskrivelseinfo)), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrerePeriode ->
            case model.aktivSamtale of
                RegistrerPeriode periodeinfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Utdannelsen gjaldt for perioden: " ++ periodeinfo.periode ]) (Oppsummering (forrigeTilOppsummeringInfo periodeinfo)), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BekreftAlleredeRegistrert ->
            IkkeFerdig ( Model model, Cmd.none )

        OppdaterRetning retning ->
            case model.aktivSamtale of
                RegistrerRetning retningsinfo ->
                    IkkeFerdig ( oppdaterSamtaleSteg model (RegistrerRetning { retningsinfo | retning = retning }), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterSkole skole ->
            case model.aktivSamtale of
                RegistrerSkole skoleinfo ->
                    IkkeFerdig ( oppdaterSamtaleSteg model (RegistrerSkole { skoleinfo | skole = skole }), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterBeskrivelse beskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse beskrivelseinfo ->
                    IkkeFerdig ( oppdaterSamtaleSteg model (RegistrerBeskrivelse { beskrivelseinfo | beskrivelse = beskrivelse }), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterPeriode periode ->
            case model.aktivSamtale of
                RegistrerPeriode periodeinfo ->
                    IkkeFerdig ( oppdaterSamtaleSteg model (RegistrerPeriode { periodeinfo | periode = periode }), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereOppsummering ->
            IkkeFerdig ( Model model, Cmd.none )

        OppdaterOppsummering string ->
            IkkeFerdig ( Model model, Cmd.none )


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg model melding samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg utdanningSeksjon =
    case utdanningSeksjon of
        Intro ->
            [ Melding.spørsmål
                [ "Nå skal vi legge til din utdanning" ]
            ]

        RegistrerNivå ->
            [ Melding.spørsmål
                [ "Hvilket nivå var utdanningen på?" ]
            ]

        RegistrerRetning _ ->
            [ Melding.spørsmål
                [ "Hvilken studieretning gikk du?" ]
            ]

        RegistrerSkole _ ->
            [ Melding.spørsmål
                [ " Hvilken skole gikk du på? " ]
            ]

        RegistrerBeskrivelse _ ->
            [ Melding.spørsmål
                [ " Kan du beskrive denne utdannelsen din for meg? " ]
            ]

        RegistrerPeriode _ ->
            [ Melding.spørsmål
                [ " Hvilken periode var det i? " ]
            ]

        Oppsummering _ ->
            [ Melding.spørsmål
                [ "Her har du en oppsummering av utdannelsen. Stemmer den? " ]
            ]


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model { aktivSamtale }) =
    case aktivSamtale of
        Intro ->
            div []
                [ button [ onClick BrukerVilRegistrereUtdanning ] [ text "Jeg vil registrere utdanning" ]
                , button [ onClick GåTilArbeidserfaring ] [ text "Jeg har ingen utdanning" ]
                ]

        RegistrerNivå ->
            div []
                [ label [] [ text "Nivå: " ]
                , button [ onClick (BrukerVilRegistrereNivå Grunnskole) ] [ text (nivåToString Grunnskole) ]
                , button [ onClick (BrukerVilRegistrereNivå VideregåendeYrkesskole) ] [ text (nivåToString VideregåendeYrkesskole) ]
                , button [ onClick (BrukerVilRegistrereNivå Fagskole) ] [ text (nivåToString Fagskole) ]
                , button [ onClick (BrukerVilRegistrereNivå Folkehøyskole) ] [ text (nivåToString Folkehøyskole) ]
                , button [ onClick (BrukerVilRegistrereNivå HøyereUtdanning1til4) ] [ text (nivåToString HøyereUtdanning1til4) ]
                , button [ onClick (BrukerVilRegistrereNivå HøyereUtdanning4pluss) ] [ text (nivåToString HøyereUtdanning4pluss) ]
                , button [ onClick (BrukerVilRegistrereNivå Phd) ] [ text (nivåToString Phd) ]
                ]

        RegistrerRetning retningsinfo ->
            div []
                [ label [] [ text "Retning: " ]
                , input
                    [ onInput OppdaterRetning
                    , value retningsinfo.retning
                    ]
                    []
                , button
                    [ onClick BrukerVilRegistrereRetning ]
                    [ text "Lagre"
                    ]
                ]

        RegistrerSkole skoleinfo ->
            div []
                [ label [] [ text "Skole: " ]
                , input
                    [ onInput OppdaterSkole
                    , value skoleinfo.skole
                    ]
                    []
                , button
                    [ onClick BrukerVilRegistrereSkole ]
                    [ text "Lagre"
                    ]
                ]

        RegistrerBeskrivelse beskrivelseinfo ->
            div []
                [ label [] [ text "Beskrivelse: " ] -- Maks char?
                , input
                    [ onInput OppdaterBeskrivelse, value beskrivelseinfo.beskrivelse ]
                    []
                , button
                    [ onClick BrukerVilRegistrereBeskrivelse ]
                    [ text "Lagre"
                    ]
                ]

        RegistrerPeriode periodeinfo ->
            div []
                [ label [] [ text "Periode: " ] -- Bruke datetime?
                , input
                    [ onInput OppdaterPeriode
                    , value periodeinfo.periode
                    ]
                    []
                , button
                    [ onClick BrukerVilRegistrerePeriode ]
                    [ text "Lagre"
                    ]
                ]

        Oppsummering oppsummeringinfo ->
            div []
                [ label [] [ text "Oppsummering: " ] --Skulle dette være et skjema man godkjenner?
                , input
                    [ onInput OppdaterOppsummering
                    , value oppsummeringinfo.oppsummering
                    ]
                    []
                , button
                    [ onClick BrukerVilRegistrereOppsummering ]
                    [ text "Godkjenn"
                    ]
                ]



--- INIT ---


init : MeldingsLogg -> List Utdanning -> Model
init gammelMeldingsLogg utdanningListe =
    let
        aktivSamtale =
            Intro
    in
    Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål
                (samtaleTilMeldingsLogg aktivSamtale)
                gammelMeldingsLogg
        , aktivSamtale = aktivSamtale
        , utdanningListe = utdanningListe
        }
