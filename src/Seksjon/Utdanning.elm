module Seksjon.Utdanning exposing (Model, Msg, SamtaleStatus(..), init, meldingsLogg, update, viewBrukerInput)

import Browser.Dom as Dom
import Cv.Utdanning as Cv exposing (Nivå(..), Utdanning)
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (MeldingsLogg)
import SamtaleAnimasjon
import Skjema.Utdanning as Skjema
    exposing
        ( Felt(..)
        , oppdaterBeskrivelse
        , oppdaterFradato
        , oppdaterNavarende
        , oppdaterNuskode
        , oppdaterStudiested
        , oppdaterTildato
        , oppdaterUtdanningsretning
        )



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
    | RegistrerSkole SkoleInfo
    | RegistrerRetning RetningInfo
    | RegistrerBeskrivelse BeskrivelseInfo
    | RegistrerPeriode PeriodeInfo
    | Oppsummering Skjema.UtdanningSkjema
    | EndrerOppsummering Skjema.UtdanningSkjema
    | LagrerOppsummering OppsummeringInfo


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Utdanning) MeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type alias SkoleInfo =
    { forrige : Nivå, skole : String }


type alias RetningInfo =
    { forrige : SkoleInfo, retning : String }


type alias BeskrivelseInfo =
    { forrige : RetningInfo, beskrivelse : String }


type alias PeriodeInfo =
    { forrige : BeskrivelseInfo, periode : String }


type alias OppsummeringInfo =
    { nivå : Nivå
    , skole : String
    , retning : String
    , beskrivelse : String
    , periode : String
    }


nivåToString : Nivå -> String
nivåToString nivå =
    case nivå of
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


forrigeTilRetningInfo : SkoleInfo -> RetningInfo
forrigeTilRetningInfo skole =
    { forrige = skole, retning = "" }


forrigeTilSkoleInfo : Nivå -> SkoleInfo
forrigeTilSkoleInfo nivå =
    { forrige = nivå, skole = "" }


forrigeTilBeskrivelseInfo : RetningInfo -> BeskrivelseInfo
forrigeTilBeskrivelseInfo retning =
    { forrige = retning, beskrivelse = "" }


forrigeTilPeriodeInfo : BeskrivelseInfo -> PeriodeInfo
forrigeTilPeriodeInfo beskrivelse =
    { forrige = beskrivelse, periode = "" }


forrigeTilOppsummeringInfo : PeriodeInfo -> Skjema.UtdanningSkjema
forrigeTilOppsummeringInfo periodeInfo =
    Skjema.initManueltSkjema
        { nuskode = periodeInfo.forrige.forrige.forrige.forrige
        , studiested = periodeInfo.forrige.forrige.forrige.skole
        , utdanningsretning = periodeInfo.forrige.forrige.retning
        , beskrivelse = periodeInfo.forrige.beskrivelse
        , fradato = periodeInfo.periode
        , tildato = periodeInfo.periode
        , navarende = True
        }


type Msg
    = BrukerVilRegistrereUtdanning
    | GåTilArbeidserfaring
    | BekreftAlleredeRegistrert
    | BrukerVilRegistrereNivå Nivå
    | BrukerVilRegistrereSkole
    | OppdaterSkole String
    | BrukerVilRegistrereRetning
    | OppdaterRetning String
    | BrukerVilRegistrereBeskrivelse
    | OppdaterBeskrivelse String
    | BrukerVilRegistrerePeriode
    | OppdaterPeriode String
    | BrukerVilEndreOppsummering
    | OriginalOppsummeringBekreftet
    | OppsummeringEndret Skjema.Felt String
    | OppsummeringSkjemaLagreknappTrykket
    | SkjemaOppdatert SkjemaEndring
    | ViewportSatt (Result Dom.Error ())


type SkjemaEndring
    = NivåEndret Nivå
    | BeskrivelseEndret String
    | SkoleEndret String
    | FratidEndret String
    | TiltidEndret String
    | NavarendeEndret Bool
    | RetningEndret String



--- UPDATE ---


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        BrukerVilRegistrereUtdanning ->
            case model.aktivSamtale of
                Intro ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Jeg vil registrere utdannning" ]) RegistrerNivå
                        , SamtaleAnimasjon.scrollTilBunn ViewportSatt
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
                        ( nesteSamtaleSteg model (Melding.svar [ nivåToString nivå ]) (RegistrerSkole (forrigeTilSkoleInfo nivå))
                        , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereSkole ->
            case model.aktivSamtale of
                RegistrerSkole skoleinfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ skoleinfo.skole ]) (RegistrerRetning (forrigeTilRetningInfo skoleinfo))
                        , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereRetning ->
            case model.aktivSamtale of
                RegistrerRetning retninginfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ retninginfo.retning ]) (RegistrerBeskrivelse (forrigeTilBeskrivelseInfo retninginfo))
                        , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereBeskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse beskrivelseinfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ beskrivelseinfo.beskrivelse ]) (RegistrerPeriode (forrigeTilPeriodeInfo beskrivelseinfo))
                        , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrerePeriode ->
            case model.aktivSamtale of
                RegistrerPeriode periodeinfo ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ periodeinfo.periode ]) (Oppsummering (forrigeTilOppsummeringInfo periodeinfo))
                        , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilEndreOppsummering ->
            case model.aktivSamtale of
                Oppsummering utdanningskjema ->
                    IkkeFerdig ( nesteSamtaleSteg model (Melding.svar [ "Jeg vil endre" ]) (EndrerOppsummering utdanningskjema), SamtaleAnimasjon.scrollTilBunn ViewportSatt )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppsummeringEndret felt string ->
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

        SkjemaOppdatert skjemaendring ->
            case model.aktivSamtale of
                EndrerOppsummering skjemaendringinfo ->
                    IkkeFerdig ( oppdaterSamtaleSteg model (EndrerOppsummering (oppdaterSkjema skjemaendring skjemaendringinfo)), Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OriginalOppsummeringBekreftet ->
            IkkeFerdig ( Model model, SamtaleAnimasjon.scrollTilBunn ViewportSatt )

        OppsummeringSkjemaLagreknappTrykket ->
            IkkeFerdig ( Model model, SamtaleAnimasjon.scrollTilBunn ViewportSatt )

        ViewportSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )


oppdaterSkjema : SkjemaEndring -> Skjema.UtdanningSkjema -> Skjema.UtdanningSkjema
oppdaterSkjema endring skjema =
    case endring of
        NivåEndret nivå ->
            Skjema.oppdaterNuskode nivå skjema

        BeskrivelseEndret string ->
            Skjema.oppdaterBeskrivelse string skjema

        SkoleEndret string ->
            Skjema.oppdaterStudiested string skjema

        FratidEndret string ->
            Skjema.oppdaterFradato string skjema

        TiltidEndret string ->
            Skjema.oppdaterTildato string skjema

        NavarendeEndret bool ->
            Skjema.oppdaterNavarende bool skjema

        RetningEndret string ->
            Skjema.oppdaterUtdanningsretning string skjema


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

        RegistrerSkole _ ->
            [ Melding.spørsmål
                [ " Hvilken skole gikk du på? " ]
            ]

        RegistrerRetning _ ->
            [ Melding.spørsmål
                [ "Hvilken studieretning gikk du?" ]
            ]

        RegistrerBeskrivelse _ ->
            [ Melding.spørsmål
                [ " Kan du beskrive denne utdannelsen din for meg? " ]
            ]

        RegistrerPeriode _ ->
            [ Melding.spørsmål
                [ " Hvilken periode var det i? " ]
            ]

        --TODO: Implementer fra og til/nåværende
        Oppsummering utdanningsskjema ->
            [ Melding.spørsmål
                [ "Her har du en oppsummering av utdannelsen du la inn:"
                , "Nivå: " ++ nivåToString (Skjema.nuskode utdanningsskjema)
                , "Skole: " ++ Skjema.studiested utdanningsskjema
                , "Retning: " ++ Skjema.utdanningsretning utdanningsskjema
                , "Beskrivelse: " ++ Skjema.beskrivelse utdanningsskjema
                , "Periode: " ++ Skjema.fradato utdanningsskjema
                ]
            ]

        EndrerOppsummering _ ->
            [ Melding.spørsmål
                [ "Ok! Vennligst skriv inn riktig informasjon i feltene under: " ]
            ]

        LagrerOppsummering _ ->
            [ Melding.spørsmål
                [ "Godt jobbet! Da tar jeg vare på den nye infoen! " ]
            ]


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model { aktivSamtale }) =
    case aktivSamtale of
        Intro ->
            div [ class "inputrad" ]
                [ div [ class "inputrad-innhold" ]
                    [ Knapp.knapp BrukerVilRegistrereUtdanning "Jeg vil registrere utdanning"
                        |> Knapp.toHtml
                    , Knapp.knapp GåTilArbeidserfaring "Jeg har ingen utdanning"
                        |> Knapp.toHtml
                    ]
                ]

        RegistrerNivå ->
            div [ class "inputrad" ]
                [ div [ class "Utdanningsnivå" ]
                    [ Knapp.knapp (BrukerVilRegistrereNivå Grunnskole) (nivåToString Grunnskole) |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå VideregåendeYrkesskole) (nivåToString VideregåendeYrkesskole) |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå Fagskole) (nivåToString Fagskole) |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå Folkehøyskole) (nivåToString Folkehøyskole) |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå HøyereUtdanning1til4) (nivåToString HøyereUtdanning1til4) |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå HøyereUtdanning4pluss) (nivåToString HøyereUtdanning4pluss) |> Knapp.toHtml
                    , Knapp.knapp (BrukerVilRegistrereNivå Phd) (nivåToString Phd) |> Knapp.toHtml
                    ]
                ]

        RegistrerSkole skoleinfo ->
            div [ class "Skjema" ]
                [ skoleinfo.skole |> Input.input { msg = OppdaterSkole, label = "Skole" } |> Input.toHtml
                , Knapp.knapp BrukerVilRegistrereSkole "Lagre"
                    |> Knapp.toHtml
                ]

        RegistrerRetning retningsinfo ->
            div [ class "Skjema" ]
                [ retningsinfo.retning |> Input.input { msg = OppdaterRetning, label = "Retning" } |> Input.toHtml
                , Knapp.knapp BrukerVilRegistrereRetning "Lagre"
                    |> Knapp.toHtml
                ]

        RegistrerBeskrivelse beskrivelseinfo ->
            div [ class "Skjema" ]
                [ beskrivelseinfo.beskrivelse |> Input.input { msg = OppdaterBeskrivelse, label = "Beskrivelse" } |> Input.toHtml
                , Knapp.knapp BrukerVilRegistrereBeskrivelse "Lagre"
                    |> Knapp.toHtml
                ]

        RegistrerPeriode periodeinfo ->
            div [ class "Skjema" ]
                [ periodeinfo.periode |> Input.input { msg = OppdaterPeriode, label = "Periode" } |> Input.toHtml
                , Knapp.knapp BrukerVilRegistrerePeriode "Lagre"
                    |> Knapp.toHtml
                ]

        Oppsummering _ ->
            div [ class "inputrad" ]
                [ Knapp.knapp BrukerVilEndreOppsummering "Endre"
                    |> Knapp.toHtml
                , Knapp.knapp OriginalOppsummeringBekreftet "Bekreft"
                    |> Knapp.toHtml
                ]

        EndrerOppsummering utdanningsskjema ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ utdanningsskjema
                        |> Skjema.studiested
                        |> Input.input { label = "Studiested", msg = SkoleEndret >> SkjemaOppdatert }
                        |> Input.toHtml
                    , utdanningsskjema
                        |> Skjema.utdanningsretning
                        |> Input.input { label = "Retning", msg = RetningEndret >> SkjemaOppdatert }
                        |> Input.toHtml
                    , utdanningsskjema
                        |> Skjema.beskrivelse
                        |> Input.input { label = "Beskrivelse", msg = BeskrivelseEndret >> SkjemaOppdatert }
                        |> Input.toHtml
                    , utdanningsskjema
                        |> Skjema.fradato
                        |> Input.input { label = "Fra ", msg = FratidEndret >> SkjemaOppdatert }
                        |> Input.toHtml
                    , utdanningsskjema
                        |> Skjema.tildato
                        |> Input.input { label = "Til ", msg = TiltidEndret >> SkjemaOppdatert }
                        |> Input.toHtml
                    , div [ class "inputrad" ]
                        [ Knapp.knapp OppsummeringSkjemaLagreknappTrykket "Lagre"
                            |> Knapp.toHtml
                        ]
                    ]
                ]

        LagrerOppsummering _ ->
            text ""


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
