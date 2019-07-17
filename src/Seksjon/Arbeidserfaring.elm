module Seksjon.Arbeidserfaring exposing (Model, Msg, SamtaleStatus(..), init, meldingsLogg, update, viewBrukerInput)

import Api
import Cv.Arbeidserfaring exposing (Arbeidserfaring)
import Dato
import Feilmelding
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Melding exposing (Melding)
import MeldingsLogg exposing (MeldingsLogg)



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , arbeidserfaringListe : List Arbeidserfaring
    , aktivSamtale : Samtale
    }


arbeidserfaringListe : Model -> List Arbeidserfaring
arbeidserfaringListe (Model info) =
    info.arbeidserfaringListe


aktivSamtale : Model -> Samtale
aktivSamtale (Model info) =
    info.aktivSamtale


hentAAregArbeidserfaring : Model -> Cmd Msg
hentAAregArbeidserfaring (Model info) =
    Api.hentAAreg HentetAAregArbeidserfaring


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )



--- UPDATE ---


type Msg
    = BrukerOppretterNyArbeidserfaring
    | HentAAregArbeidserfaring
    | HentetAAregArbeidserfaring (Result Http.Error (List Arbeidserfaring))
    | BrukerOppdatererYrke String
    | BrukerLagrerYrke JobbtittelInfo
    | BrukerVilEndreJobbtittel JobbtittelInfo
    | BrukerOppdatererJobbtittelFelt JobbtittelInfo String
    | BrukerVilRegistrereBedriftnavn String BeriftnavnsInfo
    | BrukerOppdatererBedriftnavn BeriftnavnsInfo String
    | BrukerVilRegistrereLokasjon LokasjonInfo
    | BrukerOppdatererLokasjon LokasjonInfo String
    | BrukerVilRegistrereArbeidsoppgaver ArbeidsoppgaverInfo
    | BrukerOppdatererArbeidsoppgaver ArbeidsoppgaverInfo String
    | BrukerVilRegistrereFraMåned PeriodeInfo
    | BrukerOppdatererFradato PeriodeInfo String
    | BrukerOppdatererTildato PeriodeInfo String
    | BrukerOppdatererNaavarende PeriodeInfo
    | ErrorLogget (Result Http.Error ())


type Samtale
    = Intro
    | HentetFraAAreg
    | IkkeHentetFraAAreg
    | IngenArbeidserfaringFraAareg (List Arbeidserfaring)
    | VisArbeidserfaringFraAareg (List Arbeidserfaring)
    | RegistrerYrke YrkeInfo
    | SpørOmBrukerVilEndreJobbtittel JobbtittelInfo
    | EndreJobbtittel JobbtittelInfo
    | RegistrereBedriftNavn BeriftnavnsInfo
    | RegistrereLokasjon LokasjonInfo
    | RegistrereArbeidsoppgaver ArbeidsoppgaverInfo
    | RegistrereFraMåned PeriodeInfo
    | RegistrereFraÅr PeriodeInfo
    | RegistrereNaavarende PeriodeInfo
    | RegistrereTilMåned PeriodeInfo
    | RegistrereTilÅr PeriodeInfo


type alias YrkeInfo =
    { yrke : String }


type alias JobbtittelInfo =
    { tidligereInfo : YrkeInfo
    , jobbtittel : String
    }


type alias BeriftnavnsInfo =
    { tidligereInfo : JobbtittelInfo
    , bedriftNavn : String
    }


type alias LokasjonInfo =
    { tidligereInfo : BeriftnavnsInfo
    , lokasjon : String
    }


type alias ArbeidsoppgaverInfo =
    { tidligereInfo : LokasjonInfo
    , arbeidsoppgaver : String
    }


type alias PeriodeInfo =
    { tidligereInfo : ArbeidsoppgaverInfo
    , fradato : String
    , tildato : String
    , naavarende : Bool
    }


type alias FraDatoInfo =
    { tidligereInfo : ArbeidsoppgaverInfo
    , fraMåned : Dato.Måned
    , fraÅr : String
    }


type Felt
    = YrkeFelt
    | JobbTittelFelt
    | BedriftNavnFelt
    | LokasjonFelt
    | ArbeidsOppgaverFelt
    | FradatoFelt
    | TildatoFelt
    | NavarendeFelt


yrkeInfoTilJobbtittelInfo : YrkeInfo -> JobbtittelInfo
yrkeInfoTilJobbtittelInfo yrke =
    { tidligereInfo = yrke, jobbtittel = "" }


jobbtittelInfoTilBedriftnavnsInfo : JobbtittelInfo -> BeriftnavnsInfo
jobbtittelInfoTilBedriftnavnsInfo jobbtittelInfo =
    { tidligereInfo = jobbtittelInfo
    , bedriftNavn = ""
    }


bedriftnavnsInfoTilLokasjonInfo : BeriftnavnsInfo -> LokasjonInfo
bedriftnavnsInfoTilLokasjonInfo beriftnavnsInfo =
    { tidligereInfo = beriftnavnsInfo
    , lokasjon = ""
    }


lokasjonInfoTilArbeidsoppgaverInfo : LokasjonInfo -> ArbeidsoppgaverInfo
lokasjonInfoTilArbeidsoppgaverInfo lokasjonInfo =
    { tidligereInfo = lokasjonInfo
    , arbeidsoppgaver = ""
    }


arbeidsoppgaverInfoTilPeriodeInfo : ArbeidsoppgaverInfo -> PeriodeInfo
arbeidsoppgaverInfoTilPeriodeInfo arbeidsoppgaverInfo =
    { tidligereInfo = arbeidsoppgaverInfo
    , fradato = ""
    , tildato = ""
    , naavarende = False
    }


update : Msg -> Model -> SamtaleStatus
update msg (Model info) =
    case msg of
        HentAAregArbeidserfaring ->
            ( Model info, Api.hentAAreg HentetAAregArbeidserfaring )
                |> IkkeFerdig

        HentetAAregArbeidserfaring result ->
            case result of
                Ok arbeidserfaringFraAAreg ->
                    ( info
                        |> visAaregResultat arbeidserfaringFraAAreg
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Err error ->
                    ( IkkeHentetFraAAreg
                        |> nesteSamtaleSteg info (Melding.svar [ "Ja, jeg har arbeidserfaring" ])
                    , logFeilmelding error "Hente fra Aareg"
                    )
                        |> IkkeFerdig

        BrukerOppretterNyArbeidserfaring ->
            ( RegistrerYrke { yrke = "" }
                |> nesteSamtaleSteg info (Melding.svar [ "Legg til arbeidserfaring" ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererYrke string ->
            ( Model
                { info
                    | aktivSamtale = RegistrerYrke { yrke = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerLagrerYrke jobbtittelInfo ->
            ( SpørOmBrukerVilEndreJobbtittel jobbtittelInfo
                |> nesteSamtaleSteg info (Melding.svar [ jobbtittelInfo.tidligereInfo.yrke ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilEndreJobbtittel jobbtittelInfo ->
            ( EndreJobbtittel jobbtittelInfo
                |> nesteSamtaleSteg info (Melding.svar [ "Nei, legg til et nytt navn" ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererJobbtittelFelt jobbtittelInfo string ->
            ( Model
                { info
                    | aktivSamtale = EndreJobbtittel { jobbtittelInfo | jobbtittel = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrereBedriftnavn knappeTekst bedriftnavnInfo ->
            ( RegistrereBedriftNavn bedriftnavnInfo
                |> nesteSamtaleSteg info (Melding.svar [ knappeTekst ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererBedriftnavn beriftnavnsInfo string ->
            ( Model
                { info
                    | aktivSamtale = RegistrereBedriftNavn { beriftnavnsInfo | bedriftNavn = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrereLokasjon lokasjonInfo ->
            ( RegistrereLokasjon lokasjonInfo
                |> nesteSamtaleSteg info (Melding.svar [ lokasjonInfo.tidligereInfo.bedriftNavn ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererLokasjon lokasjonsInfo string ->
            ( Model
                { info
                    | aktivSamtale = RegistrereLokasjon { lokasjonsInfo | lokasjon = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
            ( RegistrereArbeidsoppgaver arbeidsoppgaverInfo
                |> nesteSamtaleSteg info (Melding.svar [ arbeidsoppgaverInfo.tidligereInfo.lokasjon ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererArbeidsoppgaver arbeidsoppgaverInfo string ->
            ( Model
                { info
                    | aktivSamtale =
                        RegistrereArbeidsoppgaver { arbeidsoppgaverInfo | arbeidsoppgaver = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrereFraMåned periodeInfo ->
            ( RegistrereFraMåned periodeInfo
                |> nesteSamtaleSteg info (Melding.svar [ periodeInfo.tidligereInfo.arbeidsoppgaver ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererFradato periodeInfo string ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        BrukerOppdatererTildato periodeInfo string ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        ErrorLogget result ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        BrukerOppdatererNaavarende periodeInfo ->
            ( Model info, Cmd.none )
                |> IkkeFerdig


visAaregResultat : List Arbeidserfaring -> ModelInfo -> Model
visAaregResultat list info =
    if List.isEmpty list then
        RegistrerYrke { yrke = "" }
            |> oppdaterSamtalesteg info

    else
        list
            ++ info.arbeidserfaringListe
            |> VisArbeidserfaringFraAareg
            |> oppdaterSamtalesteg info


leggTilIArbeidserfaring : List Arbeidserfaring -> ModelInfo -> Samtale -> Model
leggTilIArbeidserfaring arbeidserfaringFraAareg modelInfo samtaleSeksjon =
    Model
        { modelInfo
            | aktivSamtale = samtaleSeksjon
            , arbeidserfaringListe = modelInfo.arbeidserfaringListe ++ arbeidserfaringFraAareg
        }


toggleNaavarende : PeriodeInfo -> PeriodeInfo
toggleNaavarende periodeInfo =
    if periodeInfo.naavarende then
        { periodeInfo | naavarende = False }

    else
        { periodeInfo | naavarende = True }


oppdaterSamtalesteg : ModelInfo -> Samtale -> Model
oppdaterSamtalesteg modelInfo samtaleSeksjon =
    case samtaleSeksjon of
        IngenArbeidserfaringFraAareg list ->
            Model
                { modelInfo
                    | aktivSamtale = samtaleSeksjon
                    , arbeidserfaringListe = list
                    , seksjonsMeldingsLogg =
                        modelInfo.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
                }

        VisArbeidserfaringFraAareg list ->
            Model
                { modelInfo
                    | aktivSamtale = samtaleSeksjon
                    , arbeidserfaringListe = list
                    , seksjonsMeldingsLogg =
                        modelInfo.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
                }

        RegistrerYrke _ ->
            Model
                { modelInfo
                    | aktivSamtale = samtaleSeksjon
                    , seksjonsMeldingsLogg =
                        modelInfo.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
                }

        _ ->
            Model modelInfo


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg modelInfo melding samtaleSeksjon =
    Model
        { modelInfo
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                modelInfo.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg personaliaSeksjon =
    case personaliaSeksjon of
        Intro ->
            [ Melding.spørsmål [ "Nå skal vi registrere arbeidserfaringen din" ] ]

        HentetFraAAreg ->
            [ Melding.spørsmål
                [ "Da har vi hentet arbeidserfaringen fra Aa-registeret."
                , "Informasjonen er registrert av arbeidsgiverene dine. "
                ]
            , Melding.spørsmål
                [ "Dessverre finnes det bare informasjon om arbeidsforhold etter 2015."
                , "Jeg håper det kan hjelpe deg litt "
                ]
            ]

        IkkeHentetFraAAreg ->
            [ Melding.spørsmål
                [ "Normalt ville jeg hentet informasjon om arbeidserfaringen dine fra Aa-reg."
                , "Dessverre fikk jeg ikke kontakt med dem nå, så vi må fylle ut arbeidserfaringen selv."
                ]
            , Melding.spørsmål [ "Da må vi registrere arbeidserfaringen selv, men det går heldigvis ganske kjapt!" ]
            , Melding.spørsmål
                [ "Vi begynner med å registrere yrke,"
                , "det er viktig at du velger et yrke fra listen."
                , "Hvis ikke navnet passer helt, så kan du endre det senere."
                ]
            ]

        IngenArbeidserfaringFraAareg liste ->
            [ Melding.spørsmål
                [ "Normalt ville jeg hentet informasjon om arbeidserfaringen dine fra Aa-reg."
                , "Dessverre ser det ut til at du ikke har noen registrerte jobber der."
                ]
            , Melding.spørsmål [ "Da må vi registrere arbeidserfaringen selv, men det går heldigvis ganske kjapt!" ]
            , Melding.spørsmål
                [ "Vi begynner med å registrere yrke,"
                , "det er viktig at du velger et yrke fra listen."
                , "Hvis ikke navnet passer helt, så kan du endre det senere."
                ]
            ]

        VisArbeidserfaringFraAareg liste ->
            [ Melding.spørsmål [ "" ] ]

        RegistrerYrke yrkeInfo ->
            [ Melding.spørsmål
                [ "Nå skal du legge inn en og en av arbeidserfaringene dine. Da setter vi igjang :)"
                ]
            , Melding.spørsmål
                [ "Først må du velge et yrke. Begynn og skriv og velg fra forslagene som kommer opp"
                , "Grunnen til at du må velge et av forslagene er fordi arbeidsgiverene skal kunne finne deg i søket sitt."
                ]
            , Melding.spørsmål
                [ "Hvis du ikke finner yrket ditt, velg det nærmeste. Hvis yrket du velger ikke stemmer helt, kan du endre navnet etterpå"
                ]
            ]

        SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
            [ Melding.spørsmål
                [ "Stemte yrket du la inn, eller ønsker du å gi det et nytt navn?"
                , "Navnet du skriver vil vises på CV-en din"
                ]
            ]

        EndreJobbtittel jobbtittelInfo ->
            []

        RegistrereBedriftNavn beriftnavnsInfo ->
            [ Melding.spørsmål [ "Hva er navnet på bedriften du jobbet i?" ] ]

        RegistrereLokasjon lokasjonInfo ->
            [ Melding.spørsmål [ "Hvor holder bedriften til?" ] ]

        RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
            [ Melding.spørsmål
                [ "Fortell arbeidsgivere hvilke arbeidsoppgaver du har hatt, hva du har lært og hva som var rollen din."
                ]
            ]

        RegistrereFraMåned periodeInfo ->
            [ Melding.spørsmål [ "Hvilken måned begynte du i jobbe?" ] ]

        RegistrereFraÅr periodeInfo ->
            [ Melding.spørsmål [ "" ] ]

        RegistrereNaavarende periodeInfo ->
            [ Melding.spørsmål [ "" ] ]

        RegistrereTilMåned periodeInfo ->
            [ Melding.spørsmål [ "" ] ]

        RegistrereTilÅr periodeInfo ->
            [ Melding.spørsmål [ "" ] ]



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model info) =
    case info.aktivSamtale of
        Intro ->
            div []
                [ button [ onClick HentAAregArbeidserfaring ] [ text "Ja, jeg har arbeidserfaring" ]
                ]

        HentetFraAAreg ->
            div [] []

        IkkeHentetFraAAreg ->
            div []
                [ label [] [ text "Yrke" ]
                , input
                    [ "" |> value
                    , onInput BrukerOppdatererYrke
                    ]
                    []
                , button
                    [ yrkeInfoTilJobbtittelInfo { yrke = "" }
                        |> BrukerLagrerYrke
                        |> onClick
                    ]
                    [ text "Lagre" ]
                ]

        IngenArbeidserfaringFraAareg liste ->
            div [] []

        VisArbeidserfaringFraAareg liste ->
            div [] []

        RegistrerYrke yrke ->
            div []
                [ label [] [ text "Yrke" ]
                , input
                    [ yrke.yrke |> value
                    , onInput BrukerOppdatererYrke
                    ]
                    []
                , button
                    [ yrkeInfoTilJobbtittelInfo yrke
                        |> BrukerLagrerYrke
                        |> onClick
                    ]
                    [ text "Lagre" ]
                ]

        SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
            div []
                [ button
                    [ jobbtittelInfo
                        |> BrukerVilEndreJobbtittel
                        |> onClick
                    ]
                    [ text "Nei, legg til et nytt navn" ]
                , button
                    [ jobbtittelInfoTilBedriftnavnsInfo jobbtittelInfo
                        |> BrukerVilRegistrereBedriftnavn "Ja, det stemte"
                        |> onClick
                    ]
                    [ text "Ja, det stemte" ]
                ]

        EndreJobbtittel jobbtittelInfo ->
            div []
                [ label [] [ text "Skriv inn navnet på bedriften du jobbet i" ]
                , input
                    [ jobbtittelInfo.jobbtittel |> value
                    , onInput (BrukerOppdatererJobbtittelFelt jobbtittelInfo)
                    ]
                    []
                , button
                    [ jobbtittelInfoTilBedriftnavnsInfo jobbtittelInfo
                        |> BrukerVilRegistrereBedriftnavn "Gå videre"
                        |> onClick
                    ]
                    [ text "Gå videre" ]
                ]

        RegistrereBedriftNavn beriftnavnsInfo ->
            div []
                [ label [] [ text "Skriv inn navnet på bedriften du jobbet i" ]
                , input
                    [ beriftnavnsInfo.bedriftNavn |> value
                    , onInput (BrukerOppdatererBedriftnavn beriftnavnsInfo)
                    ]
                    []
                , button
                    [ bedriftnavnsInfoTilLokasjonInfo beriftnavnsInfo
                        |> BrukerVilRegistrereLokasjon
                        |> onClick
                    ]
                    [ text "Gå videre" ]
                ]

        RegistrereLokasjon lokasjonInfo ->
            div []
                [ label [] [ text "Sted/land" ]
                , input
                    [ lokasjonInfo.lokasjon |> value
                    , lokasjonInfo
                        |> BrukerOppdatererLokasjon
                        |> onInput
                    ]
                    []
                , button
                    [ lokasjonInfoTilArbeidsoppgaverInfo lokasjonInfo
                        |> BrukerVilRegistrereArbeidsoppgaver
                        |> onClick
                    ]
                    [ text "Lagre" ]
                ]

        RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
            div []
                [ label [] [ text "Utfyllende text" ]
                , input
                    [ arbeidsoppgaverInfo.arbeidsoppgaver |> value
                    , arbeidsoppgaverInfo
                        |> BrukerOppdatererArbeidsoppgaver
                        |> onInput
                    ]
                    []
                , button
                    [ arbeidsoppgaverInfoTilPeriodeInfo arbeidsoppgaverInfo
                        |> BrukerVilRegistrereFraMåned
                        |> onClick
                    ]
                    [ text "Lagre" ]
                ]

        RegistrereFraMåned periodeInfo ->
            div []
                [ lagMånedKnapp Dato.Januar periodeInfo
                , lagMånedKnapp Dato.Februar periodeInfo
                , lagMånedKnapp Dato.Mars periodeInfo
                , div []
                    [ lagMånedKnapp Dato.April periodeInfo
                    , lagMånedKnapp Dato.Mai periodeInfo
                    , lagMånedKnapp Dato.Juni periodeInfo
                    , div []
                        [ lagMånedKnapp Dato.Juli periodeInfo
                        , lagMånedKnapp Dato.August periodeInfo
                        , lagMånedKnapp Dato.September periodeInfo
                        , div []
                            [ lagMånedKnapp Dato.Oktober periodeInfo
                            , lagMånedKnapp Dato.November periodeInfo
                            , lagMånedKnapp Dato.Desember periodeInfo
                            ]
                        ]
                    ]
                ]

        RegistrereFraÅr periodeInfo ->
            div [] []

        RegistrereNaavarende periodeInfo ->
            div [] []

        RegistrereTilMåned periodeInfo ->
            div [] []

        RegistrereTilÅr periodeInfo ->
            div [] []


lagMånedKnapp : Dato.Måned -> PeriodeInfo -> Html Msg
lagMånedKnapp måned periodeInfo =
    case måned of
        Dato.Januar ->
            button
                []
                [ text "Januar" ]

        Dato.Februar ->
            button [] [ text "Februar" ]

        Dato.Mars ->
            button [] [ text "Mars" ]

        Dato.April ->
            button [] [ text "April" ]

        Dato.Mai ->
            button [] [ text "Mai" ]

        Dato.Juni ->
            button [] [ text "Juni" ]

        Dato.Juli ->
            button [] [ text "Juli" ]

        Dato.August ->
            button [] [ text "August" ]

        Dato.September ->
            button [] [ text "September" ]

        Dato.Oktober ->
            button [] [ text "Oktober" ]

        Dato.November ->
            button [] [ text "November" ]

        Dato.Desember ->
            button [] [ text "Desember" ]


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError ErrorLogget)
        |> Maybe.withDefault Cmd.none


init : MeldingsLogg -> Model
init gammelMeldingsLogg =
    Model
        { seksjonsMeldingsLogg =
            gammelMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Har du arbeidserfaring du vil legge inn?" ] ]
        , arbeidserfaringListe = []
        , aktivSamtale = Intro
        }
