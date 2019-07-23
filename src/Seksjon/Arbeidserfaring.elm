module Seksjon.Arbeidserfaring exposing (FraDatoInfo, Model, Msg, Oppsummering, SamtaleStatus(..), TilDatoInfo, init, meldingsLogg, update, viewBrukerInput)

import Api
import Cv.Arbeidserfaring exposing (Arbeidserfaring)
import Dato exposing (Dato)
import Feilmelding
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import Html exposing (Attribute, Html, button, div, input, label, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Melding exposing (Melding)
import MeldingsLogg exposing (MeldingsLogg)
import Skjema.ArbeidserfaringSkjema as ArbeidserfaringSkjema exposing (ArbeidserfaringSkjema)



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
    | BrukerVilRegistrereFraMåned FraDatoInfo
    | BrukerVilRegistrereFraÅr FraDatoInfo
    | BrukerOppdatererFraÅr FraDatoInfo String
    | BrukerVilRegistrereNaavarende FraDatoInfo
    | BrukerSvarerJaTilNaavarende TilDatoInfo
    | BrukerSvarerNeiTilNaavarende TilDatoInfo
    | BrukerVilRegistrereTilÅr TilDatoInfo
    | BrukerOppdatererTilÅr TilDatoInfo String
    | BrukerVilGåTilOppsummering TilDatoInfo
    | BrukerVilRedigereOppsummering
    | ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.Felt String
    | ErrorLogget (Result Http.Error ())


type Samtale
    = Intro
    | HenterFraAareg
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
    | RegistrereFraMåned FraDatoInfo
    | RegistrereFraÅr FraDatoInfo
    | RegistrereNaavarende FraDatoInfo
    | RegistrereTilMåned TilDatoInfo
    | RegistrereTilÅr TilDatoInfo
    | VisOppsummering Oppsummering
    | RedigerOppsummering ArbeidserfaringSkjema.ArbeidserfaringSkjema


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


type alias FraDatoInfo =
    { tidligereInfo : ArbeidsoppgaverInfo
    , fraMåned : Dato.Måned
    , fraÅr : String
    , naavarende : Bool
    }


type alias TilDatoInfo =
    { tidligereInfo : FraDatoInfo
    , tilMåned : Dato.Måned
    , tilÅr : String
    }


type alias Oppsummering =
    { yrke : String
    , jobbTittel : String
    , bedriftNavn : String
    , lokasjon : String
    , arbeidsoppgaver : String
    , fraDato : Dato
    , naavarende : Bool
    , tilDato : Maybe Dato
    }


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


arbeidsoppgaverInfoTilfraDatoInfo : ArbeidsoppgaverInfo -> FraDatoInfo
arbeidsoppgaverInfoTilfraDatoInfo arbeidsoppgaverInfo =
    { tidligereInfo = arbeidsoppgaverInfo
    , fraMåned = Dato.Januar
    , fraÅr = "1990"
    , naavarende = False
    }


fraDatoInfoTilTilDatoInfo : FraDatoInfo -> TilDatoInfo
fraDatoInfoTilTilDatoInfo fraDatoInfo =
    { tidligereInfo = fraDatoInfo
    , tilMåned = Dato.Januar
    , tilÅr = ""
    }


tilDatoTilOppsummering : TilDatoInfo -> Oppsummering
tilDatoTilOppsummering tilDatoInfo =
    { yrke = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.yrke
    , jobbTittel = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.jobbtittel
    , bedriftNavn = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.tidligereInfo.bedriftNavn
    , lokasjon = tilDatoInfo.tidligereInfo.tidligereInfo.tidligereInfo.lokasjon
    , arbeidsoppgaver = tilDatoInfo.tidligereInfo.tidligereInfo.arbeidsoppgaver
    , fraDato = Dato.tilDato (tilDatoInfo.tidligereInfo.fraÅr ++ "-" ++ (tilDatoInfo.tidligereInfo.fraMåned |> Dato.månedTilString))
    , naavarende = tilDatoInfo.tidligereInfo.naavarende
    , tilDato =
        if tilDatoInfo.tidligereInfo.naavarende then
            Just (Dato.tilDato "1999-09-09")

        else
            Just (Dato.tilDato (tilDatoInfo.tilÅr ++ "-" ++ (tilDatoInfo.tilMåned |> Dato.månedTilString)))
    }


oppsummeringTilSkjema : Oppsummering -> ArbeidserfaringSkjema.ArbeidserfaringSkjema
oppsummeringTilSkjema oppsummering =
    ArbeidserfaringSkjema.ArbeidserfaringSkjema
        { yrke = oppsummering.yrke
        , jobbTittel = oppsummering.jobbTittel
        , bedriftNavn = oppsummering.bedriftNavn
        , lokasjon = oppsummering.lokasjon
        , arbeidsoppgaver = oppsummering.arbeidsoppgaver
        , fraDato = oppsummering.fraDato
        , naavarende = oppsummering.naavarende
        , tilDato = oppsummering.tilDato
        }


update : Msg -> Model -> SamtaleStatus
update msg (Model info) =
    case msg of
        HentAAregArbeidserfaring ->
            ( Model
                { info
                    | seksjonsMeldingsLogg =
                        info.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (Melding.svar [ "Ja, jeg har arbeidserfaring" ])
                }
            , Api.hentAAreg HentetAAregArbeidserfaring
            )
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

        BrukerVilRegistrereFraMåned fraDatoInfo ->
            ( RegistrereFraMåned fraDatoInfo
                |> nesteSamtaleSteg info (Melding.svar [ fraDatoInfo.tidligereInfo.arbeidsoppgaver ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrereFraÅr fraDatoInfo ->
            ( RegistrereFraÅr fraDatoInfo
                |> nesteSamtaleSteg info
                    (Melding.svar
                        [ fraDatoInfo.fraMåned
                            |> Dato.månedTilString
                        ]
                    )
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererFraÅr fraDatoInfo string ->
            ( Model
                { info
                    | aktivSamtale =
                        RegistrereFraÅr { fraDatoInfo | fraÅr = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrereNaavarende fraDatoInfo ->
            ( RegistrereNaavarende fraDatoInfo
                |> nesteSamtaleSteg info (Melding.svar [ fraDatoInfo.fraÅr ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerSvarerJaTilNaavarende tilDatoInfo ->
            ( tilDatoInfo
                |> tilDatoTilOppsummering
                |> VisOppsummering
                |> nesteSamtaleSteg info (Melding.svar [ "Ja" ])
                |> setNaavarendeTilTrue
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerSvarerNeiTilNaavarende tilDatoInfo ->
            ( RegistrereTilMåned tilDatoInfo
                |> nesteSamtaleSteg info (Melding.svar [ "Nei" ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRegistrereTilÅr tilDatoInfo ->
            ( RegistrereTilÅr tilDatoInfo
                |> nesteSamtaleSteg info
                    (Melding.svar
                        [ tilDatoInfo.tilMåned
                            |> Dato.månedTilString
                        ]
                    )
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerOppdatererTilÅr tilDatoInfo string ->
            ( Model
                { info
                    | aktivSamtale =
                        RegistrereTilÅr { tilDatoInfo | tilÅr = string }
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilGåTilOppsummering tilDatoInfo ->
            ( tilDatoInfo
                |> tilDatoTilOppsummering
                |> VisOppsummering
                |> nesteSamtaleSteg info (Melding.svar [ tilDatoInfo.tilÅr ])
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilRedigereOppsummering ->
            case info.aktivSamtale of
                VisOppsummering oppsummering ->
                    ( oppsummering
                        |> oppsummeringTilSkjema
                        |> RedigerOppsummering
                        |> nesteSamtaleSteg info
                            (Melding.svar [ "Nei, jeg vil endre" ])
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        ArbeidserfaringStringSkjemaEndret felt string ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        ErrorLogget result ->
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


setNaavarendeTilTrue : Model -> Model
setNaavarendeTilTrue (Model info) =
    case info.aktivSamtale of
        RegistrereNaavarende fraDatoInfo ->
            Model { info | aktivSamtale = RegistrereNaavarende { fraDatoInfo | naavarende = True } }

        _ ->
            Model info


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg personaliaSeksjon =
    case personaliaSeksjon of
        Intro ->
            [ Melding.spørsmål [ "Nå skal vi registrere arbeidserfaringen din" ] ]

        HenterFraAareg ->
            [ Melding.spørsmål [ "bla bla bla" ] ]

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
            [ Melding.spørsmål [ "Hvilket år begynte du i jobben?" ] ]

        RegistrereNaavarende periodeInfo ->
            [ Melding.spørsmål [ "Jobber du fremdeles her?" ] ]

        RegistrereTilMåned periodeInfo ->
            [ Melding.spørsmål [ "Hvilken måned sluttet du i jobben?" ] ]

        RegistrereTilÅr periodeInfo ->
            [ Melding.spørsmål [ "Hvilket år sluttet du i jobben?" ] ]

        VisOppsummering oppsummering ->
            [ Melding.spørsmål
                [ "Er informasjonen du la inn riktig?"
                , "Stilling/Yrke: " ++ hentStilling oppsummering
                , "Bedriftnanv: " ++ oppsummering.bedriftNavn
                , "Sted: " ++ oppsummering.lokasjon
                , "Arbeidsoppgaver: " ++ oppsummering.arbeidsoppgaver
                , "Fra: " ++ hentFraDato oppsummering
                , if oppsummering.naavarende then
                    ""

                  else
                    hentTilDato oppsummering
                ]
            ]

        RedigerOppsummering skjema ->
            []


hentStilling : Oppsummering -> String
hentStilling oppsummering =
    if oppsummering.jobbTittel == "" then
        oppsummering.yrke

    else
        oppsummering.jobbTittel


hentFraDato : Oppsummering -> String
hentFraDato oppsummering =
    let
        år =
            oppsummering.fraDato |> Dato.år |> String.fromInt

        maaned =
            oppsummering.fraDato |> Dato.måned |> Dato.månedTilString
    in
    maaned
        ++ " "
        ++ år


hentTilDato : Oppsummering -> String
hentTilDato oppsummering =
    let
        dato =
            oppsummering.tilDato
                |> Maybe.withDefault oppsummering.fraDato
    in
    "Til: " ++ (dato |> Dato.måned |> Dato.månedTilString) ++ " " ++ (dato |> Dato.år |> String.fromInt)



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model info) =
    case info.aktivSamtale of
        Intro ->
            div [ class "inputrad" ]
                [ Knapp.knapp HentAAregArbeidserfaring "Ja, jeg har arbeidserfaring"
                    |> Knapp.toHtml
                ]

        HenterFraAareg ->
            div [] []

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
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ yrke.yrke
                        |> Input.input { label = "Yrke", msg = BrukerOppdatererYrke }
                        |> Input.toHtml
                    , div [ class "inputrad" ]
                        [ yrke
                            |> yrkeInfoTilJobbtittelInfo
                            |> BrukerLagrerYrke
                            |> lagTekstInputKnapp "Gå videre" yrke.yrke
                        ]
                    ]
                ]

        SpørOmBrukerVilEndreJobbtittel jobbtittelInfo ->
            div [ class "skjmawrapper" ]
                [ div [ class "inputrad" ]
                    [ (jobbtittelInfo
                        |> BrukerVilEndreJobbtittel
                        |> Knapp.knapp
                      )
                        "Nei, jeg vil endre tittel"
                        |> Knapp.toHtml
                    , (jobbtittelInfo
                        |> jobbtittelInfoTilBedriftnavnsInfo
                        |> BrukerVilRegistrereBedriftnavn "Ja, det stemmer"
                        |> Knapp.knapp
                      )
                        "Ja, det stemmer"
                        |> Knapp.toHtml
                    ]
                ]

        EndreJobbtittel jobbtittelInfo ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ jobbtittelInfo.jobbtittel
                        |> Input.input
                            { label = "Stilling/yrke som vil vises i CV", msg = BrukerOppdatererJobbtittelFelt jobbtittelInfo }
                        |> Input.toHtml
                    ]
                , div [ class "inputrad" ]
                    [ jobbtittelInfo
                        |> jobbtittelInfoTilBedriftnavnsInfo
                        |> BrukerVilRegistrereBedriftnavn "Gå videre"
                        |> lagTekstInputKnapp "Gå videre" jobbtittelInfo.jobbtittel
                    ]
                ]

        RegistrereBedriftNavn bedriftnanvsInfo ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ bedriftnanvsInfo.bedriftNavn
                        |> Input.input { label = "Hva er navnet på bedriften du jobbet i?", msg = BrukerOppdatererBedriftnavn bedriftnanvsInfo }
                        |> Input.toHtml
                    ]
                , div [ class "inputrad" ]
                    [ bedriftnanvsInfo
                        |> bedriftnavnsInfoTilLokasjonInfo
                        |> BrukerVilRegistrereLokasjon
                        |> lagTekstInputKnapp "Gå videre" bedriftnanvsInfo.bedriftNavn
                    ]
                ]

        RegistrereLokasjon lokasjonInfo ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ lokasjonInfo.lokasjon
                        |> Input.input { label = "Sted/land", msg = BrukerOppdatererLokasjon lokasjonInfo }
                        |> Input.toHtml
                    ]
                , div [ class "inputrad" ]
                    [ lokasjonInfo
                        |> lokasjonInfoTilArbeidsoppgaverInfo
                        |> BrukerVilRegistrereArbeidsoppgaver
                        |> lagTekstInputKnapp "Gå videre" lokasjonInfo.lokasjon
                    ]
                ]

        RegistrereArbeidsoppgaver arbeidsoppgaverInfo ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ arbeidsoppgaverInfo.arbeidsoppgaver
                        |> Input.input { label = "Hvilke arbeidsoppgaver gjorde du?", msg = BrukerOppdatererArbeidsoppgaver arbeidsoppgaverInfo }
                        |> Input.toHtml
                    ]
                , div [ class "inputrad" ]
                    [ arbeidsoppgaverInfoTilfraDatoInfo arbeidsoppgaverInfo
                        |> BrukerVilRegistrereFraMåned
                        |> lagTekstInputKnapp "Gå videre" arbeidsoppgaverInfo.arbeidsoppgaver
                    ]
                ]

        RegistrereFraMåned fraDatoInfo ->
            div [ class "skjema-wrapper" ]
                [ div [ class "inputrad" ]
                    [ Dato.Januar
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.Februar
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.Mars
                        |> lagFraMånedKnapp fraDatoInfo
                    ]
                , div [ class "inputrad" ]
                    [ Dato.April
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.Mai
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.Juni
                        |> lagFraMånedKnapp fraDatoInfo
                    ]
                , div [ class "inputrad" ]
                    [ Dato.Juli
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.August
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.September
                        |> lagFraMånedKnapp fraDatoInfo
                    ]
                , div [ class "inputrad" ]
                    [ Dato.Oktober
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.November
                        |> lagFraMånedKnapp fraDatoInfo
                    , Dato.Desember
                        |> lagFraMånedKnapp fraDatoInfo
                    ]
                ]

        RegistrereFraÅr fraDatoInfo ->
            div [ class "skjemawrapper" ]
                [ div [ class "skjema" ]
                    [ fraDatoInfo.fraÅr
                        |> Input.input { label = "Hvilket år begynte du der?", msg = BrukerOppdatererFraÅr fraDatoInfo }
                        |> Input.toHtml
                    ]
                , div [ class "inputrad" ]
                    [ fraDatoInfo
                        |> BrukerVilRegistrereNaavarende
                        |> lagÅrInputKnapp "Gå videre" fraDatoInfo.fraÅr
                    ]
                ]

        RegistrereNaavarende fraDatoInfo ->
            div []
                [ div [ class "inputrad" ]
                    [ fraDatoInfo
                        |> fraDatoInfoTilTilDatoInfo
                        |> BrukerSvarerJaTilNaavarende
                        |> lagMessageKnapp "Ja"
                    , fraDatoInfo
                        |> fraDatoInfoTilTilDatoInfo
                        |> BrukerSvarerNeiTilNaavarende
                        |> lagMessageKnapp "Nei"
                    ]
                ]

        RegistrereTilMåned tilDatoInfo ->
            div [ class "inputrad" ]
                [ Dato.Januar
                    |> lagTilMånedKnapp tilDatoInfo
                , Dato.Februar
                    |> lagTilMånedKnapp tilDatoInfo
                , Dato.Mars
                    |> lagTilMånedKnapp tilDatoInfo
                , div []
                    [ Dato.April
                        |> lagTilMånedKnapp tilDatoInfo
                    , Dato.Mai
                        |> lagTilMånedKnapp tilDatoInfo
                    , Dato.Juni
                        |> lagTilMånedKnapp tilDatoInfo
                    , div []
                        [ Dato.Juli
                            |> lagTilMånedKnapp tilDatoInfo
                        , Dato.August
                            |> lagTilMånedKnapp tilDatoInfo
                        , Dato.September
                            |> lagTilMånedKnapp tilDatoInfo
                        , div []
                            [ Dato.Oktober
                                |> lagTilMånedKnapp tilDatoInfo
                            , Dato.November
                                |> lagTilMånedKnapp tilDatoInfo
                            , Dato.Desember
                                |> lagTilMånedKnapp tilDatoInfo
                            ]
                        ]
                    ]
                ]

        RegistrereTilÅr tilDatoInfo ->
            div []
                [ label [] [ text "Skriv inn år" ]
                , input
                    [ tilDatoInfo.tilÅr |> value
                    , tilDatoInfo
                        |> BrukerOppdatererTilÅr
                        |> onInput
                    ]
                    []
                , button
                    [ tilDatoInfo
                        |> BrukerVilGåTilOppsummering
                        |> onClick
                    ]
                    [ text "Gå videre" ]
                ]

        VisOppsummering oppsummering ->
            div []
                [ button [] [ text "Ja, informasjonen er riktig" ]
                , BrukerVilRedigereOppsummering
                    |> lagMessageKnapp "Nei, jeg vil endre"
                ]

        RedigerOppsummering skjema ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    [ skjema
                        |> ArbeidserfaringSkjema.yrke
                        |> Input.input { label = "Yrke", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.Yrke }
                        |> Input.toHtml
                    , if
                        ArbeidserfaringSkjema.jobbTittel skjema
                            == ""
                      then
                        div [] []

                      else
                        skjema
                            |> ArbeidserfaringSkjema.jobbTittel
                            |> Input.input { label = "Jobbtittel", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.JobbTittel }
                            |> Input.toHtml
                    , skjema
                        |> ArbeidserfaringSkjema.bedriftNavn
                        |> Input.input { label = "Bedriftnavn", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.BedriftNavn }
                        |> Input.toHtml
                    , skjema
                        |> ArbeidserfaringSkjema.lokasjon
                        |> Input.input { label = "Sted", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.Lokasjon }
                        |> Input.toHtml
                    , skjema
                        |> ArbeidserfaringSkjema.arbeidsoppgaver
                        |> Input.input { label = "Arbeidsoppgaver", msg = ArbeidserfaringStringSkjemaEndret ArbeidserfaringSkjema.Arbeidsoppgaver }
                        |> Input.toHtml
                    ]
                ]


lagTekstInputKnapp : String -> String -> Msg -> Html Msg
lagTekstInputKnapp knappeTekst inputTekst msg =
    Knapp.knapp msg knappeTekst
        |> (if inputTekst /= "" then
                Knapp.withEnabled Knapp.Enabled

            else
                Knapp.withEnabled Knapp.Disabled
           )
        |> Knapp.toHtml


lagÅrInputKnapp : String -> String -> Msg -> Html Msg
lagÅrInputKnapp knappeTekst inputTekst msg =
    Knapp.knapp msg knappeTekst
        |> (if inputTekst /= "" && Dato.validerÅr inputTekst then
                Knapp.withEnabled Knapp.Enabled

            else
                Knapp.withEnabled Knapp.Disabled
           )
        |> Knapp.toHtml


lagMessageKnapp : String -> Msg -> Html Msg
lagMessageKnapp knappeTekst msg =
    Knapp.knapp msg knappeTekst
        |> Knapp.toHtml


lagFraMånedKnapp : FraDatoInfo -> Dato.Måned -> Html Msg
lagFraMånedKnapp fraDatoInfo måned =
    let
        msg =
            { fraDatoInfo | fraMåned = måned }
                |> BrukerVilRegistrereFraÅr
    in
    måned
        |> Dato.månedTilString
        |> Knapp.knapp msg
        |> Knapp.withClass Knapp.MånedKnapp
        |> Knapp.toHtml


lagTilMånedKnapp : TilDatoInfo -> Dato.Måned -> Html Msg
lagTilMånedKnapp tilDatoInfo måned =
    button
        [ { tilDatoInfo | tilMåned = måned }
            |> BrukerVilRegistrereTilÅr
            |> onClick
        ]
        [ måned
            |> Dato.månedTilString
            |> text
        ]


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
